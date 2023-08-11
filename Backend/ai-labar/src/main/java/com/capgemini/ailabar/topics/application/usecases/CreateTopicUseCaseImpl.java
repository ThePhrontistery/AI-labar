package com.capgemini.ailabar.topics.application.usecases;

import com.capgemini.ailabar.commons.utils.Constants;
import com.capgemini.ailabar.commons.utils.MailService;
import com.capgemini.ailabar.groups.domain.exceptions.CreateGroupException;
import com.capgemini.ailabar.options.domain.models.OptionsModel;
import com.capgemini.ailabar.topics.domain.exceptions.CreateTopicException;
import com.capgemini.ailabar.topics.domain.models.TopicsModel;
import com.capgemini.ailabar.topics.domain.ports.in.CreateTopicUseCase;
import com.capgemini.ailabar.topics.domain.ports.out.TopicsRepositoryPort;
import com.capgemini.ailabar.topics.infraestructure.utils.TopicsUtils;
import com.capgemini.ailabar.topics.infraestructure.entities.TopicsEntity;
import org.springframework.context.annotation.PropertySource;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.List;

@Service
@Transactional
@PropertySource("classpath:application.properties")
public class CreateTopicUseCaseImpl implements CreateTopicUseCase {
    private final TopicsRepositoryPort topicsRepositoryPort;
    private final Environment environment;
    private final MailService mailService;
    private TopicsEntity topicsEntity;
    private TopicsModel topicsModel;

    public CreateTopicUseCaseImpl(TopicsRepositoryPort topicsRepositoryPort, Environment environment,
                                  MailService mailService) {
        this.topicsRepositoryPort = topicsRepositoryPort;
        this.environment = environment;
        this.mailService = mailService;
    }

    /* Se deberá cambiar la gestión de las opciones, members y votedBy */
    @Override
    public void createTopic(TopicsModel topicsModelBody) {
        this.topicsModel = topicsModelBody;

        checkValues();

        checkAuthorization();

        checkTitle();

        topicsModel.setTitle(topicsModel.getTitle().strip());
        topicsModel.setAuthor(topicsModel.getUser().strip());

        checkType();

        topicsModel.setType(topicsModel.getType());

        validateOptions();

        topicsModel.setVisits(topicsModel.getVisits() != null ? topicsModel.getVisits() : 0);
        topicsModel.setStatus(topicsModel.getStatus() != null ? topicsModel.getStatus() : 1);

        topicsEntity = new TopicsEntity(topicsModel);

        manageMembers();

        checkGroup();

        manageCloseDate();

        checkMailService();

        topicsRepositoryPort.createTopic(topicsEntity);

        manageOptions(topicsRepositoryPort.getTopicIdByTopicName(topicsEntity.getTitle()));
    }

    private void checkValues() {
        if(topicsModel.getTitle().isBlank() || topicsModel.getType().isBlank()
                || topicsModel.getQuestion().isBlank() || topicsModel.getOptions().isEmpty()
                || topicsModel.getUser().isBlank() || (topicsModel.getGroupName() == null && (topicsModel.getMembers() == null || topicsModel.getMembers().isEmpty()))) {
            throw new CreateTopicException("All data is required to edit a topic");
        }

        if(topicsModel.getGroupName() != null && topicsModel.getMembers() != null) {
            throw new CreateTopicException("You cannot send both a group name and a list of members, choose one of the two");
        }
    }

    private void checkAuthorization() {
        if(Boolean.FALSE.equals(topicsRepositoryPort.checkAuthorization(topicsModel.getUser(), topicsModel.getToken()))) {
            throw new CreateTopicException("Unauthorized user");
        }
    }

    private void checkTitle() {
        if(Boolean.TRUE.equals(topicsRepositoryPort.checkByTitleAndAuthor(topicsModel.getTitle().strip(), topicsModel.getUser()))) {
            throw new CreateTopicException("There is already a topic assigned to the author with that name");
        }
    }

    private void checkType() {
        if(TopicsUtils.checkTopicType(topicsModel.getType()).equals("KO")) {
            throw new CreateTopicException("The topic type is not valid");
        }

        if(topicsModel.getType().equals("AS")) {
            for (OptionsModel option : topicsModel.getOptions()) {
                if (!topicsRepositoryPort.checkMember(option.getOption())) {
                    throw new CreateTopicException("The user " + option.getOption() + " is not valid");
                }
            }
        }
    }

    private void validateOptions() {
        if ((topicsModel.getType().equals(Constants.TopicType.IMAGE_SINGLE.toString()) || topicsModel.getType().equals(Constants.TopicType.IMAGE_MULTIPLE.toString()))
                && TopicsUtils.validateOptionsDataList(topicsModel.getOptions())) {
            throw new CreateTopicException("It is mandatory to send the images and options for this type of topic");
        }
    }

    private void checkGroup() {
        if(topicsModel.getGroupName() != null && !topicsRepositoryPort.checkIfGroupExists(topicsModel.getGroupName())) {
            throw new CreateTopicException("The group does not exist");
        }
    }

    private void manageMembers() {
        if(topicsModel.getMembers() != null && topicsModel.getGroupName() == null) {
            topicsModel.getMembers().forEach(member -> {
                if (!topicsRepositoryPort.checkMember(member)) {
                    throw new CreateTopicException("The member "+ member +" is not a valid user");
                }
            });

            topicsRepositoryPort.createTemporalGroup("*temp* " + topicsEntity.getTitle(), topicsEntity.getAuthor());
            Integer groupId = topicsRepositoryPort.getGroupIdByGroupNameAndAdmin("*temp* " + topicsEntity.getTitle(), topicsEntity.getAuthor());

            topicsModel.getMembers().forEach(member -> {
                try {
                    if(!topicsEntity.getAuthor().equals(member)) {
                        topicsRepositoryPort.insertMember(groupId, topicsRepositoryPort.getUserIdByUserName(member));
                    }
                } catch (CreateTopicException createGroupException) {
                    topicsRepositoryPort.deleteMembersByGroupId(groupId);
                    topicsRepositoryPort.deleteGroup(groupId);
                    throw new CreateTopicException("An error occurred during the registration of group members (Group deleted)");
                }
            });

            topicsEntity.setGroupId(groupId);
        } else {
            topicsEntity.setGroupId(topicsRepositoryPort.getGroupIdByGroupNameAndAdmin(topicsModel.getGroupName(), topicsModel.getUser()));

            if(topicsEntity.getGroupId() == null) {
                throw new CreateTopicException("The user does not have any group with the indicated name");
            }
        }
    }

    private void manageOptions(Integer topicId) {
        List<OptionsModel> optionsDataList = TopicsUtils.initiateVoting(topicsModel.getType(), topicsModel.getOptions());
        optionsDataList.forEach(option -> {
            try {
                if(option.getImage() == null || option.getImage().isBlank()) {
                    topicsRepositoryPort.insertOption(topicId, option.getOption(), option.getVotes());
                } else {
                    topicsRepositoryPort.insertOption(topicId, option.getImage(), option.getOption(), option.getVotes());
                }
            } catch (CreateTopicException createGroupException) {
                if(topicsEntity.getTitle().startsWith("*temp*")) {
                    topicsRepositoryPort.deleteMembersByGroupId(topicsEntity.getGroupId());
                    topicsRepositoryPort.deleteGroup(topicsEntity.getGroupId());
                }
                topicsRepositoryPort.deleteOptions(topicId);
                topicsRepositoryPort.deleteTopic(topicId);
                throw new CreateTopicException("An error occurred during the registration of the options");
            }
        });
    }

    private void manageCloseDate() {
        if(topicsModel.getCloseDate() != null && !topicsModel.getCloseDate().isBlank()) {
            String dateString = TopicsUtils.validateFormatDate(topicsModel.getCloseDate());

            if(dateString.contains("KO")) {
                throw new CreateTopicException(dateString);
            } else {
                topicsEntity.setCloseDate(topicsModel.getCloseDate());
            }

            try {
                LocalDate inputDate = LocalDate.parse(dateString, DateTimeFormatter.ofPattern("yyyyMMdd"));
                LocalDate currentDate = LocalDate.now();

                if (!inputDate.isAfter(currentDate)) {
                    throw new CreateTopicException("The closing date cannot be earlier than the current date");
                }
            } catch (DateTimeParseException dateTimeParseException) {
                throw new CreateTopicException(dateTimeParseException.getMessage());
            }
        }
    }

    private void checkMailService() {
        if(!"false".equals(environment.getProperty("activate.mail"))) {
            mailService.sendEmail(topicsModel);
        }
    }
}
