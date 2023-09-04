package com.capgemini.ailabar.topics.application.usecases;

import com.capgemini.ailabar.commons.utils.Constants;
import com.capgemini.ailabar.commons.utils.MailService;
import com.capgemini.ailabar.options.domain.models.OptionsModel;
import com.capgemini.ailabar.topics.domain.exceptions.EditTopicException;
import com.capgemini.ailabar.topics.domain.models.TopicsModel;
import com.capgemini.ailabar.topics.domain.ports.in.EditTopicUseCase;
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
public class EditTopicUseCaseImpl implements EditTopicUseCase {
    private final TopicsRepositoryPort topicsRepositoryPort;
    private final Environment environment;
    private final MailService mailService;
    private TopicsEntity topicsEntity;
    private TopicsModel topicsModel;

    public EditTopicUseCaseImpl(TopicsRepositoryPort topicsRepositoryPort, Environment environment,
                                MailService mailService) {
        this.topicsRepositoryPort = topicsRepositoryPort;
        this.environment = environment;
        this.mailService = mailService;
    }

    @Override
    public void editTopic(TopicsModel topicsModelBody) {
        this.topicsModel = topicsModelBody;

        checkValues();

        checkAuthorization();

        checkId();

        topicsEntity = topicsRepositoryPort.getTopicsEntityById(topicsModel.getId());

        checkAuthor();

        checkStatus();

        checkTitle();

        checkType();

        validateOptions();

        topicsEntity.setQuestion(topicsModel.getQuestion());

        manageOptions();

        topicsEntity.setType(topicsModel.getType());

        topicsEntity.setAuthor(topicsModel.getUser());

        String groupName = topicsRepositoryPort.getGroupNameByGroupId(topicsEntity.getGroupId());

        manageMembers(groupName);

        checkGroup(groupName);

        topicsEntity.setGroupId(topicsRepositoryPort.getGroupIdByGroupNameAndAdmin(topicsModel.getGroupName(), topicsModel.getUser()));

        checkCloseDate();

        topicsEntity.setVisits(topicsModel.getVisits() != null ? topicsModel.getVisits() : 0);
        topicsEntity.setStatus(topicsModel.getStatus() != null ? topicsModel.getStatus() : 1);

        manageCloseDate();

        manageMailService();

        topicsRepositoryPort.editTopic(topicsEntity);
    }

    private void checkValues() {
        if(topicsModel.getId() == null || topicsModel.getTitle().isBlank()
                || topicsModel.getType().isBlank() || topicsModel.getQuestion().isBlank()
                || topicsModel.getOptions().isEmpty() || topicsModel.getUser().isBlank()
                || (topicsModel.getGroupName() == null && (topicsModel.getMembers() == null || topicsModel.getMembers().isEmpty()))) {
            throw new EditTopicException("All data is required to edit a topic");
        }

        if(topicsModel.getGroupName() != null && topicsModel.getMembers() != null) {
            throw new EditTopicException("You cannot send both a group name and a list of members, choose one of the two");
        }
    }

    private void checkAuthorization() {
        if(Boolean.FALSE.equals(topicsRepositoryPort.checkAuthorization(topicsModel.getUser(), topicsModel.getToken()))) {
            throw new EditTopicException("Unauthorized user");
        }
    }

    private void checkId() {
        if(Boolean.FALSE.equals(topicsRepositoryPort.checkId(topicsModel.getId()))) {
            throw new EditTopicException("There is no topic with that id");
        }
    }

    private void checkAuthor() {
        if (!topicsModel.getUser().equals(topicsEntity.getAuthor())) {
            throw new EditTopicException("The user is not the author of the topic");
        }
    }

    private void checkStatus() {
        if(topicsEntity.getStatus().equals(0)) {
            throw new EditTopicException("The topic is closed");
        }
    }

    private void checkTitle() {
        if(!topicsModel.getTitle().equals(topicsEntity.getTitle()) && Boolean.TRUE.equals(topicsRepositoryPort.checkByTitleAndAuthor(topicsModel.getTitle().strip(), topicsModel.getUser()))) {
            throw new EditTopicException("There is already a topic assigned to the author with that name");
        } else {
            topicsEntity.setTitle(topicsModel.getTitle().strip());
        }
    }

    private void checkType() {
        if(TopicsUtils.checkTopicType(topicsModel.getType()).equals("KO")) {
            throw new EditTopicException("The topic type is not valid");
        }

        if(topicsModel.getType().equals("AS")) {
            for (OptionsModel option : topicsModel.getOptions()) {
                if (!topicsRepositoryPort.checkMember(option.getOption())) {
                    throw new EditTopicException("The user " + option.getOption() + " is not valid");
                }
            }
        }
    }

    private void validateOptions() {
        if ((topicsModel.getType().equals(Constants.TopicType.IMAGE_SINGLE.toString()) || topicsModel.getType().equals(Constants.TopicType.IMAGE_MULTIPLE.toString()))
                && TopicsUtils.validateOptionsDataList(topicsModel.getOptions())) {
            throw new EditTopicException("It is mandatory to send the images and options for this type of topic");
        }
    }

    private void checkGroup(String groupName) {
        if(groupName.startsWith("*temp*")) {
            topicsRepositoryPort.deleteMembersByGroupId(topicsEntity.getGroupId());
            topicsRepositoryPort.deleteGroup(topicsEntity.getGroupId());
        }

        if(topicsModel.getGroupName() != null && !topicsRepositoryPort.checkIfGroupExists(topicsModel.getGroupName())) {
            throw new EditTopicException("The group does not exist");
        }
    }

    private void manageMembers(String groupName) {
        if(topicsModel.getMembers() != null && topicsModel.getGroupName() == null) {
            if(groupName.startsWith("*temp*")) {
                topicsRepositoryPort.deleteMembersByGroupId(topicsEntity.getGroupId());
                topicsRepositoryPort.deleteGroup(topicsEntity.getGroupId());
            }

            topicsModel.getMembers().forEach(member -> {
                if (!topicsRepositoryPort.checkMember(member)) {
                    throw new EditTopicException("The member "+ member +" is not a valid user");
                }
            });

            topicsRepositoryPort.createTemporalGroup("*temp* " + topicsEntity.getTitle(), topicsEntity.getAuthor());
            Integer groupId = topicsRepositoryPort.getGroupIdByGroupNameAndAdmin("*temp* " + topicsEntity.getTitle(), topicsEntity.getAuthor());

            topicsModel.getMembers().forEach(member -> {
                try {
                    if(!topicsEntity.getAuthor().equals(member)) {
                        topicsRepositoryPort.insertMember(groupId, topicsRepositoryPort.getUserIdByUserName(member));
                    }
                } catch (EditTopicException editTopicException) {
                    topicsRepositoryPort.deleteGroup(groupId);
                    throw new EditTopicException("An error occurred during the registration of group members (Group deleted)");
                }
            });

            topicsModel.setGroupName("*temp* " + topicsEntity.getTitle());
        } else {
            if(groupName.startsWith("*temp*")) {
                topicsRepositoryPort.deleteMembersByGroupId(topicsEntity.getGroupId());
                topicsRepositoryPort.deleteGroup(topicsEntity.getGroupId());
            }

            topicsEntity.setGroupId(topicsRepositoryPort.getGroupIdByGroupNameAndAdmin(topicsModel.getGroupName(), topicsModel.getUser()));

            if(topicsEntity.getGroupId() == null) {
                throw new EditTopicException("The user does not have any group with the indicated name");
            }
        }
    }

    private void manageOptions() {
        if(TopicsUtils.checkIfOptionsChanged(TopicsUtils.transformToOptionsModelList(topicsRepositoryPort.getOptions(topicsEntity.getId())), topicsModel.getOptions())) {
            updateOptions();
        } else {
            if(topicsModel.getType().equals(Constants.TopicType.IMAGE_SINGLE.toString()) || topicsModel.getType().equals(Constants.TopicType.IMAGE_MULTIPLE.toString())) {
                updateOptionsImages();
            }
        }
    }

    private void updateOptions() {
        topicsRepositoryPort.deleteOptions(topicsEntity.getId());
        List<OptionsModel> optionsDataList = TopicsUtils.initiateVoting(topicsModel.getType(), topicsModel.getOptions());
        optionsDataList.forEach(option -> {
            try {
                if(option.getImage() == null || option.getImage().isBlank()) {
                    topicsRepositoryPort.insertOption(topicsEntity.getId(), option.getOption(), option.getVotes());
                } else {
                    topicsRepositoryPort.insertOption(topicsEntity.getId(), option.getImage(), option.getOption(), option.getVotes());
                }
            } catch (EditTopicException editTopicException) {
                throw new EditTopicException("An error occurred during the registration of the options");
            }
        });
        topicsRepositoryPort.deleteVotedByOnTopic(topicsEntity.getId());
    }

    private void updateOptionsImages() {
        List<OptionsModel> optionsListWithImageChanges = TopicsUtils.checkIfImagesChanged(TopicsUtils.transformToOptionsModelList(topicsRepositoryPort.getOptions(topicsEntity.getId())), topicsModel.getOptions());
        if(!optionsListWithImageChanges.isEmpty()) {
            try {
                optionsListWithImageChanges.forEach(option -> topicsRepositoryPort.updateOptionImage(option.getId(), option.getImage()));
            } catch (EditTopicException editTopicException) {
                throw new EditTopicException("An error occurred during the update of the options");
            }
        }
    }

    private void checkCloseDate() {
        if(topicsModel.getCloseDate() != null) {
            if(topicsModel.getCloseDate().isBlank() && topicsEntity.getCloseDate() != null) {
                topicsEntity.setCloseDate(null);
            } else {
                topicsEntity.setCloseDate(topicsModel.getCloseDate());
            }
        } else {
            if(topicsEntity.getCloseDate() != null) {
                topicsEntity.setCloseDate(null);
            }
        }
    }

    private void manageCloseDate() {
        if(topicsModel.getCloseDate() != null && !topicsModel.getCloseDate().isBlank()) {
            String dateString = TopicsUtils.validateFormatDate(topicsModel.getCloseDate());
            if(dateString.contains("KO")) {
                throw new EditTopicException(dateString);
            } else {
                topicsEntity.setCloseDate(topicsModel.getCloseDate());
            }

            try {
                LocalDate inputDate = LocalDate.parse(dateString, DateTimeFormatter.ofPattern("yyyyMMdd"));
                LocalDate currentDate = LocalDate.now();

                if (!inputDate.isAfter(currentDate)) {
                    throw new EditTopicException("The closing date cannot be earlier than the current date");
                }
            } catch (DateTimeParseException dateTimeParseException) {
                throw new EditTopicException(dateTimeParseException.getMessage());
            }try {
                LocalDate inputDate = LocalDate.parse(dateString, DateTimeFormatter.ofPattern("yyyyMMdd"));
                LocalDate currentDate = LocalDate.now();

                if (!inputDate.isAfter(currentDate)) {
                    throw new EditTopicException("The closing date cannot be earlier than the current date");
                }
            } catch (DateTimeParseException dateTimeParseException) {
                throw new EditTopicException(dateTimeParseException.getMessage());
            }
        }
    }

    private void manageMailService() {
        if(!"false".equals(environment.getProperty("activate.mail"))) {
            mailService.sendEmail(topicsModel.getTitle(), topicsRepositoryPort.getEmailsByGroupId(topicsModel.getGroupId()));
        }
    }
}
