package com.capgemini.ailabar.topics.application.usecases;

import com.capgemini.ailabar.commons.utils.Constants;
import com.capgemini.ailabar.commons.utils.MailService;
import com.capgemini.ailabar.topics.domain.exceptions.EditTopicException;
import com.capgemini.ailabar.topics.domain.models.TopicsModel;
import com.capgemini.ailabar.topics.domain.ports.in.EditTopicUseCase;
import com.capgemini.ailabar.topics.domain.ports.out.TopicsRepositoryPort;
import com.capgemini.ailabar.topics.domain.validators.TopicsValidator;
import com.capgemini.ailabar.topics.infraestructure.entities.TopicsEntity;
import com.google.gson.Gson;
import org.springframework.context.annotation.PropertySource;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional
@PropertySource("classpath:application.properties")
public class EditTopicUseCaseImpl implements EditTopicUseCase {
    private final TopicsRepositoryPort topicsRepositoryPort;
    private final Environment environment;
    private final MailService mailService;

    public EditTopicUseCaseImpl(TopicsRepositoryPort topicsRepositoryPort, Environment environment,
                                MailService mailService) {
        this.topicsRepositoryPort = topicsRepositoryPort;
        this.environment = environment;
        this.mailService = mailService;
    }

    @Override
    public void editTopic(TopicsModel topicsModel) {
        if(topicsModel.getId() == null || topicsModel.getTitle().isBlank()
                || topicsModel.getType().isBlank() || topicsModel.getQuestion().isBlank()
                || topicsModel.getOptions().isEmpty() || topicsModel.getMembers().isEmpty()
                || topicsModel.getUser().isBlank()) {
            throw new EditTopicException("All data is required to edit a topic");
        }

        if(Boolean.FALSE.equals(topicsRepositoryPort.checkAuthorization(topicsModel.getUser(), topicsModel.getToken()))) {
            throw new EditTopicException("Unauthorized user");
        }

        if(Boolean.FALSE.equals(topicsRepositoryPort.checkId(topicsModel.getId()))) {
            throw new EditTopicException("There is no topic with that id");
        }

        TopicsEntity topicsEntity = topicsRepositoryPort.getTopicsEntityById(topicsModel.getId());

        if (!topicsModel.getUser().equals(topicsEntity.getAuthor())) {
            throw new EditTopicException("The user is not the author of the topic");
        }

        if(topicsEntity.getStatus().equals(Constants.STATUS_CLOSED)) {
            throw new EditTopicException("The topic is closed");
        }

        if(!topicsModel.getTitle().equals(topicsEntity.getTitle()) && Boolean.TRUE.equals(topicsRepositoryPort.checkByTitleAndAuthor(topicsModel.getTitle().strip(), topicsModel.getUser()))) {
            throw new EditTopicException("There is already a topic assigned to the author with that name");
        } else {
            topicsEntity.setTitle(topicsModel.getTitle().strip());
        }

        if(TopicsValidator.checkTopicType(topicsModel.getType()).equals("KO")) {
            throw new EditTopicException("The topic type is not valid");
        }

        if ((topicsModel.getType().equals(Constants.TopicType.IMAGE_SINGLE.toString()) || topicsModel.getType().equals(Constants.TopicType.IMAGE_MULTIPLE.toString()))
                && !TopicsValidator.validateOptionsDataList(topicsModel.getOptions())) {
            throw new EditTopicException("It is mandatory to send the images and options for this type of topic");
        }

        topicsEntity.setQuestion(topicsModel.getQuestion());

        TopicsValidator.getOptionsWithoutVotes(topicsEntity.getOptions());
        topicsEntity.setType(topicsModel.getType());
        topicsEntity.setOptions(TopicsValidator.initiateVoting(topicsModel.getType(), topicsModel.getOptions()));
        topicsEntity.setVotedBy(null);

        topicsEntity.setAuthor(topicsModel.getUser());
        topicsEntity.setMembers(new Gson().toJson(topicsModel.getMembers()));

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

        topicsEntity.setVisits(topicsModel.getVisits() != null ? topicsModel.getVisits() : 0);
        topicsEntity.setStatus(topicsModel.getStatus() != null ? topicsModel.getStatus() : Constants.STATUS_OPENED);

        if(topicsModel.getCloseDate() != null && !topicsModel.getCloseDate().isBlank()) {
            String dateString = TopicsValidator.validateFormatDate(topicsModel.getCloseDate());
            if(dateString.contains("KO")) {
                throw new EditTopicException(dateString);
            } else {
                topicsEntity.setCloseDate(topicsModel.getCloseDate());
            }
        }

        if(!"false".equals(environment.getProperty("activate.mail"))) {
            mailService.sendEmail(topicsModel);
        }

        topicsRepositoryPort.editTopic(topicsEntity);
    }
}
