package com.capgemini.ailabar.topics.application.usecases;

import com.capgemini.ailabar.commons.utils.Constants;
import com.capgemini.ailabar.commons.utils.MailService;
import com.capgemini.ailabar.topics.domain.exceptions.CreateTopicException;
import com.capgemini.ailabar.topics.domain.models.TopicsModel;
import com.capgemini.ailabar.topics.domain.ports.in.CreateTopicUseCase;
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
public class CreateTopicUseCaseImpl implements CreateTopicUseCase {
    private final TopicsRepositoryPort topicsRepositoryPort;
    private final Environment environment;
    private final MailService mailService;

    public CreateTopicUseCaseImpl(TopicsRepositoryPort topicsRepositoryPort, Environment environment,
                                  MailService mailService) {
        this.topicsRepositoryPort = topicsRepositoryPort;
        this.environment = environment;
        this.mailService = mailService;
    }

    /* Se deberá cambiar la gestión de las opciones, members y votedBy */
    @Override
    public void createTopic(TopicsModel topicsModel) {
        if(topicsModel.getTitle().isBlank() || topicsModel.getType().isBlank()
                || topicsModel.getQuestion().isBlank() || topicsModel.getOptions().isEmpty()
                || topicsModel.getUser().isBlank() || topicsModel.getMembers().isEmpty()) {
            throw new CreateTopicException("All data is required to edit a topic");
        }

        if(Boolean.FALSE.equals(topicsRepositoryPort.checkAuthorization(topicsModel.getUser(), topicsModel.getToken()))) {
            throw new CreateTopicException("Unauthorized user");
        }

        if(Boolean.TRUE.equals(topicsRepositoryPort.checkByTitleAndAuthor(topicsModel.getTitle().strip(), topicsModel.getUser()))) {
            throw new CreateTopicException("There is already a topic assigned to the author with that name");
        }

        topicsModel.setTitle(topicsModel.getTitle().strip());
        topicsModel.setAuthor(topicsModel.getUser().strip());
        if(TopicsValidator.checkTopicType(topicsModel.getType()).equals("KO")) {
            throw new CreateTopicException("The topic type is not valid");
        }
        topicsModel.setType(topicsModel.getType());

        if ((topicsModel.getType().equals(Constants.TopicType.IMAGE_SINGLE.toString()) || topicsModel.getType().equals(Constants.TopicType.IMAGE_MULTIPLE.toString()))
                && !TopicsValidator.validateOptionsDataList(topicsModel.getOptions())) {
            throw new CreateTopicException("It is mandatory to send the images and options for this type of topic");
        }

        topicsModel.setVisits(topicsModel.getVisits() != null ? topicsModel.getVisits() : 0);
        topicsModel.setStatus(topicsModel.getStatus() != null ? topicsModel.getStatus() : Constants.STATUS_OPENED);

        TopicsEntity topicsEntity = new TopicsEntity(topicsModel);
        topicsEntity.setOptions(TopicsValidator.initiateVoting(topicsModel.getType(), topicsModel.getOptions()));
        topicsEntity.setMembers(new Gson().toJson(topicsModel.getMembers()));

        if(topicsModel.getCloseDate() != null && !topicsModel.getCloseDate().isBlank()) {
            String dateString = TopicsValidator.validateFormatDate(topicsModel.getCloseDate());
            if(dateString.contains("KO")) {
                throw new CreateTopicException(dateString);
            } else {
                topicsEntity.setCloseDate(topicsModel.getCloseDate());
            }
        }

        if(!"false".equals(environment.getProperty("activate.mail"))) {
            mailService.sendEmail(topicsModel);
        }

        topicsRepositoryPort.createTopic(topicsEntity);
    }
}
