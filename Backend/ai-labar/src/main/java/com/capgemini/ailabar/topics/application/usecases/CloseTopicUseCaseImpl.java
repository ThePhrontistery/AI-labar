package com.capgemini.ailabar.topics.application.usecases;

import com.capgemini.ailabar.commons.utils.Constants;
import com.capgemini.ailabar.topics.domain.exceptions.CloseTopicException;
import com.capgemini.ailabar.topics.domain.models.TopicsModel;
import com.capgemini.ailabar.topics.domain.ports.in.CloseTopicUseCase;
import com.capgemini.ailabar.topics.domain.ports.out.TopicsRepositoryPort;
import com.capgemini.ailabar.topics.infraestructure.entities.TopicsEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional
public class CloseTopicUseCaseImpl implements CloseTopicUseCase {
    private final TopicsRepositoryPort topicsRepositoryPort;

    public CloseTopicUseCaseImpl(TopicsRepositoryPort topicsRepositoryPort) {
        this.topicsRepositoryPort = topicsRepositoryPort;
    }

    @Override
    public void closeTopic(TopicsModel topicsModel) {
        if(Boolean.FALSE.equals(topicsRepositoryPort.checkAuthorization(topicsModel.getUser(), topicsModel.getToken()))) {
            throw new CloseTopicException("Unauthorized user");
        }

        TopicsEntity topicEntity = topicsRepositoryPort.getTopicsEntityById(topicsModel.getId());

        if(topicEntity == null) {
            throw new CloseTopicException("There is no topic with that id");
        }

        if(!topicEntity.getAuthor().equals(topicsModel.getUser())) {
            throw new CloseTopicException("The user is not the author of the topic");
        }

        if(topicEntity.getStatus().equals(Constants.STATUS_CLOSED)) {
            throw new CloseTopicException("The topic is currently closed");
        }

        topicEntity.setStatus(Constants.STATUS_CLOSED);

        topicsRepositoryPort.closeTopic(topicEntity);
    }
}
