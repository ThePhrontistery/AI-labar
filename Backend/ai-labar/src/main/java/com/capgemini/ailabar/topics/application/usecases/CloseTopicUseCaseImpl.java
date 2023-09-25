package com.capgemini.ailabar.topics.application.usecases;

import com.capgemini.ailabar.commons.utils.DateTime;
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

        TopicsEntity topicsEntity = topicsRepositoryPort.getTopicsEntityById(topicsModel.getId());

        if(topicsEntity == null) {
            throw new CloseTopicException("There is no topic with that id");
        }

        if(!topicsEntity.getAuthor().equals(topicsModel.getUser())) {
            throw new CloseTopicException("The user is not the author of the topic");
        }

        if(topicsEntity.getStatus().equals(0)) {
            throw new CloseTopicException("The topic is currently closed");
        }

        topicsEntity.setStatus(0);
        topicsEntity.setReopeningDate(null);
        topicsEntity.setExecutedClosureDate(DateTime.actualDateAndTime());

        topicsRepositoryPort.closeTopic(topicsEntity);
    }
}
