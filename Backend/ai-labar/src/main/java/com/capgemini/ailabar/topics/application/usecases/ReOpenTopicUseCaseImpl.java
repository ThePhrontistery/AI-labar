package com.capgemini.ailabar.topics.application.usecases;

import com.capgemini.ailabar.commons.utils.Constants;
import com.capgemini.ailabar.topics.domain.exceptions.ReOpenTopicException;
import com.capgemini.ailabar.topics.domain.models.TopicsModel;
import com.capgemini.ailabar.topics.domain.ports.in.ReOpenTopicUseCase;
import com.capgemini.ailabar.topics.domain.ports.out.TopicsRepositoryPort;
import com.capgemini.ailabar.topics.infraestructure.entities.TopicsEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional
public class ReOpenTopicUseCaseImpl implements ReOpenTopicUseCase {
    private final TopicsRepositoryPort topicsRepositoryPort;

    public ReOpenTopicUseCaseImpl(TopicsRepositoryPort topicsRepositoryPort) {
        this.topicsRepositoryPort = topicsRepositoryPort;
    }

    @Override
    public void reOpenTopic(TopicsModel topicsModel) {
        if(Boolean.FALSE.equals(topicsRepositoryPort.checkAuthorization(topicsModel.getUser(), topicsModel.getToken()))) {
            throw new ReOpenTopicException("Unauthorized user");
        }

        TopicsEntity topicsEntity = topicsRepositoryPort.getTopicsEntityById(topicsModel.getId());

        if(topicsEntity == null) {
            throw new ReOpenTopicException("There is no topic with that id");
        }

        if(!topicsEntity.getAuthor().equals(topicsModel.getUser())) {
            throw new ReOpenTopicException("The user is not the author of the topic");
        }

        if(topicsEntity.getStatus().equals(Constants.STATUS_OPENED)) {
            throw new ReOpenTopicException("The topic is currently open");
        }

        topicsEntity.setStatus(Constants.STATUS_OPENED);

        topicsRepositoryPort.reOpenTopic(topicsEntity);
    }
}
