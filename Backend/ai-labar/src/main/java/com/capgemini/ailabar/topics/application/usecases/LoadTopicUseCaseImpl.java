package com.capgemini.ailabar.topics.application.usecases;

import com.capgemini.ailabar.topics.domain.exceptions.LoadTopicException;
import com.capgemini.ailabar.topics.domain.ports.in.LoadTopicUseCase;
import com.capgemini.ailabar.topics.domain.ports.out.TopicsRepositoryPort;
import com.capgemini.ailabar.topics.infraestructure.entities.TopicsEntity;
import com.capgemini.ailabar.users.domain.models.UsersModel;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
@Transactional(readOnly = true)
public class LoadTopicUseCaseImpl implements LoadTopicUseCase {
    private final TopicsRepositoryPort topicsRepositoryPort;

    public LoadTopicUseCaseImpl(TopicsRepositoryPort topicsRepositoryPort) {
        this.topicsRepositoryPort = topicsRepositoryPort;
    }

    @Override
    public List<TopicsEntity> loadTopics(UsersModel usersModel) {
        if(Boolean.FALSE.equals(topicsRepositoryPort.checkAuthorization(usersModel.getUser(), usersModel.getToken()))) {
            throw new LoadTopicException("Unauthorized user");
        }

        List<TopicsEntity> topicsList = topicsRepositoryPort.loadTopics(usersModel.getUser());

        if(topicsList.isEmpty()) {
            throw new LoadTopicException("There are no topics related to the user");
        }

        return topicsList;
    }
}
