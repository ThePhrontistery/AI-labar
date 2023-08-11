package com.capgemini.ailabar.topics.application.usecases;

import com.capgemini.ailabar.topics.domain.exceptions.LoadTopicException;
import com.capgemini.ailabar.topics.domain.models.TopicsModel;
import com.capgemini.ailabar.topics.domain.ports.in.LoadTopicUseCase;
import com.capgemini.ailabar.topics.domain.ports.out.TopicsRepositoryPort;
import com.capgemini.ailabar.topics.infraestructure.utils.TopicsUtils;
import com.capgemini.ailabar.topics.infraestructure.entities.TopicsEntity;
import com.capgemini.ailabar.users.domain.models.UsersModel;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;

@Service
@Transactional(readOnly = true)
public class LoadTopicUseCaseImpl implements LoadTopicUseCase {
    private final TopicsRepositoryPort topicsRepositoryPort;

    public LoadTopicUseCaseImpl(TopicsRepositoryPort topicsRepositoryPort) {
        this.topicsRepositoryPort = topicsRepositoryPort;
    }

    @Override
    public List<TopicsModel> loadTopics(UsersModel usersModel) {
        if(usersModel.getUser().isBlank() || usersModel.getToken().isEmpty()
                || usersModel.getElements() == null) {
            throw new LoadTopicException("User, token, and the number of items to display are required");
        }

        if(Boolean.FALSE.equals(topicsRepositoryPort.checkAuthorization(usersModel.getUser(), usersModel.getToken()))) {
            throw new LoadTopicException("Unauthorized user");
        }

        int elementsPerPage = usersModel.getElements();
        int requestedPage = usersModel.getPage() != null && usersModel.getPage() > 0 ? usersModel.getPage() : 1;

        int offset = (requestedPage - 1) * elementsPerPage;

        Integer userId = topicsRepositoryPort.getUserIdByUserName(usersModel.getUser());
        List<Integer> groupIds= topicsRepositoryPort.getGroupsWithMemberId(topicsRepositoryPort.getUserIdByUserName(usersModel.getUser()));
        List<TopicsEntity> loadTopics = topicsRepositoryPort.loadTopics(usersModel.getUser(), groupIds, elementsPerPage, offset);

        List<TopicsModel> allModels = new ArrayList<>();
        loadTopics.stream()
                .map(topicEntity -> {
                    TopicsModel topicsModel = new TopicsModel(topicEntity);
                    topicsModel.setGroupName(topicsRepositoryPort.getGroupNameByGroupId(topicsModel.getGroupId()));
                    topicsModel.setOptions(TopicsUtils.transformToOptionsModelList(topicsRepositoryPort.getOptions(topicsModel.getId())));
                    topicsModel.setCanVote(!topicsRepositoryPort.checkIfUserAlreadyVoted(topicEntity.getId(), userId));
                    return topicsModel;
                })
                .forEach(allModels::add);

        return allModels;
    }
}
