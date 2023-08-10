package com.capgemini.ailabar.topics.application.usecases;

import com.capgemini.ailabar.options.infraestructure.entities.OptionsEntity;
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
        if(Boolean.FALSE.equals(topicsRepositoryPort.checkAuthorization(usersModel.getUser(), usersModel.getToken()))) {
            throw new LoadTopicException("Unauthorized user");
        }

        List<TopicsEntity> topicsListByAuthor = topicsRepositoryPort.loadTopicsByAuthor(usersModel.getUser());
        List<Integer> groupsIdList = topicsRepositoryPort.getGroupsWithMemberId(topicsRepositoryPort.getUserIdByUserName(usersModel.getUser()));
        List<TopicsEntity> topicsListByMember = new ArrayList<>();

        groupsIdList.forEach(groupId -> {
            try {
                List<TopicsEntity> loadedTopics = topicsRepositoryPort.loadTopicsByGroupId(groupId);

                if (!loadedTopics.isEmpty()) {
                    topicsListByMember.addAll(loadedTopics);
                }
            } catch (LoadTopicException loadTopicException) {
                throw new LoadTopicException("An error occurred during the loading of topics");
            }
        });

        if(topicsListByAuthor.isEmpty() && topicsListByMember.isEmpty() ) {
            throw new LoadTopicException("There are no topics related to the user");
        }

        List<TopicsEntity> allTopics = new ArrayList<>();
        allTopics.addAll(topicsListByMember);
        allTopics.addAll(topicsListByAuthor);


        List<TopicsModel> allModels = new ArrayList<>();
        allTopics.stream()
                .map(topicEntity -> {
                    TopicsModel topicsModel = new TopicsModel(topicEntity);
                    topicsModel.setGroupName(topicsRepositoryPort.getGroupNameByGroupId(topicsModel.getGroupId()));
                    topicsModel.setOptions(TopicsUtils.transformToOptionsModelList(topicsRepositoryPort.getOptions(topicsModel.getId())));
                    return topicsModel;
                })
                .forEach(allModels::add);

        return allModels;
    }
}
