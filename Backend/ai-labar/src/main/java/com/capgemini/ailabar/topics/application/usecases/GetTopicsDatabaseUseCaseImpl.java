package com.capgemini.ailabar.topics.application.usecases;

import com.capgemini.ailabar.topics.domain.exceptions.GetTopicsDatabaseException;
import com.capgemini.ailabar.topics.domain.models.TopicsModel;
import com.capgemini.ailabar.topics.domain.ports.in.GetTopicsDatabaseUseCase;
import com.capgemini.ailabar.topics.domain.ports.out.TopicsRepositoryPort;
import com.capgemini.ailabar.topics.infraestructure.entities.TopicsEntity;
import com.capgemini.ailabar.topics.infraestructure.utils.TopicsUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

@Service
@Transactional(readOnly = true)
public class GetTopicsDatabaseUseCaseImpl implements GetTopicsDatabaseUseCase {
    private final TopicsRepositoryPort topicsRepositoryPort;

    public GetTopicsDatabaseUseCaseImpl(TopicsRepositoryPort topicsRepositoryPort) {
        this.topicsRepositoryPort = topicsRepositoryPort;
    }

    @Override
    public List<TopicsModel> getTopicsDatabase() {
        List<TopicsEntity> topicsList = topicsRepositoryPort.getTopicsDatabase();

        if (topicsList.isEmpty()) {
            throw new GetTopicsDatabaseException("There are no topics in database");
        }

        List<TopicsEntity> allTopicsData = new ArrayList<>(topicsList);


        List<TopicsModel> allModels = new ArrayList<>();
        allTopicsData.stream()
                .map(topicEntity -> {
                    TopicsModel topicsModel = new TopicsModel(topicEntity);
                    topicsModel.setGroupName(topicsRepositoryPort.getGroupNameByGroupId(topicsModel.getGroupId()));
                    topicsModel.setOptions(TopicsUtils.transformToOptionsModelList(topicsRepositoryPort.getOptions(topicsModel.getId())));
                    List<Integer> topicVotedByList = topicsRepositoryPort.getUsersHasVotedByTopicId(topicEntity.getId());
                    List<String> usersNameList;
                    if(topicVotedByList != null && !topicVotedByList.isEmpty()) {
                        usersNameList = topicVotedByList.stream()
                                .map(topicsRepositoryPort::getUserNameByUserId)
                                .collect(Collectors.toList());

                        topicsModel.setVotedByList(usersNameList);
                    }
                    return topicsModel;
                })
                .forEach(allModels::add);

        return allModels;
    }
}
