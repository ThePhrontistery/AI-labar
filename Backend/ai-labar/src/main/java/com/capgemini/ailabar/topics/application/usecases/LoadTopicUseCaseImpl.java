package com.capgemini.ailabar.topics.application.usecases;

import com.capgemini.ailabar.commons.utils.Constants;
import com.capgemini.ailabar.commons.utils.DateTime;
import com.capgemini.ailabar.options.domain.models.OptionsModel;
import com.capgemini.ailabar.topics.domain.exceptions.LoadTopicException;
import com.capgemini.ailabar.topics.domain.models.TopicsModel;
import com.capgemini.ailabar.topics.domain.ports.in.LoadTopicUseCase;
import com.capgemini.ailabar.topics.domain.ports.out.TopicsRepositoryPort;
import com.capgemini.ailabar.topics.infraestructure.utils.TopicsUtils;
import com.capgemini.ailabar.topics.infraestructure.entities.TopicsEntity;
import com.capgemini.ailabar.users.domain.models.UsersModel;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.EntityManager;
import javax.persistence.Query;
import java.util.*;
import java.util.stream.Collectors;

@Service
@Transactional(readOnly = true)
public class LoadTopicUseCaseImpl implements LoadTopicUseCase {
    private final TopicsRepositoryPort topicsRepositoryPort;
    private final EntityManager entityManager;

    public LoadTopicUseCaseImpl(TopicsRepositoryPort topicsRepositoryPort, EntityManager entityManager) {
        this.topicsRepositoryPort = topicsRepositoryPort;
        this.entityManager = entityManager;
    }

    @Override
    public Map<String, Object> loadTopics(UsersModel usersModel) {
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

        if(usersModel.getFilters() != null && !usersModel.getFilters().isEmpty()) {
            return loadTopicsWithFilters(usersModel, userId, groupIds, requestedPage, elementsPerPage, offset);
        }

        List<TopicsEntity> loadTopics = topicsRepositoryPort.loadTopics(usersModel.getUser(), groupIds, elementsPerPage, offset);

        List<TopicsModel> allModels = this.transformToTopicsModel(loadTopics, userId);


        int totalTopics = topicsRepositoryPort.countTotalTopics(usersModel.getUser(), groupIds);

        List<Map<String, Integer>> pagination = new ArrayList<>();
        Map<String, Integer> pageInfo = new HashMap<>();
        pageInfo.put("page", requestedPage);
        pageInfo.put("elements", allModels.size());
        pageInfo.put("total", totalTopics);
        pagination.add(pageInfo);

        Map<String, Object> response = new HashMap<>();
        response.put("pagination", pagination);
        response.put("entity", allModels);

        return response;
    }

    private List<OptionsModel> addUsersPhotos(List<OptionsModel> optionsModelList) {
        return optionsModelList.stream()
                .map(option -> {
                    String userPhoto = topicsRepositoryPort.getUserPhotoByOption(option.getOption());
                    return new OptionsModel(userPhoto, option.getOption(), option.getVotes());
                })
                .collect(Collectors.toList());
    }

    @SuppressWarnings("unchecked")
    private Map<String, Object> loadTopicsWithFilters(UsersModel usersModel, int userId, List<Integer> groupIds, int requestedPage, int elementsPerPage, int offset) {
        boolean mines = false;
        boolean status = false;
        boolean votePending = false;
        int statusValue = 1;

        for (String filter : usersModel.getFilters()) {
            switch (filter) {
                case "mines":
                    mines = true;
                    break;
                case "opened":
                    status = true;
                    statusValue = 1;
                    break;
                case "closed":
                    status = true;
                    statusValue = 0;
                    break;
                case "votePending":
                    votePending = true;
                    break;
            }
        }

        List<TopicsEntity> loadTopics;
        int totalTopics = 0;

        String countQuery = "SELECT COUNT(t.id) FROM topics t ";
        String loadQuery = "SELECT t.* FROM topics t ";

        if (mines) {
            countQuery += "WHERE t.author = :user ";
            loadQuery += "WHERE t.author = :user ";
        } else {
            countQuery += "WHERE (t.author = :user OR (t.group_id IN :groupIds AND t.author != :user)) ";
            loadQuery += "WHERE (t.author = :user OR (t.group_id IN :groupIds AND t.author != :user)) ";
        }

        if (status) {
            countQuery += "AND t.status = :status ";
            loadQuery += "AND t.status = :status ";
        }

        if (votePending) {
            countQuery += "AND t.id NOT IN (SELECT v.topic_id FROM voted_by v WHERE v.user_id = :userId) ";
            loadQuery += "AND t.id NOT IN (SELECT v.topic_id FROM voted_by v WHERE v.user_id = :userId) ";
        }

        loadQuery += "ORDER BY t.close_date DESC, t.id DESC  " +
                "LIMIT :limit OFFSET :offset";

        Query nativeCountQuery = entityManager.createNativeQuery(countQuery)
                .setParameter("user", usersModel.getUser());

        Query nativeLoadQuery = entityManager.createNativeQuery(loadQuery, TopicsEntity.class)
                .setParameter("user", usersModel.getUser())
                .setParameter("limit", elementsPerPage)
                .setParameter("offset", offset);

        if(!mines) {
            nativeCountQuery.setParameter("groupIds", groupIds);
            nativeLoadQuery.setParameter("groupIds", groupIds);
        }

        if(status) {
            String valueStatus=""+statusValue;
            nativeCountQuery.setParameter("status",valueStatus);
            nativeLoadQuery.setParameter("status", valueStatus);
        }

        if(votePending) {
            nativeCountQuery.setParameter("userId", userId);
            nativeLoadQuery.setParameter("userId", userId);
        }

        totalTopics = ((Number) nativeCountQuery.getSingleResult()).intValue();
        loadTopics = nativeLoadQuery.getResultList();

        List<TopicsModel> allModels = this.transformToTopicsModel(loadTopics, userId);

        List<Map<String, Integer>> pagination = new ArrayList<>();
        Map<String, Integer> pageInfo = new HashMap<>();
        pageInfo.put("page", requestedPage);
        pageInfo.put("elements", allModels.size());
        pageInfo.put("total", totalTopics);
        pagination.add(pageInfo);

        Map<String, Object> response = new HashMap<>();
        response.put("pagination", pagination);
        response.put("entity", allModels);

        return response;
    }

    private List<TopicsModel> transformToTopicsModel(List<TopicsEntity> loadTopics, Integer userId) {
        List<TopicsModel> allModels = new ArrayList<>();

        loadTopics.stream()
                .map(topicEntity -> {
                    TopicsModel topicsModel = new TopicsModel(topicEntity);
                    topicsModel.setGroupName(topicsRepositoryPort.getGroupNameByGroupId(topicsModel.getGroupId()));
                    topicsModel.setOptions(TopicsUtils.transformToOptionsModelList(topicsRepositoryPort.getOptions(topicsModel.getId())));
                    if(topicsModel.getType().equals(String.valueOf(Constants.TopicType.AS))) {
                        topicsModel.setOptions(addUsersPhotos(topicsModel.getOptions()));
                    }
                    topicsModel.setCanVote(!topicsRepositoryPort.checkIfUserAlreadyVoted(topicEntity.getId(), userId));
                    topicsModel.setCloseDateString(DateTime.timestampToStringWithoutTime(topicsModel.getCloseDate()));
                    return topicsModel;
                })
                .forEach(allModels::add);

        return allModels;
    }
}
