package com.capgemini.ailabar.topics.application.services;

import com.capgemini.ailabar.groups.domain.exceptions.GetGroupsDatabaseException;
import com.capgemini.ailabar.groups.domain.ports.in.GetGroupsDatabaseUseCase;
import com.capgemini.ailabar.groups.infraestructure.entities.GroupsEntity;
import com.capgemini.ailabar.topics.domain.exceptions.*;
import com.capgemini.ailabar.topics.domain.models.TopicsModel;
import com.capgemini.ailabar.topics.domain.ports.in.*;
import com.capgemini.ailabar.topics.infraestructure.entities.TopicsEntity;
import com.capgemini.ailabar.users.domain.models.UsersModel;
import org.springframework.stereotype.Service;
import javax.transaction.Transactional;
import java.util.*;

@Service
@Transactional
public class TopicsService implements LoadTopicUseCase, CreateTopicUseCase, EditTopicUseCase, CloseTopicUseCase,
        ReOpenTopicUseCase, DeleteTopicUseCase, GetTopicsDatabaseUseCase {
    private final LoadTopicUseCase loadTopicUseCase;
    private final CreateTopicUseCase createTopicUseCase;
    private final EditTopicUseCase editTopicUseCase;
    private final CloseTopicUseCase closeTopicUseCase;
    private final ReOpenTopicUseCase reOpenTopicUseCase;
    private final DeleteTopicUseCase deleteTopicUseCase;
    private final GetTopicsDatabaseUseCase getTopicsDatabaseUseCase;

    public TopicsService(LoadTopicUseCase loadTopicUseCase, CreateTopicUseCase createTopicUseCase,
                         EditTopicUseCase editTopicUseCase, CloseTopicUseCase closeTopicUseCase,
                         ReOpenTopicUseCase reOpenTopicUseCase, DeleteTopicUseCase deleteTopicUseCase,
                         GetTopicsDatabaseUseCase getTopicsDatabaseUseCase) {
        this.loadTopicUseCase = loadTopicUseCase;
        this.createTopicUseCase = createTopicUseCase;
        this.editTopicUseCase = editTopicUseCase;
        this.closeTopicUseCase = closeTopicUseCase;
        this.reOpenTopicUseCase = reOpenTopicUseCase;
        this.deleteTopicUseCase = deleteTopicUseCase;
        this.getTopicsDatabaseUseCase = getTopicsDatabaseUseCase;
    }

    @Override
    public List<TopicsEntity> loadTopics(UsersModel usersModel) {
        try {
            return loadTopicUseCase.loadTopics(usersModel);
        } catch (LoadTopicException loadTopicsException) {
            throw loadTopicsException;
        }
    }

    @Override
    public void createTopic(TopicsModel topicsModel) {
        try {
            createTopicUseCase.createTopic(topicsModel);
        } catch (CreateTopicException createTopicsException) {
            throw createTopicsException;
        }
    }

    @Override
    public void editTopic(TopicsModel topicsModel) {
        try {
            editTopicUseCase.editTopic(topicsModel);
        } catch (EditTopicException editTopicsException) {
            throw editTopicsException;
        }
    }

    @Override
    public void closeTopic(TopicsModel topicsModel) {
        try {
            closeTopicUseCase.closeTopic(topicsModel);
        } catch (CloseTopicException closeTopicsException) {
            throw closeTopicsException;
        }
    }

    @Override
    public void reOpenTopic(TopicsModel topicsModel) {
        try {
            reOpenTopicUseCase.reOpenTopic(topicsModel);
        } catch (ReOpenTopicException reOpenTopicsException) {
            throw reOpenTopicsException;
        }
    }

    @Override
    public void deleteTopic(TopicsModel topicsModel) {
        try {
            deleteTopicUseCase.deleteTopic(topicsModel);
        } catch (DeleteTopicException deleteTopicException) {
            throw deleteTopicException;
        }
    }

    @Override
    public List<TopicsEntity> getTopicsDatabase() {
        try {
            return getTopicsDatabaseUseCase.getTopicsDatabase();
        } catch (GetGroupsDatabaseException getGroupsDatabaseException) {
            throw getGroupsDatabaseException;
        }
    }

//    public Boolean login(String user, String password) {
//        return topicsRepository.existsByUserAndPassword(user, password);
//    }
//
//    public List<TopicsEntity> loadTopics(String user) {
//        return topicsRepository.findByUser(user);
//    }
//
//    public void saveTopic(TopicsEntity topicEntity) {
//        topicsRepository.save(topicEntity);
//    }
//
//    public TopicsEntity getTopicForEdit (Integer id) {
//        return topicsRepository.findByIdIfExists(id);
//    }
//
//    public void deleteTopic(Integer id) {
//        topicsRepository.deleteById(id);
//    }
//
//    public List<TopicsEntity> getAllTopicsData() {
//        return topicsRepository.findAll();
//    }
//
//    public Boolean existsById (Integer id) {
//        return topicsRepository.existsById(id);
//    }
//
//    public TopicsEntity findTopicsEntityById (Integer id) {
//        return topicsRepository.findTopicsEntityById(id);
//    }
//
//    public Boolean existsByTitleAndAuthor(String title, String author) {
//        return topicsRepository.existsByTitleAndAuthor(title, author);
//    }
//
//
//
//
//
//    public TopicsEntity findTopicByIdAndUser(Integer id, String user) {
//        return topicsRepository.findTopicByIdAndUser(id, user);
//    }
}
