package com.capgemini.ailabar.topics.domain.ports.out;

import com.capgemini.ailabar.topics.infraestructure.entities.TopicsEntity;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface TopicsRepositoryPort {
    List<TopicsEntity> loadTopics(String user);
    void createTopic(TopicsEntity topicsEntity);
    void editTopic(TopicsEntity topicsEntity);
    void closeTopic(TopicsEntity topicsEntity);
    void reOpenTopic(TopicsEntity topicsEntity);
    void deleteTopic(Integer id);
    List<TopicsEntity> getTopicsDatabase();
    TopicsEntity getTopicsEntityById(Integer id);

    boolean checkAuthorization(String user, String token);
    boolean checkByTitleAndAuthor(String title, String user);
    boolean checkId(Integer id);
}
