package com.capgemini.ailabar.topics.infraestructure.adapters.out;

import com.capgemini.ailabar.topics.domain.ports.out.TopicsRepositoryPort;
import com.capgemini.ailabar.topics.infraestructure.entities.TopicsEntity;
import com.capgemini.ailabar.topics.infraestructure.repositories.TopicsRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public class TopicsRepositoryAdapter implements TopicsRepositoryPort {
    private final TopicsRepository topicsRepository;

    public TopicsRepositoryAdapter(TopicsRepository topicsRepository) {
        this.topicsRepository = topicsRepository;
    }

    @Override
    public List<TopicsEntity> loadTopics(String user) {
        return topicsRepository.loadTopicsByUser(user);
    }

    @Override
    public void createTopic(TopicsEntity topicsEntity) {
        topicsRepository.save(topicsEntity);
    }

    @Override
    public void editTopic(TopicsEntity topicsEntity) {
        topicsRepository.save(topicsEntity);
    }

    @Override
    public void closeTopic(TopicsEntity topicsEntity) {
        topicsRepository.save(topicsEntity);
    }

    @Override
    public void reOpenTopic(TopicsEntity topicsEntity) {
        topicsRepository.save(topicsEntity);
    }

    @Override
    public void deleteTopic(Integer id) {
        topicsRepository.deleteById(id);
    }

    @Override
    public List<TopicsEntity> getTopicsDatabase() {
        return topicsRepository.findAll();
    }

    @Override
    public TopicsEntity getTopicsEntityById(Integer id) {
        return topicsRepository.getTopicsEntityById(id);
    }

    @Override
    public boolean checkAuthorization(String user, String token) {
        return topicsRepository.checkAuthorization(user, token);
    }

    @Override
    public boolean checkByTitleAndAuthor(String title, String user) {
        return topicsRepository.checkByTitleAndAuthor(title, user);
    }

    @Override
    public boolean checkId(Integer id) {
        return topicsRepository.existsById(id);
    }
}
