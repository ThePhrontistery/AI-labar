package com.capgemini.beni.ailabar.service;

import com.capgemini.beni.ailabar.entity.TopicsEntity;
import com.capgemini.beni.ailabar.repository.TopicsRepository;
import com.google.gson.Gson;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import javax.transaction.Transactional;
import java.util.*;
import java.util.stream.Collectors;

@Service
@Transactional
public class TopicsService {
    private final TopicsRepository topicsRepository;

    @Autowired
    public TopicsService(TopicsRepository topicsRepository) {
        this.topicsRepository = topicsRepository;
    }

    public Boolean login(String user, String password) {
        return topicsRepository.existsByUserAndPassword(user, password);
    }

    public List<TopicsEntity> loadTopics(String user) {
        return topicsRepository.findByUser(user);
    }

    public TopicsEntity openTopic(Integer id) {
        return topicsRepository.findTopicsEntityById(id);
    }

    public void saveTopic(TopicsEntity topicEntity) {
        topicsRepository.save(topicEntity);
    }

    public TopicsEntity getTopicForEdit (Integer id) {
        return topicsRepository.findByIdIfExists(id);
    }

    public void deleteTopic(Integer id) {
        topicsRepository.deleteById(id);
    }

    public List<TopicsEntity> getAllTopicsData() {
        return topicsRepository.findAll();
    }

    public Boolean existsById (Integer id) {
        return topicsRepository.existsById(id);
    }

    public TopicsEntity findTopicsEntityById (Integer id) {
        return topicsRepository.findTopicsEntityById(id);
    }

    public Boolean existsByTitleAndAuthor(String title, String author) {
        return topicsRepository.existsByTitleAndAuthor(title, author);
    }

    public String initiateVoting(List<String> list) {
        Map<String, Integer> map = new LinkedHashMap<>();
        for (String element : list) {
            map.put(element, 0);
        }
        Gson gson = new Gson();
        return gson.toJson(map);
    }

    public TopicsEntity findTopicByIdAndUser(Integer id, String user) {
        return topicsRepository.findTopicByIdAndUser(id, user);
    }
}
