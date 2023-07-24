package com.capgemini.beni.ailabar.application.service;

import com.capgemini.beni.ailabar.infrastructure.entity.TopicsEntity;
import com.capgemini.beni.ailabar.infrastructure.repository.TopicsRepository;
import com.capgemini.beni.ailabar.infrastructure.utils.Constants;
import com.capgemini.beni.ailabar.infrastructure.utils.OptionsData;
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

    public String initiateVoting(String type, List<OptionsData> list) {
        List<OptionsData> optionsDataList = list.stream()
                .map(element -> {
                    if (type.equals(Constants.TopicType.IMAGE_SINGLE.toString()) || type.equals(Constants.TopicType.IMAGE_MULTIPLE.toString())) {
                        return new OptionsData(element.getImage(), element.getOption(), 0);
                    } else {
                        return new OptionsData(element.getOption(), 0);
                    }
                })
                .collect(Collectors.toList());

        Gson gson = new Gson();
        return gson.toJson(optionsDataList);
    }
    
    public TopicsEntity findTopicByIdAndUser(Integer id, String user) {
        return topicsRepository.findTopicByIdAndUser(id, user);
    }
}
