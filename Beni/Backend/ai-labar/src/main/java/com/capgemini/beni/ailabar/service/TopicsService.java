package com.capgemini.beni.ailabar.service;

import com.capgemini.beni.ailabar.dto.TopicsDto;
import com.capgemini.beni.ailabar.entity.TopicsEntity;
import com.capgemini.beni.ailabar.repository.TopicsRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.util.List;

@Service
@Transactional
public class TopicsService {
    private final TopicsRepository topicsRepository;

    @Autowired
    public TopicsService(TopicsRepository topicsRepository) {
        this.topicsRepository = topicsRepository;
    }

    public List<TopicsEntity> loadTopics(String user) {
        return topicsRepository.findByUser(user);
    }

    public Boolean existsByTitleAndAuthor(String title, String author) {
        return topicsRepository.existsByTitleAndAuthor(title, author);
    }

    public void saveTopic(TopicsEntity topicEntity) {
        topicsRepository.save(topicEntity);
    }
}
