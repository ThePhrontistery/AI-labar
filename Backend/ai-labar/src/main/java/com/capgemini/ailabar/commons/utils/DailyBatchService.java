package com.capgemini.ailabar.commons.utils;

import com.capgemini.ailabar.topics.infraestructure.entities.TopicsEntity;
import com.capgemini.ailabar.topics.infraestructure.repositories.TopicsRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.time.Instant;
import java.util.List;

@Service
@Transactional
public class DailyBatchService {
    private final TopicsRepository topicsRepository;

    @Autowired
    public DailyBatchService(TopicsRepository topicsRepository) {
        this.topicsRepository = topicsRepository;
    }

    public void closeExpiredTopics() {
        Instant currentTimestamp = Instant.now();

        List<TopicsEntity> topics = topicsRepository.getByStatusAndCloseDateLessThanEqual(1, currentTimestamp.toEpochMilli());

        for (TopicsEntity topic : topics) {
            topic.setStatus(0);
            topic.setExecutedClosureDate(DateTime.actualDateAndTime());
            topicsRepository.save(topic);
        }
    }

}
