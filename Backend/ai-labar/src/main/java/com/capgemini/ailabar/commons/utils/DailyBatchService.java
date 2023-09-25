package com.capgemini.ailabar.commons.utils;

import com.capgemini.ailabar.topics.infraestructure.entities.TopicsEntity;
import com.capgemini.ailabar.topics.infraestructure.repositories.TopicsRepository;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.time.Instant;
import java.util.Date;
import java.util.List;

@Service
@Transactional
public class DailyBatchService {
    private static final Logger logger = LogManager.getLogger(DailyBatchService.class);
    private final TopicsRepository topicsRepository;

    @Autowired
    public DailyBatchService(TopicsRepository topicsRepository) {
        this.topicsRepository = topicsRepository;
    }

    public void closeExpiredTopics() {
        logger.info("Enter on closeExpiredTopics");
        Date currentTimestamp = Date.from(Instant.now());

        logger.info("Enter on getByStatusAndCloseDateLessThanEqual");
        List<TopicsEntity> topics = topicsRepository.getByStatusAndCloseDateLessThanEqual(1, currentTimestamp);

        logger.info("Closing topics...");
        for (TopicsEntity topic : topics) {
            topic.setStatus(0);
            topic.setExecutedClosureDate(DateTime.actualDateAndTime());
            topicsRepository.save(topic);
        }
        logger.info("Batch finished");
    }

}
