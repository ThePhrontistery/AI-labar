package com.capgemini.beni.ailabar.application.service;

import com.capgemini.beni.ailabar.infrastructure.entity.TopicsEntity;
import com.capgemini.beni.ailabar.infrastructure.repository.TopicsRepository;
import com.capgemini.beni.ailabar.infrastructure.utils.Constants;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.List;

@Service
@Transactional
public class DailyBatchService {
    private final TopicsRepository topicsRepository;

    private final DateTimeFormatter dateFormatter = DateTimeFormatter.ofPattern("yyyyMMdd");

    @Autowired
    public DailyBatchService(TopicsRepository topicsRepository) {
        this.topicsRepository = topicsRepository;
    }

    public void closeExpiredTopics() {
        LocalDate currentDate = LocalDate.now();

        List<TopicsEntity> topics = topicsRepository.findByStatusAndCloseDateLessThanEqual(Constants.STATUS_OPENED, currentDate.format(dateFormatter));

        for (TopicsEntity topic : topics) {
            topic.setStatus(Constants.STATUS_CLOSED);
            topicsRepository.save(topic);
        }
    }

}