package com.capgemini.ailabar.commons.utils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

@Component
public class DailyBatch {
    private static final Logger logger = LoggerFactory.getLogger(DailyBatch.class);
    private final DailyBatchService dailyBatchService;

    @Autowired
    public DailyBatch(DailyBatchService dailyBatchService) {
        logger.debug("Start batch");
        this.dailyBatchService = dailyBatchService;
    }

    @Scheduled(cron = "0 0 0 * * ?")
    public void runDailyBatch() {
        logger.debug("Enter on runDailyBatch");
        dailyBatchService.closeExpiredTopics();
    }
}
