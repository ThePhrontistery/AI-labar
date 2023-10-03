package com.capgemini.ailabar.commons.utils;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

@Component
public class DailyBatch {
    private static final Logger logger = LogManager.getLogger(DailyBatch.class);
    private final DailyBatchService dailyBatchService;

    @Autowired
    public DailyBatch(DailyBatchService dailyBatchService) {
        logger.info("Start batch");
        this.dailyBatchService = dailyBatchService;
    }

    @Scheduled(cron = "0 0 0 * * ?")
    public void runDailyBatch() {
        logger.info("Enter on runDailyBatch");
        dailyBatchService.closeExpiredTopics();
    }
}
