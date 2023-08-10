package com.capgemini.ailabar.commons.utils;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

@Component
public class DailyBatch {
    private final DailyBatchService dailyBatchService;

    @Autowired
    public DailyBatch(DailyBatchService dailyBatchService) {
        this.dailyBatchService = dailyBatchService;
    }

    @Scheduled(cron = "0 0 0 * * ?")
    public void runDailyBatch() {
        dailyBatchService.closeExpiredTopics();
    }
}
