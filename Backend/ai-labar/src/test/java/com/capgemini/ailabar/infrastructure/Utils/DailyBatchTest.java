package com.capgemini.ailabar.infrastructure.Utils;

import com.capgemini.ailabar.application.service.DailyBatchService;
import com.capgemini.ailabar.infrastructure.utils.DailyBatch;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

@ExtendWith(SpringExtension.class)
@SpringBootTest
class DailyBatchTest {

    @Mock
    private DailyBatchService dailyBatchService;

    @InjectMocks
    private DailyBatch dailyBatch;

    @Test
    void runDailyBatch_CallsCloseExpiredTopics() {
        dailyBatch.runDailyBatch();

        verify(dailyBatchService, times(1)).closeExpiredTopics();
    }
}
