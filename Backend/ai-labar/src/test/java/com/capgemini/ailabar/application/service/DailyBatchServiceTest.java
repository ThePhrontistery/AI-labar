package com.capgemini.ailabar.application.service;

import com.capgemini.ailabar.infrastructure.entity.TopicsEntity;
import com.capgemini.ailabar.infrastructure.repository.TopicsRepository;
import com.capgemini.ailabar.infrastructure.utils.Constants;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.List;

import static org.mockito.Mockito.*;

@ExtendWith(SpringExtension.class)
@SpringBootTest
class DailyBatchServiceTest {

    @Mock
    private TopicsRepository topicsRepository;

    @InjectMocks
    private DailyBatchService dailyBatchService;

    @BeforeEach
    void setUp() {
        Mockito.reset(topicsRepository);
    }

    @Test
    void testCloseExpiredTopics() {
        DateTimeFormatter dateFormatter = DateTimeFormatter.ofPattern("yyyyMMdd");
        LocalDate currentDate = LocalDate.now();

        TopicsEntity topic1 = new TopicsEntity();
        topic1.setStatus(Constants.STATUS_OPENED);
        topic1.setCloseDate(currentDate.minusDays(1).format(dateFormatter));

        TopicsEntity topic2 = new TopicsEntity();
        topic2.setStatus(Constants.STATUS_OPENED);
        topic2.setCloseDate(currentDate.format(dateFormatter));

        List<TopicsEntity> topics = Arrays.asList(topic1, topic2);

        when(topicsRepository.findByStatusAndCloseDateLessThanEqual(Constants.STATUS_OPENED, currentDate.format(dateFormatter))).thenReturn(topics);

        dailyBatchService.closeExpiredTopics();

        verify(topicsRepository, times(1)).findByStatusAndCloseDateLessThanEqual(Constants.STATUS_OPENED, currentDate.format(dateFormatter));
    }
}
