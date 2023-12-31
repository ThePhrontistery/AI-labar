package com.capgemini.ailabar.commons.utils;

import com.capgemini.ailabar.topics.infraestructure.entities.TopicsEntity;
import com.capgemini.ailabar.topics.infraestructure.repositories.TopicsRepository;
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
        topic1.setStatus(1);
        topic1.setCloseDate(currentDate.minusDays(1).format(dateFormatter));

        TopicsEntity topic2 = new TopicsEntity();
        topic2.setStatus(1);
        topic2.setCloseDate(currentDate.format(dateFormatter));

        List<TopicsEntity> topics = Arrays.asList(topic1, topic2);

        when(topicsRepository.getByStatusAndCloseDateLessThanEqual(1, currentDate.format(dateFormatter))).thenReturn(topics);

        dailyBatchService.closeExpiredTopics();

        verify(topicsRepository, times(1)).getByStatusAndCloseDateLessThanEqual(1, currentDate.format(dateFormatter));
    }
}
