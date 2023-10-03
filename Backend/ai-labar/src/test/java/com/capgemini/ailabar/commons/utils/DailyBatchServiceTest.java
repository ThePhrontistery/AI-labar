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

import java.sql.Timestamp;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.Date;
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
        LocalDate localDate = LocalDate.of(2022, 12, 31);
        LocalDateTime localDateTime = localDate.atStartOfDay();
        Timestamp timestamp = Timestamp.valueOf(localDateTime);

        Date currentTimestamp = Date.from(Instant.now());

        TopicsEntity topic1 = new TopicsEntity();
        topic1.setStatus(1);
        topic1.setCloseDate(timestamp);

        TopicsEntity topic2 = new TopicsEntity();
        topic2.setStatus(1);
        topic2.setCloseDate(timestamp);

        List<TopicsEntity> topics = Arrays.asList(topic1, topic2);

        when(topicsRepository.getByStatusAndCloseDateLessThanEqual(anyInt(), any(Date.class))).thenReturn(topics);

        dailyBatchService.closeExpiredTopics();

        verify(topicsRepository, times(1)).getByStatusAndCloseDateLessThanEqual(anyInt(), any(Date.class));
    }
}
