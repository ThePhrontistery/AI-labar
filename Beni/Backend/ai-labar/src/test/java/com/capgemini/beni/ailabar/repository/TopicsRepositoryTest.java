package com.capgemini.beni.ailabar.repository;

import com.capgemini.beni.ailabar.entity.TopicsEntity;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@SpringBootTest
class TopicsRepositoryTest {
    @Mock
    private TopicsRepository topicsRepository;

    @BeforeEach
    void setUp() {
        Mockito.reset(topicsRepository);
    }

    @Test
    void testExistsByUserAndPassword() {
        String user = "john";
        String password = "password";

        when(topicsRepository.existsByUserAndPassword(user, password)).thenReturn(true);

        boolean result = topicsRepository.existsByUserAndPassword(user, password);

        assertTrue(result);
        verify(topicsRepository, times(1)).existsByUserAndPassword(user, password);
        verifyNoMoreInteractions(topicsRepository);
    }

    @Test
    void testFindByUser() {
        String user = "john";
        TopicsEntity topic1 = new TopicsEntity();
        TopicsEntity topic2 = new TopicsEntity();
        List<TopicsEntity> expectedTopics = Arrays.asList(topic1, topic2);

        when(topicsRepository.findByUser(user)).thenReturn(expectedTopics);

        List<TopicsEntity> result = topicsRepository.findByUser(user);

        assertEquals(expectedTopics, result);
        verify(topicsRepository, times(1)).findByUser(user);
        verifyNoMoreInteractions(topicsRepository);
    }

    @Test
    void testFindTopicsEntityById() {
        Integer id = 1;
        TopicsEntity expectedEntity = new TopicsEntity();

        when(topicsRepository.findTopicsEntityById(id)).thenReturn(expectedEntity);

        TopicsEntity result = topicsRepository.findTopicsEntityById(id);

        assertEquals(expectedEntity, result);
        verify(topicsRepository, times(1)).findTopicsEntityById(id);
        verifyNoMoreInteractions(topicsRepository);
    }

    @Test
    void testExistsByTitleAndAuthor() {
        String title = "Topic";
        String author = "john";

        when(topicsRepository.existsByTitleAndAuthor(title, author)).thenReturn(true);

        boolean result = topicsRepository.existsByTitleAndAuthor(title, author);

        assertTrue(result);
        verify(topicsRepository, times(1)).existsByTitleAndAuthor(title, author);
        verifyNoMoreInteractions(topicsRepository);
    }

    @Test
    void testFindByIdIfExists() {
        Integer id = 1;
        TopicsEntity expectedEntity = new TopicsEntity();

        when(topicsRepository.findByIdIfExists(id)).thenReturn(expectedEntity);

        TopicsEntity result = topicsRepository.findByIdIfExists(id);

        assertEquals(expectedEntity, result);
        verify(topicsRepository, times(1)).findByIdIfExists(id);
        verifyNoMoreInteractions(topicsRepository);
    }

    @Test
    void testFindTopicByIdAndUser() {
        Integer id = 1;
        String user = "john";
        TopicsEntity expectedEntity = new TopicsEntity();

        when(topicsRepository.findTopicByIdAndUser(id, user)).thenReturn(expectedEntity);

        TopicsEntity result = topicsRepository.findTopicByIdAndUser(id, user);

        assertEquals(expectedEntity, result);
        verify(topicsRepository, times(1)).findTopicByIdAndUser(id, user);
        verifyNoMoreInteractions(topicsRepository);
    }

}