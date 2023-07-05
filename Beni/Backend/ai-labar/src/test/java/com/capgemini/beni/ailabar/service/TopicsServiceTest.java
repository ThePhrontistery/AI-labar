package com.capgemini.beni.ailabar.service;

import com.capgemini.beni.ailabar.entity.TopicsEntity;
import com.capgemini.beni.ailabar.repository.TopicsRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@SpringBootTest
class TopicsServiceTest {
    @Mock
    private TopicsRepository topicsRepository;

    @InjectMocks
    private TopicsService topicsService;

    @BeforeEach
    void setUp() {
        Mockito.reset(topicsRepository);
    }

    @Test
    void testLogin() {
        String user = "john";
        String password = "password";
        boolean expectedResult = true;

        when(topicsRepository.existsByUserAndPassword(user, password)).thenReturn(expectedResult);

        boolean result = topicsService.login(user, password);

        assertEquals(expectedResult, result);
        verify(topicsRepository, times(1)).existsByUserAndPassword(user, password);
        verifyNoMoreInteractions(topicsRepository);
    }

    @Test
    void testLoadTopics() {
        String user = "john";
        List<TopicsEntity> expectedTopics = Arrays.asList(new TopicsEntity(), new TopicsEntity());

        when(topicsRepository.findByUser(user)).thenReturn(expectedTopics);

        List<TopicsEntity> result = topicsService.loadTopics(user);

        assertEquals(expectedTopics, result);
        verify(topicsRepository, times(1)).findByUser(user);
        verifyNoMoreInteractions(topicsRepository);
    }

    @Test
    void testOpenTopic() {
        Integer id = 1;
        TopicsEntity expectedTopic = new TopicsEntity();

        when(topicsRepository.findTopicsEntityById(id)).thenReturn(expectedTopic);

        TopicsEntity result = topicsService.openTopic(id);

        assertEquals(expectedTopic, result);
        verify(topicsRepository, times(1)).findTopicsEntityById(id);
        verifyNoMoreInteractions(topicsRepository);
    }

    @Test
    void testSaveTopic() {
        TopicsEntity topicEntity = new TopicsEntity();

        topicsService.saveTopic(topicEntity);

        verify(topicsRepository, times(1)).save(topicEntity);
        verifyNoMoreInteractions(topicsRepository);
    }

    @Test
    void testGetTopicForEdit() {
        Integer id = 1;
        TopicsEntity expectedTopic = new TopicsEntity();

        when(topicsRepository.findByIdIfExists(id)).thenReturn(expectedTopic);

        TopicsEntity result = topicsService.getTopicForEdit(id);

        assertEquals(expectedTopic, result);
        verify(topicsRepository, times(1)).findByIdIfExists(id);
        verifyNoMoreInteractions(topicsRepository);
    }

    @Test
    void testDeleteTopic() {
        Integer id = 1;

        topicsService.deleteTopic(id);

        verify(topicsRepository, times(1)).deleteById(id);
        verifyNoMoreInteractions(topicsRepository);
    }

    @Test
    void testGetAllTopicsData() {
        List<TopicsEntity> expectedTopics = Arrays.asList(new TopicsEntity(), new TopicsEntity());

        when(topicsRepository.findAll()).thenReturn(expectedTopics);

        List<TopicsEntity> result = topicsService.getAllTopicsData();

        assertEquals(expectedTopics, result);
        verify(topicsRepository, times(1)).findAll();
        verifyNoMoreInteractions(topicsRepository);
    }

    @Test
    void testExistsById() {
        Integer id = 1;
        boolean expectedResult = true;

        when(topicsRepository.existsById(id)).thenReturn(expectedResult);

        boolean result = topicsService.existsById(id);

        assertEquals(expectedResult, result);
        verify(topicsRepository, times(1)).existsById(id);
        verifyNoMoreInteractions(topicsRepository);
    }

    @Test
    void testFindTopicsEntityById() {
        Integer id = 1;
        TopicsEntity expectedTopic = new TopicsEntity();

        when(topicsRepository.findTopicsEntityById(id)).thenReturn(expectedTopic);

        TopicsEntity result = topicsService.findTopicsEntityById(id);

        assertEquals(expectedTopic, result);
        verify(topicsRepository, times(1)).findTopicsEntityById(id);
        verifyNoMoreInteractions(topicsRepository);
    }

    @Test
    void testExistsByTitleAndAuthor() {
        String title = "Topic A";
        String author = "John";
        boolean expectedResult = true;

        when(topicsRepository.existsByTitleAndAuthor(title, author)).thenReturn(expectedResult);

        boolean result = topicsService.existsByTitleAndAuthor(title, author);

        assertEquals(expectedResult, result);
        verify(topicsRepository, times(1)).existsByTitleAndAuthor(title, author);
        verifyNoMoreInteractions(topicsRepository);
    }

    @Test
    void testInitiateVotingWithOptions() {
        String options = "Option 1, Option 2, Option 3";
        String expectedResult = "Option 1:0, Option 2:0, Option 3:0";

        String result = topicsService.initiateVoting(options);

        assertEquals(expectedResult, result);
    }

    @Test
    void testInitiateVotingWithList() {
        List<String> list = Arrays.asList("Option 1", "Option 2", "Option 3");
        String expectedResult = "Option 1:0, Option 2:0, Option 3:0";

        String result = topicsService.initiateVoting(list);

        assertEquals(expectedResult, result);
    }

    @Test
    void testFindTopicByIdAndUser() {
        Integer id = 1;
        String user = "John";
        TopicsEntity expectedTopic = new TopicsEntity();

        when(topicsRepository.findTopicByIdAndUser(id, user)).thenReturn(expectedTopic);

        TopicsEntity result = topicsService.findTopicByIdAndUser(id, user);

        assertEquals(expectedTopic, result);
        verify(topicsRepository, times(1)).findTopicByIdAndUser(id, user);
        verifyNoMoreInteractions(topicsRepository);
    }
}

