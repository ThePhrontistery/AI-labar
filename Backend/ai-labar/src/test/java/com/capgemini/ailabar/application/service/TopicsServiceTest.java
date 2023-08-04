package com.capgemini.ailabar.application.service;

import com.capgemini.ailabar.infrastructure.entity.TopicsEntity;
import com.capgemini.ailabar.infrastructure.repository.TopicsRepository;
import com.capgemini.ailabar.infrastructure.utils.Constants;
import com.capgemini.ailabar.infrastructure.utils.OptionsData;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(SpringExtension.class)
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

    @Value("${activate.mail}")
    private String activateMailProperty;

    @Test
    void testLogin() {
        String user = "john";
        String password = "password";

        when(topicsRepository.existsByUserAndPassword(user, password)).thenReturn(true);

        boolean result = topicsService.login(user, password);

        assertTrue(result);
        verify(topicsRepository, times(1)).existsByUserAndPassword(user, password);
        verifyNoMoreInteractions(topicsRepository);
    }

    @Test
    void testLoadTopics() {
        String user = "john";
        TopicsEntity topic1 = new TopicsEntity();
        TopicsEntity topic2 = new TopicsEntity();
        List<TopicsEntity> expectedTopics = Arrays.asList(topic1, topic2);

        when(topicsRepository.findByUser(user)).thenReturn(expectedTopics);

        List<TopicsEntity> result = topicsService.loadTopics(user);

        assertEquals(expectedTopics, result);
        verify(topicsRepository, times(1)).findByUser(user);
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
        TopicsEntity expectedEntity = new TopicsEntity();

        when(topicsRepository.findByIdIfExists(id)).thenReturn(expectedEntity);

        TopicsEntity result = topicsService.getTopicForEdit(id);

        assertEquals(expectedEntity, result);
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
        TopicsEntity topic1 = new TopicsEntity();
        TopicsEntity topic2 = new TopicsEntity();
        List<TopicsEntity> expectedTopics = Arrays.asList(topic1, topic2);

        when(topicsRepository.findAll()).thenReturn(expectedTopics);

        List<TopicsEntity> result = topicsService.getAllTopicsData();

        assertEquals(expectedTopics, result);
        verify(topicsRepository, times(1)).findAll();
        verifyNoMoreInteractions(topicsRepository);
    }

    @Test
    void testExistsById() {
        Integer id = 1;

        when(topicsRepository.existsById(id)).thenReturn(true);

        boolean result = topicsService.existsById(id);

        assertTrue(result);
        verify(topicsRepository, times(1)).existsById(id);
        verifyNoMoreInteractions(topicsRepository);
    }

    @Test
    void testFindTopicsEntityById() {
        Integer id = 1;
        TopicsEntity expectedEntity = new TopicsEntity();

        when(topicsRepository.findTopicsEntityById(id)).thenReturn(expectedEntity);

        TopicsEntity result = topicsService.findTopicsEntityById(id);

        assertEquals(expectedEntity, result);
        verify(topicsRepository, times(1)).findTopicsEntityById(id);
        verifyNoMoreInteractions(topicsRepository);
    }

    @Test
    void testExistsByTitleAndAuthor() {
        String title = "Topic";
        String author = "john";

        when(topicsRepository.existsByTitleAndAuthor(title, author)).thenReturn(true);

        boolean result = topicsService.existsByTitleAndAuthor(title, author);

        assertTrue(result);
        verify(topicsRepository, times(1)).existsByTitleAndAuthor(title, author);
        verifyNoMoreInteractions(topicsRepository);
    }

    @Test
    void testInitiateVoting_ImageType() {
        List<OptionsData> list = Arrays.asList(
                new OptionsData("imagen1", "Opción 1", 0),
                new OptionsData("imagen2", "Opción 2", 0)
        );
        String type = Constants.TopicType.IMAGE_SINGLE.toString();

        String result = topicsService.initiateVoting(type, list);

        String expected = "[{\"image\":\"imagen1\",\"option\":\"Opción 1\",\"votes\":0},{\"image\":\"imagen2\",\"option\":\"Opción 2\",\"votes\":0}]";
        assertEquals(expected, result);
    }

    @Test
    void testInitiateVoting_NonImageType() {
        List<OptionsData> list = Arrays.asList(
                new OptionsData("Opción 1", 0),
                new OptionsData("Opción 2", 0)
        );
        String type = Constants.TopicType.TEXT_SINGLE.toString();

        String result = topicsService.initiateVoting(type, list);

        String expected = "[{\"option\":\"Opción 1\",\"votes\":0},{\"option\":\"Opción 2\",\"votes\":0}]";
        assertEquals(expected, result);
    }

    @Test
    void testCheckMailActivate() {
        boolean result = topicsService.checkMailActivate();
        assertTrue(result);
    }

    @Test
    void testFindTopicByIdAndUser() {
        Integer id = 1;
        String user = "john";
        TopicsEntity expectedEntity = new TopicsEntity();

        when(topicsRepository.findTopicByIdAndUser(id, user)).thenReturn(expectedEntity);

        TopicsEntity result = topicsService.findTopicByIdAndUser(id, user);

        assertEquals(expectedEntity, result);
        verify(topicsRepository, times(1)).findTopicByIdAndUser(id, user);
        verifyNoMoreInteractions(topicsRepository);
    }
}
