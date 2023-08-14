package com.capgemini.ailabar.topics.infraestructure.entities;

import com.capgemini.ailabar.topics.domain.models.TopicsModel;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(MockitoExtension.class)
class TopicsEntityTest {

    @Mock
    private TopicsModel mockTopicsModel;

    @Test
    void testTopicsEntityCreation() {
        when(mockTopicsModel.getTitle()).thenReturn("TestTitle");
        when(mockTopicsModel.getType()).thenReturn("TestType");
        when(mockTopicsModel.getQuestion()).thenReturn("TestQuestion");
        when(mockTopicsModel.getAuthor()).thenReturn("TestAuthor");
        when(mockTopicsModel.getGroupId()).thenReturn(1);
        when(mockTopicsModel.getCloseDate()).thenReturn("2023-12-31");
        when(mockTopicsModel.getVisits()).thenReturn(100);
        when(mockTopicsModel.getStatus()).thenReturn(1);

        TopicsEntity topicsEntity = new TopicsEntity(mockTopicsModel);

        assertEquals("TestTitle", topicsEntity.getTitle());
        assertEquals("TestType", topicsEntity.getType());
        assertEquals("TestQuestion", topicsEntity.getQuestion());
        assertEquals("TestAuthor", topicsEntity.getAuthor());
        assertEquals(1, topicsEntity.getGroupId());
        assertEquals("2023-12-31", topicsEntity.getCloseDate());
        assertEquals(100, topicsEntity.getVisits());
        assertEquals(1, topicsEntity.getStatus());
    }
}
