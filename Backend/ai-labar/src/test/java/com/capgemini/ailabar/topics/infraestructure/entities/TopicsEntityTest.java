package com.capgemini.ailabar.topics.infraestructure.entities;

import com.capgemini.ailabar.topics.domain.models.TopicsModel;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.sql.Timestamp;
import java.time.LocalDate;
import java.time.LocalDateTime;

import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(MockitoExtension.class)
class TopicsEntityTest {

    @Mock
    private TopicsModel mockTopicsModel;

    @Test
    void testTopicsEntityCreation() {
        LocalDate localDate = LocalDate.of(2022, 12, 31);
        LocalDateTime localDateTime = localDate.atStartOfDay();
        Timestamp timestamp = Timestamp.valueOf(localDateTime);

        when(mockTopicsModel.getTitle()).thenReturn("TestTitle");
        when(mockTopicsModel.getType()).thenReturn("TestType");
        when(mockTopicsModel.getQuestion()).thenReturn("TestQuestion");
        when(mockTopicsModel.getAuthor()).thenReturn("TestAuthor");
        when(mockTopicsModel.getGroupId()).thenReturn(1);
        when(mockTopicsModel.getCloseDate()).thenReturn(timestamp);
        when(mockTopicsModel.getVisits()).thenReturn(100);
        when(mockTopicsModel.getStatus()).thenReturn(1);

        TopicsEntity topicsEntity = new TopicsEntity(mockTopicsModel);

        assertEquals("TestTitle", topicsEntity.getTitle());
        assertEquals("TestType", topicsEntity.getType());
        assertEquals("TestQuestion", topicsEntity.getQuestion());
        assertEquals("TestAuthor", topicsEntity.getAuthor());
        assertEquals(1, topicsEntity.getGroupId());
        assertEquals(timestamp, topicsEntity.getCloseDate());
        assertEquals(100, topicsEntity.getVisits());
        assertEquals(1, topicsEntity.getStatus());
    }
}
