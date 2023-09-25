package com.capgemini.ailabar.topics.domain.models;

import com.capgemini.ailabar.topics.infraestructure.entities.TopicsEntity;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.extension.ExtendWith;

import java.sql.Timestamp;
import java.time.LocalDate;
import java.time.LocalDateTime;

@ExtendWith(MockitoExtension.class)
class TopicsModelTest {
    @Mock
    private TopicsEntity mockTopicsEntity;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    void testConstructorFromEntity() {
        LocalDate localDate = LocalDate.of(2022, 12, 31);
        LocalDateTime localDateTime = localDate.atStartOfDay();
        Timestamp timestamp = Timestamp.valueOf(localDateTime);

        when(mockTopicsEntity.getId()).thenReturn(1);
        when(mockTopicsEntity.getTitle()).thenReturn("TestTitle");
        when(mockTopicsEntity.getType()).thenReturn("TestType");
        when(mockTopicsEntity.getQuestion()).thenReturn("TestQuestion");
        when(mockTopicsEntity.getAuthor()).thenReturn("TestAuthor");
        when(mockTopicsEntity.getGroupId()).thenReturn(2);
        when(mockTopicsEntity.getCloseDate()).thenReturn(timestamp);
        when(mockTopicsEntity.getVisits()).thenReturn(100);
        when(mockTopicsEntity.getStatus()).thenReturn(1);

        TopicsModel topicsModel = new TopicsModel(mockTopicsEntity);

        assertEquals(1, topicsModel.getId());
        assertEquals("TestTitle", topicsModel.getTitle());
        assertEquals("TestType", topicsModel.getType());
        assertEquals("TestQuestion", topicsModel.getQuestion());
        assertEquals("TestAuthor", topicsModel.getAuthor());
        assertEquals(2, topicsModel.getGroupId());
        assertEquals(timestamp, topicsModel.getCloseDate());
        assertEquals(100, topicsModel.getVisits());
        assertEquals(1, topicsModel.getStatus());

        verify(mockTopicsEntity, times(1)).getId();
        verify(mockTopicsEntity, times(1)).getTitle();
        verify(mockTopicsEntity, times(1)).getType();
        verify(mockTopicsEntity, times(1)).getQuestion();
        verify(mockTopicsEntity, times(1)).getAuthor();
        verify(mockTopicsEntity, times(1)).getGroupId();
        verify(mockTopicsEntity, times(1)).getCloseDate();
        verify(mockTopicsEntity, times(1)).getVisits();
        verify(mockTopicsEntity, times(1)).getStatus();
    }
}
