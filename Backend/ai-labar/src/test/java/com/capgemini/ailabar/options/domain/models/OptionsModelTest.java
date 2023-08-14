package com.capgemini.ailabar.options.domain.models;

import com.capgemini.ailabar.options.infraestructure.entities.OptionsEntity;
import com.capgemini.ailabar.topics.infraestructure.entities.TopicsEntity;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(MockitoExtension.class)
class OptionsModelTest {
    @Mock
    private OptionsEntity mockOptionsEntity;

    @Mock
    private TopicsEntity mockTopicsEntity;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        when(mockOptionsEntity.getTopic()).thenReturn(mockTopicsEntity);
        when(mockTopicsEntity.getId()).thenReturn(2);
    }

    @Test
    void testConstructor() {
        Integer id = 1;
        String image = "image.jpg";
        String option = "TestOption";
        Integer votes = 10;

        when(mockOptionsEntity.getId()).thenReturn(id);
        when(mockOptionsEntity.getImage()).thenReturn(image);
        when(mockOptionsEntity.getOption()).thenReturn(option);
        when(mockOptionsEntity.getVotes()).thenReturn(votes);

        OptionsModel optionsModel = new OptionsModel();
        optionsModel.setId(mockOptionsEntity.getId());
        optionsModel.setTopicId(mockOptionsEntity.getTopic().getId());
        optionsModel.setImage(mockOptionsEntity.getImage());
        optionsModel.setOption(mockOptionsEntity.getOption());
        optionsModel.setVotes(mockOptionsEntity.getVotes());

        assertEquals(id, optionsModel.getId());
        assertEquals(2, optionsModel.getTopicId());
        assertEquals(image, optionsModel.getImage());
        assertEquals(option, optionsModel.getOption());
        assertEquals(votes, optionsModel.getVotes());

        verify(mockOptionsEntity, times(1)).getId();
        verify(mockOptionsEntity, times(1)).getTopic();
        verify(mockTopicsEntity, times(1)).getId();
        verify(mockOptionsEntity, times(1)).getImage();
        verify(mockOptionsEntity, times(1)).getOption();
        verify(mockOptionsEntity, times(1)).getVotes();
    }
}

