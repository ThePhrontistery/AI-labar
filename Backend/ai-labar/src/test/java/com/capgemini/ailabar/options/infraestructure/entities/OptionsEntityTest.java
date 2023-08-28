package com.capgemini.ailabar.options.infraestructure.entities;

import com.capgemini.ailabar.topics.infraestructure.entities.TopicsEntity;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(MockitoExtension.class)
class OptionsEntityTest {

    @Mock
    private TopicsEntity mockTopicsEntity;

    @Test
    void testOptionsEntityCreation() {
        OptionsEntity optionsEntity = new OptionsEntity();
        optionsEntity.setTopic(mockTopicsEntity);
        optionsEntity.setId(1);
        optionsEntity.setImage("image.jpg");
        optionsEntity.setOption("TestOption");
        optionsEntity.setVotes(10);

        assertEquals(1, optionsEntity.getId());
        assertEquals(mockTopicsEntity, optionsEntity.getTopic());
        assertEquals("image.jpg", optionsEntity.getImage());
        assertEquals("TestOption", optionsEntity.getOption());
        assertEquals(10, optionsEntity.getVotes());
    }
}

