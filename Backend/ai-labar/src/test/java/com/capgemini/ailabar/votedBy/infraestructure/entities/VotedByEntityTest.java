package com.capgemini.ailabar.votedBy.infraestructure.entities;

import com.capgemini.ailabar.topics.infraestructure.entities.TopicsEntity;
import com.capgemini.ailabar.users.infraestructure.entities.UsersEntity;
import com.capgemini.ailabar.votedby.infraestructure.entities.VotedByEntity;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(MockitoExtension.class)
class VotedByEntityTest {

    @Mock
    private TopicsEntity mockTopicsEntity;

    @Mock
    private UsersEntity mockUsersEntity;

    @Test
    void testVotedByEntityCreationAndRetrieval() {
        when(mockTopicsEntity.getId()).thenReturn(1);
        when(mockUsersEntity.getId()).thenReturn(2);

        VotedByEntity votedByEntity = new VotedByEntity();
        votedByEntity.setTopic(mockTopicsEntity);
        votedByEntity.setUser(mockUsersEntity);

        assertEquals(1, votedByEntity.getTopic().getId());
        assertEquals(2, votedByEntity.getUser().getId());
    }
}

