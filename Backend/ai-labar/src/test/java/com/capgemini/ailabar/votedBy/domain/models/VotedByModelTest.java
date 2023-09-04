package com.capgemini.ailabar.votedBy.domain.models;

import com.capgemini.ailabar.topics.infraestructure.entities.TopicsEntity;
import com.capgemini.ailabar.users.infraestructure.entities.UsersEntity;
import com.capgemini.ailabar.votedby.domain.models.VotedByModel;
import com.capgemini.ailabar.votedby.infraestructure.entities.VotedByEntity;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.extension.ExtendWith;

@ExtendWith(MockitoExtension.class)
class VotedByModelTest {

    @Mock
    private VotedByEntity mockVotedByEntity;

    @Mock
    private TopicsEntity mockTopicsEntity;

    @Mock
    private UsersEntity mockUsersEntity;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        when(mockVotedByEntity.getTopic()).thenReturn(mockTopicsEntity);
        when(mockVotedByEntity.getUser()).thenReturn(mockUsersEntity);
    }

    @Test
    void testConstructorFromEntity() {
        when(mockVotedByEntity.getId()).thenReturn(1);
        when(mockVotedByEntity.getTopic().getId()).thenReturn(2);
        when(mockVotedByEntity.getUser().getId()).thenReturn(3);

        VotedByModel votedByModel = new VotedByModel();
        votedByModel.setId(mockVotedByEntity.getId());
        votedByModel.setTopicId(mockVotedByEntity.getTopic().getId());
        votedByModel.setUserId(mockVotedByEntity.getUser().getId());

        assertEquals(1, votedByModel.getId());
        assertEquals(2, votedByModel.getTopicId());
        assertEquals(3, votedByModel.getUserId());

        verify(mockVotedByEntity, times(1)).getId();
        verify(mockVotedByEntity, times(2)).getTopic();
        verify(mockTopicsEntity, times(1)).getId();
        verify(mockVotedByEntity, times(2)).getUser();
        verify(mockUsersEntity, times(1)).getId();
    }
}
