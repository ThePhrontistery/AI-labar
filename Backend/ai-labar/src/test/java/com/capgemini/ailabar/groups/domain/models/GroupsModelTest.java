package com.capgemini.ailabar.groups.domain.models;

import com.capgemini.ailabar.groups.infraestructure.entities.GroupsEntity;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.extension.ExtendWith;

@ExtendWith(MockitoExtension.class)
class GroupsModelTest {
    @Mock
    private GroupsEntity mockGroupsEntity;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    void testConstructorFromEntity() {
        when(mockGroupsEntity.getId()).thenReturn(1);
        when(mockGroupsEntity.getGroupName()).thenReturn("TestGroup");
        when(mockGroupsEntity.getAdmin()).thenReturn("AdminUser");

        GroupsModel groupsModel = new GroupsModel(mockGroupsEntity);

        assertEquals(1, groupsModel.getId());
        assertEquals("TestGroup", groupsModel.getGroupName());
        assertEquals("AdminUser", groupsModel.getAdmin());

        verify(mockGroupsEntity, times(1)).getId();
        verify(mockGroupsEntity, times(1)).getGroupName();
        verify(mockGroupsEntity, times(1)).getAdmin();
    }
}
