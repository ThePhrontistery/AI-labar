package com.capgemini.ailabar.groups.infraestructure.entities;

import com.capgemini.ailabar.groups.domain.models.GroupsModel;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(MockitoExtension.class)
class GroupsEntityTest {

    @Mock
    private GroupsModel mockGroupsModel;

    @Test
    void testGroupsEntityCreation() {
        when(mockGroupsModel.getGroupName()).thenReturn("TestGroup");
        when(mockGroupsModel.getUser()).thenReturn("AdminUser");

        GroupsEntity groupsEntity = new GroupsEntity(mockGroupsModel);

        assertEquals("TestGroup", groupsEntity.getGroupName());
        assertEquals("AdminUser", groupsEntity.getAdmin());
    }
}
