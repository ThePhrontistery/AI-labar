package com.capgemini.ailabar.members.infraestructure.entities;

import com.capgemini.ailabar.groups.infraestructure.entities.GroupsEntity;
import com.capgemini.ailabar.users.infraestructure.entities.UsersEntity;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(MockitoExtension.class)
class MembersEntityTest {

    @Mock
    private GroupsEntity mockGroupsEntity;

    @Mock
    private UsersEntity mockUsersEntity;

    @Test
    void testMembersEntityCreation() {
        MembersEntity membersEntity = new MembersEntity();
        membersEntity.setGroup(mockGroupsEntity);
        membersEntity.setUser(mockUsersEntity);
        membersEntity.setId(1);

        assertEquals(1, membersEntity.getId());
        assertEquals(mockGroupsEntity, membersEntity.getGroup());
        assertEquals(mockUsersEntity, membersEntity.getUser());
    }
}
