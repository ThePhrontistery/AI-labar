package com.capgemini.ailabar.members.domain.models;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.assertEquals;

@ExtendWith(MockitoExtension.class)
class MembersModelTest {

    @Test
    void testConstructor() {
        Integer id = 1;
        Integer groupId = 2;
        Integer userId = 3;

        MembersModel membersModel = new MembersModel();
        membersModel.setId(id);
        membersModel.setGroupId(groupId);
        membersModel.setUserId(userId);

        assertEquals(id, membersModel.getId());
        assertEquals(groupId, membersModel.getGroupId());
        assertEquals(userId, membersModel.getUserId());
    }
}
