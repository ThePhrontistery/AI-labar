package com.capgemini.beni.ailabar.dto;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import static org.junit.jupiter.api.Assertions.assertEquals;

@ExtendWith(SpringExtension.class)
@SpringBootTest
class GroupsDtoTest {
    @Test
    void testId() {
        GroupsDto groupsDto = new GroupsDto();

        Integer expectedId = 1;
        groupsDto.setId(expectedId);

        Integer actualId = groupsDto.getId();

        assertEquals(expectedId, actualId);
    }

    @Test
    void testGroupName() {
        GroupsDto groupsDto = new GroupsDto();

        String expectedGroupName = "Test Group";
        groupsDto.setGroupName(expectedGroupName);

        String actualGroupName = groupsDto.getGroupName();

        assertEquals(expectedGroupName, actualGroupName);
    }

    @Test
    void testMembers() {
        GroupsDto groupsDto = new GroupsDto();

        String expectedMembers = "John, Jane, Bob";
        groupsDto.setMembers(expectedMembers);

        String actualMembers = groupsDto.getMembers();

        assertEquals(expectedMembers, actualMembers);
    }

    @Test
    void testAdmin() {
        GroupsDto groupsDto = new GroupsDto();

        String expectedAdmin = "AdminUser";
        groupsDto.setAdmin(expectedAdmin);

        String actualAdmin = groupsDto.getAdmin();

        assertEquals(expectedAdmin, actualAdmin);
    }

    @Test
    void testNewGroupName() {
        GroupsDto groupsDto = new GroupsDto();

        String expectedNewGroupName = "New Group";
        groupsDto.setNewGroupName(expectedNewGroupName);

        String actualNewGroupName = groupsDto.getNewGroupName();

        assertEquals(expectedNewGroupName, actualNewGroupName);
    }
}

