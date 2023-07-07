package com.capgemini.beni.ailabar.entity;

import com.capgemini.beni.ailabar.dto.GroupsDto;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import static org.junit.jupiter.api.Assertions.assertEquals;

@ExtendWith(SpringExtension.class)
@SpringBootTest
class GroupsEntityTest {
    @Test
    void testId() {
        GroupsEntity groupsEntity = new GroupsEntity();

        Integer expectedId = 1;
        groupsEntity.setId(expectedId);

        Integer actualId = groupsEntity.getId();

        assertEquals(expectedId, actualId);
    }

    @Test
    void testGroupName() {
        GroupsEntity groupsEntity = new GroupsEntity();

        String expectedGroupName = "TestGroup";
        groupsEntity.setGroupName(expectedGroupName);

        String actualGroupName = groupsEntity.getGroupName();

        assertEquals(expectedGroupName, actualGroupName);
    }

    @Test
    void testMembers() {
        GroupsEntity groupsEntity = new GroupsEntity();

        String expectedMembers = "Member1, Member2, Member3";
        groupsEntity.setMembers(expectedMembers);

        String actualMembers = groupsEntity.getMembers();

        assertEquals(expectedMembers, actualMembers);
    }

    @Test
    void testAdmin() {
        GroupsEntity groupsEntity = new GroupsEntity();

        String expectedAdmin = "AdminUser";
        groupsEntity.setAdmin(expectedAdmin);

        String actualAdmin = groupsEntity.getAdmin();

        assertEquals(expectedAdmin, actualAdmin);
    }

    @Test
    void testConstructor() {
        GroupsDto groupsDto = new GroupsDto();
        groupsDto.setGroupName("TestGroup");
        groupsDto.setMembers("Member1, Member2, Member3");
        groupsDto.setAdmin("AdminUser");

        GroupsEntity groupsEntity = new GroupsEntity(groupsDto);

        assertEquals(groupsDto.getGroupName(), groupsEntity.getGroupName());
        assertEquals(groupsDto.getMembers(), groupsEntity.getMembers());
        assertEquals(groupsDto.getAdmin(), groupsEntity.getAdmin());
    }

}

