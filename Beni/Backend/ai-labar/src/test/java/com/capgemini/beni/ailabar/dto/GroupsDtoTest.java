package com.capgemini.beni.ailabar.dto;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

@ExtendWith(SpringExtension.class)
@SpringBootTest
class GroupsDtoTest {
    @Test
    void testIdProperty() {
        GroupsDto dto = new GroupsDto();
        dto.setId(1);
        assertEquals(1, dto.getId());
    }

    @Test
    void testGroupNameProperty() {
        GroupsDto dto = new GroupsDto();
        dto.setGroupName("Group Name");
        assertEquals("Group Name", dto.getGroupName());
    }

    @Test
    void testMembersProperty() {
        GroupsDto dto = new GroupsDto();
        List<String> members = Arrays.asList("Member1", "Member2");
        dto.setMembers(members);
        assertEquals(members, dto.getMembers());
    }

    @Test
    void testAdminProperty() {
        GroupsDto dto = new GroupsDto();
        dto.setAdmin("Admin");
        assertEquals("Admin", dto.getAdmin());
    }

    @Test
    void testNewGroupNameProperty() {
        GroupsDto dto = new GroupsDto();
        dto.setNewGroupName("New Group Name");
        assertEquals("New Group Name", dto.getNewGroupName());
    }

    @Test
    void testUserProperty() {
        GroupsDto dto = new GroupsDto();
        dto.setUser("User");
        assertEquals("User", dto.getUser());
    }

    @Test
    void testTokenProperty() {
        GroupsDto dto = new GroupsDto();
        dto.setToken("Token");
        assertEquals("Token", dto.getToken());
    }
}

