package com.capgemini.beni.ailabar.domain.model;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

@ExtendWith(SpringExtension.class)
@SpringBootTest
class GroupsModelTest {
    @Test
    void testIdProperty() {
        GroupsModel dto = new GroupsModel();
        dto.setId(1);
        assertEquals(1, dto.getId());
    }

    @Test
    void testGroupNameProperty() {
        GroupsModel dto = new GroupsModel();
        dto.setGroupName("Group Name");
        assertEquals("Group Name", dto.getGroupName());
    }

    @Test
    void testMembersProperty() {
        GroupsModel dto = new GroupsModel();
        List<String> members = Arrays.asList("Member1", "Member2");
        dto.setMembers(members);
        assertEquals(members, dto.getMembers());
    }

    @Test
    void testAdminProperty() {
        GroupsModel dto = new GroupsModel();
        dto.setAdmin("Admin");
        assertEquals("Admin", dto.getAdmin());
    }

    @Test
    void testNewGroupNameProperty() {
        GroupsModel dto = new GroupsModel();
        dto.setNewGroupName("New Group Name");
        assertEquals("New Group Name", dto.getNewGroupName());
    }

    @Test
    void testUserProperty() {
        GroupsModel dto = new GroupsModel();
        dto.setUser("User");
        assertEquals("User", dto.getUser());
    }

    @Test
    void testTokenProperty() {
        GroupsModel dto = new GroupsModel();
        dto.setToken("Token");
        assertEquals("Token", dto.getToken());
    }
}

