package com.capgemini.ailabar.domain.model;

import org.junit.jupiter.api.BeforeEach;
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

    private GroupsModel groupsModel;

    @BeforeEach
    void setUp() {
        groupsModel = new GroupsModel();
    }

    @Test
    void testIdProperty() {
        groupsModel.setId(1);
        assertEquals(1, groupsModel.getId());
    }

    @Test
    void testGroupNameProperty() {
        groupsModel.setGroupName("Group Name");
        assertEquals("Group Name", groupsModel.getGroupName());
    }

    @Test
    void testMembersProperty() {
        List<String> members = Arrays.asList("Member1", "Member2");
        groupsModel.setMembers(members);
        assertEquals(members, groupsModel.getMembers());
    }

    @Test
    void testAdminProperty() {
        groupsModel.setAdmin("Admin");
        assertEquals("Admin", groupsModel.getAdmin());
    }

    @Test
    void testNewGroupNameProperty() {
        groupsModel.setNewGroupName("New Group Name");
        assertEquals("New Group Name", groupsModel.getNewGroupName());
    }

    @Test
    void testUserProperty() {
        groupsModel.setUser("User");
        assertEquals("User", groupsModel.getUser());
    }

    @Test
    void testTokenProperty() {
        groupsModel.setToken("Token");
        assertEquals("Token", groupsModel.getToken());
    }
}

