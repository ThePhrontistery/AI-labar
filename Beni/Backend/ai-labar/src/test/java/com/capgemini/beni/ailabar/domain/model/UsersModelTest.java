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
class UsersModelTest {
    @Test
    void testIdProperty() {
        UsersModel dto = new UsersModel();
        dto.setId(1);
        assertEquals(1, dto.getId());
    }

    @Test
    void testUserProperty() {
        UsersModel dto = new UsersModel();
        dto.setUser("Username");
        assertEquals("Username", dto.getUser());
    }

    @Test
    void testPasswordProperty() {
        UsersModel dto = new UsersModel();
        dto.setPassword("Password");
        assertEquals("Password", dto.getPassword());
    }

    @Test
    void testEmailProperty() {
        UsersModel dto = new UsersModel();
        dto.setEmail("email@example.com");
        assertEquals("email@example.com", dto.getEmail());
    }

    @Test
    void testTokenProperty() {
        UsersModel dto = new UsersModel();
        dto.setToken("Token");
        assertEquals("Token", dto.getToken());
    }

    @Test
    void testMatcherProperty() {
        UsersModel dto = new UsersModel();
        dto.setMatcher("Matcher");
        assertEquals("Matcher", dto.getMatcher());
    }

    @Test
    void testNewUserProperty() {
        UsersModel dto = new UsersModel();
        dto.setNewUser("NewUser");
        assertEquals("NewUser", dto.getNewUser());
    }

    @Test
    void testNewPasswordProperty() {
        UsersModel dto = new UsersModel();
        dto.setNewPassword("NewPassword");
        assertEquals("NewPassword", dto.getNewPassword());
    }

    @Test
    void testUsersListProperty() {
        UsersModel dto = new UsersModel();
        List<String> usersList = Arrays.asList("User1", "User2");
        dto.setUsersList(usersList);
        assertEquals(usersList, dto.getUsersList());
    }
}
