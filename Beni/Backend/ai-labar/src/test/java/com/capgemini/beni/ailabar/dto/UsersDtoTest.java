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
class UsersDtoTest {
    @Test
    void testIdProperty() {
        UsersDto dto = new UsersDto();
        dto.setId(1);
        assertEquals(1, dto.getId());
    }

    @Test
    void testUserProperty() {
        UsersDto dto = new UsersDto();
        dto.setUser("Username");
        assertEquals("Username", dto.getUser());
    }

    @Test
    void testPasswordProperty() {
        UsersDto dto = new UsersDto();
        dto.setPassword("Password");
        assertEquals("Password", dto.getPassword());
    }

    @Test
    void testEmailProperty() {
        UsersDto dto = new UsersDto();
        dto.setEmail("email@example.com");
        assertEquals("email@example.com", dto.getEmail());
    }

    @Test
    void testTokenProperty() {
        UsersDto dto = new UsersDto();
        dto.setToken("Token");
        assertEquals("Token", dto.getToken());
    }

    @Test
    void testMatcherProperty() {
        UsersDto dto = new UsersDto();
        dto.setMatcher("Matcher");
        assertEquals("Matcher", dto.getMatcher());
    }

    @Test
    void testNewUserProperty() {
        UsersDto dto = new UsersDto();
        dto.setNewUser("NewUser");
        assertEquals("NewUser", dto.getNewUser());
    }

    @Test
    void testNewPasswordProperty() {
        UsersDto dto = new UsersDto();
        dto.setNewPassword("NewPassword");
        assertEquals("NewPassword", dto.getNewPassword());
    }

    @Test
    void testUsersListProperty() {
        UsersDto dto = new UsersDto();
        List<String> usersList = Arrays.asList("User1", "User2");
        dto.setUsersList(usersList);
        assertEquals(usersList, dto.getUsersList());
    }
}
