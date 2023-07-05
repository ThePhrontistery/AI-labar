package com.capgemini.beni.ailabar.dto;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

@ExtendWith(MockitoExtension.class)
@SpringBootTest
class UsersDtoTest {
    @Test
    void testId() {
        UsersDto usersDto = new UsersDto();

        Integer expectedId = 1;
        usersDto.setId(expectedId);

        Integer actualId = usersDto.getId();

        assertEquals(expectedId, actualId);
    }

    @Test
    void testUser() {
        UsersDto usersDto = new UsersDto();

        String expectedUser = "TestUser";
        usersDto.setUser(expectedUser);

        String actualUser = usersDto.getUser();

        assertEquals(expectedUser, actualUser);
    }

    @Test
    void testPassword() {
        UsersDto usersDto = new UsersDto();

        String expectedPassword = "TestPassword";
        usersDto.setPassword(expectedPassword);

        String actualPassword = usersDto.getPassword();

        assertEquals(expectedPassword, actualPassword);
    }

    @Test
    void testEmail() {
        UsersDto usersDto = new UsersDto();

        String expectedEmail = "test@example.com";
        usersDto.setEmail(expectedEmail);

        String actualEmail = usersDto.getEmail();

        assertEquals(expectedEmail, actualEmail);
    }

    @Test
    void testNewUser() {
        UsersDto usersDto = new UsersDto();

        String expectedNewUser = "NewUser";
        usersDto.setNewUser(expectedNewUser);

        String actualNewUser = usersDto.getNewUser();

        assertEquals(expectedNewUser, actualNewUser);
    }

    @Test
    void testNewPassword() {
        UsersDto usersDto = new UsersDto();

        String expectedNewPassword = "NewPassword";
        usersDto.setNewPassword(expectedNewPassword);

        String actualNewPassword = usersDto.getNewPassword();

        assertEquals(expectedNewPassword, actualNewPassword);
    }

    @Test
    void testUsersList() {
        UsersDto usersDto = new UsersDto();

        List<String> expectedUsersList = Arrays.asList("User1", "User2", "User3");
        usersDto.setUsersList(expectedUsersList);

        List<String> actualUsersList = usersDto.getUsersList();

        assertEquals(expectedUsersList, actualUsersList);
    }
}

