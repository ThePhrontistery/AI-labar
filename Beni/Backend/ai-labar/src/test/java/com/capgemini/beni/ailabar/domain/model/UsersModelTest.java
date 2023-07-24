package com.capgemini.beni.ailabar.domain.model;

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
class UsersModelTest {

    private UsersModel usersModel;

    @BeforeEach
    void setUp() {
        usersModel = new UsersModel();
    }
    @Test
    void testIdProperty() {
        usersModel.setId(1);
        assertEquals(1, usersModel.getId());
    }

    @Test
    void testUserProperty() {
        usersModel.setUser("Username");
        assertEquals("Username", usersModel.getUser());
    }

    @Test
    void testPasswordProperty() {
        usersModel.setPassword("Password");
        assertEquals("Password", usersModel.getPassword());
    }

    @Test
    void testEmailProperty() {
        usersModel.setEmail("email@example.com");
        assertEquals("email@example.com", usersModel.getEmail());
    }

    @Test
    void testGenderProperty() {
        usersModel.setGender("H");
        assertEquals("H", usersModel.getGender());
    }

    @Test
    void testPhotoProperty() {
        usersModel.setPhoto("photo");
        assertEquals("photo", usersModel.getPhoto());
    }

    @Test
    void testTokenProperty() {
        usersModel.setToken("Token");
        assertEquals("Token", usersModel.getToken());
    }

    @Test
    void testMatcherProperty() {
        usersModel.setMatcher("Matcher");
        assertEquals("Matcher", usersModel.getMatcher());
    }

    @Test
    void testNewUserProperty() {
        usersModel.setNewUser("NewUser");
        assertEquals("NewUser", usersModel.getNewUser());
    }

    @Test
    void testNewPasswordProperty() {
        usersModel.setNewPassword("NewPassword");
        assertEquals("NewPassword", usersModel.getNewPassword());
    }

    @Test
    void testUsersListProperty() {
        List<String> usersList = Arrays.asList("User1", "User2");
        usersModel.setUsersList(usersList);
        assertEquals(usersList, usersModel.getUsersList());
    }
}
