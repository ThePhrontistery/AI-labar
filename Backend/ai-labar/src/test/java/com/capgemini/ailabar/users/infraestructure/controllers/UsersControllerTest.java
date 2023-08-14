package com.capgemini.ailabar.users.infraestructure.controllers;

import com.capgemini.ailabar.commons.utils.SpecialResponse;
import com.capgemini.ailabar.users.application.services.UsersService;
import com.capgemini.ailabar.users.domain.exceptions.*;
import com.capgemini.ailabar.users.domain.models.UsersModel;
import com.capgemini.ailabar.users.infraestructure.entities.UsersEntity;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class UsersControllerTest {

    @InjectMocks
    private UsersController usersController;

    @Mock
    private UsersService usersService;

    @Test
    void testLoginSuccess() {
        UsersModel usersModel = new UsersModel();
        usersModel.setUser("testUser");
        usersModel.setPassword("testPassword");

        String validToken = "validToken";
        when(usersService.login(usersModel)).thenReturn(validToken);

        String expectedMessage = "Login successful";

        ResponseEntity<SpecialResponse> actualResponse = usersController.login(usersModel);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        SpecialResponse specialResponse = actualResponse.getBody();
        assertNotNull(specialResponse);
        assertEquals(expectedMessage, specialResponse.getMessage());
        assertEquals(validToken, specialResponse.getEntity());

        verify(usersService, times(1)).login(usersModel);
    }

    @Test
    void testCreateUserSuccess() {
        UsersModel usersModel = new UsersModel();
        usersModel.setUser("newUser");
        usersModel.setPassword("password");
        usersModel.setEmail("user@example.com");
        usersModel.setGender("M");

        String expectedMessage = "User created successfully";

        doNothing().when(usersService).createUser(usersModel);

        ResponseEntity<SpecialResponse> actualResponse = usersController.createUser(usersModel);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        SpecialResponse specialResponse = actualResponse.getBody();
        assertNotNull(specialResponse);
        assertEquals(expectedMessage, specialResponse.getMessage());

        verify(usersService, times(1)).createUser(usersModel);
    }


    @Test
    void testEditUserSuccess() {
        UsersModel usersModel = new UsersModel();
        usersModel.setUser("existingUser");
        usersModel.setNewUser("newUser");
        usersModel.setNewPassword("newPassword");
        usersModel.setGender("Male");
        usersModel.setPhoto("newPhoto");
        usersModel.setToken("validToken");

        doNothing().when(usersService).editUser(usersModel);

        String expectedMessage = "User modified successfully";

        ResponseEntity<SpecialResponse> actualResponse = usersController.editUser(usersModel);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        SpecialResponse specialResponse = actualResponse.getBody();
        assertNotNull(specialResponse);
        assertEquals(expectedMessage, specialResponse.getMessage());

        verify(usersService, times(1)).editUser(usersModel);
    }

    @Test
    void testDeleteUserSuccess() {
        UsersModel usersModel = new UsersModel();
        usersModel.setUser("existingUser");
        usersModel.setToken("validToken");

        doNothing().when(usersService).deleteUser(usersModel);

        String expectedMessage = "User deleted successfully";

        ResponseEntity<SpecialResponse> actualResponse = usersController.deleteUser(usersModel);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        SpecialResponse specialResponse = actualResponse.getBody();
        assertNotNull(specialResponse);
        assertEquals(expectedMessage, specialResponse.getMessage());

        verify(usersService, times(1)).deleteUser(usersModel);
    }

    @Test
    void testGetUsersByMatchSuccess() {
        UsersModel usersModel = new UsersModel();
        usersModel.setUser("existingUser");
        usersModel.setToken("validToken");
        usersModel.setMatcher("searchTerm");

        List<String> userMatchesList = Arrays.asList("User1", "User2");

        when(usersService.getUsersByMatch(usersModel)).thenReturn(userMatchesList);

        String expectedMessage = userMatchesList.size() + " matches";

        ResponseEntity<SpecialResponse> actualResponse = usersController.getUsersByMatch(usersModel);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        SpecialResponse specialResponse = actualResponse.getBody();
        assertNotNull(specialResponse);
        assertEquals(expectedMessage, specialResponse.getMessage());
        assertEquals(userMatchesList, specialResponse.getEntity());

        verify(usersService, times(1)).getUsersByMatch(usersModel);
    }

    @Test
    void testGetAllUsersSuccess() {
        UsersModel usersModel = new UsersModel();
        usersModel.setUser("existingUser");
        usersModel.setToken("validToken");

        List<String> usersList = Arrays.asList("User1", "User2", "User3");

        when(usersService.getAllUsers(usersModel)).thenReturn(usersList);

        String expectedMessage = "List of users obtained successfully";

        ResponseEntity<SpecialResponse> actualResponse = usersController.getAllUsers(usersModel);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        SpecialResponse specialResponse = actualResponse.getBody();
        assertNotNull(specialResponse);
        assertEquals(expectedMessage, specialResponse.getMessage());
        assertEquals(usersList, specialResponse.getEntity());

        verify(usersService, times(1)).getAllUsers(usersModel);
    }

    @Test
    void testGetUsersDatabaseSuccess() {
        UsersEntity user1 = new UsersEntity();
        user1.setId(1);
        user1.setUser("User1");

        UsersEntity user2 = new UsersEntity();
        user2.setId(2);
        user2.setUser("User2");

        List<UsersEntity> usersEntitiesList = new ArrayList<>();
        usersEntitiesList.add(user1);
        usersEntitiesList.add(user2);

        when(usersService.getUsersDatabase()).thenReturn(usersEntitiesList);

        String expectedMessage = "OK";

        ResponseEntity<SpecialResponse> actualResponse = usersController.getUsersDatabase();

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        SpecialResponse specialResponse = actualResponse.getBody();
        assertNotNull(specialResponse);
        assertEquals(expectedMessage, specialResponse.getMessage());
        assertEquals(usersEntitiesList, specialResponse.getEntity());

        verify(usersService, times(1)).getUsersDatabase();
    }

    @Test
    void testHandlerLoginException() {
        LoginException exception = new LoginException("Login error message");

        ResponseEntity<SpecialResponse> response = usersController.handlerLoginException(exception);

        assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
        JSONObject expectedJson = new JSONObject();
        expectedJson.put("message", "Login error message");
        assertEquals(expectedJson.getString("message"), Objects.requireNonNull(response.getBody()).getMessage());
    }

    @Test
    void testHandlerCreateUserException() {
        CreateUserException exception = new CreateUserException("Create user error message");

        ResponseEntity<SpecialResponse> response = usersController.handlerCreateUserException(exception);

        assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
        JSONObject expectedJson = new JSONObject();
        expectedJson.put("message", "Create user error message");
        assertEquals(expectedJson.getString("message"), Objects.requireNonNull(response.getBody()).getMessage());
    }

    @Test
    void testHandlerEditUserException() {
        EditUserException exception = new EditUserException("Edit user error message");

        ResponseEntity<SpecialResponse> response = usersController.handlerEditUserException(exception);

        assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
        JSONObject expectedJson = new JSONObject();
        expectedJson.put("message", "Edit user error message");
        assertEquals(expectedJson.getString("message"), Objects.requireNonNull(response.getBody()).getMessage());
    }

    @Test
    void testHandlerDeleteUserException() {
        DeleteUserException exception = new DeleteUserException("Delete user error message");

        ResponseEntity<SpecialResponse> response = usersController.handlerDeleteUserException(exception);

        assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
        JSONObject expectedJson = new JSONObject();
        expectedJson.put("message", "Delete user error message");
        assertEquals(expectedJson.getString("message"), Objects.requireNonNull(response.getBody()).getMessage());
    }

    @Test
    void testHandlerGetUsersByMatchException() {
        GetUsersByMatchException exception = new GetUsersByMatchException("Get users by match error message");

        ResponseEntity<SpecialResponse> response = usersController.handlerGetUsersByMatchException(exception);

        assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
        JSONObject expectedJson = new JSONObject();
        expectedJson.put("message", "Get users by match error message");
        assertEquals(expectedJson.getString("message"), Objects.requireNonNull(response.getBody()).getMessage());
    }

    @Test
    void testHandlerGetAllUsersException() {
        GetAllUsersException exception = new GetAllUsersException("Get all users error message");

        ResponseEntity<SpecialResponse> response = usersController.handlerGetAllUsersException(exception);

        assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
        JSONObject expectedJson = new JSONObject();
        expectedJson.put("message", "Get all users error message");
        assertEquals(expectedJson.getString("message"), Objects.requireNonNull(response.getBody()).getMessage());
    }

    @Test
    void testHandlerGetUsersDatabaseException() {
        GetUsersDatabaseException exception = new GetUsersDatabaseException("Get users database error message");

        ResponseEntity<SpecialResponse> response = usersController.handlerGetUsersDatabaseException(exception);

        assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
        JSONObject expectedJson = new JSONObject();
        expectedJson.put("message", "Get users database error message");
        assertEquals(expectedJson.getString("message"), Objects.requireNonNull(response.getBody()).getMessage());
    }
}
