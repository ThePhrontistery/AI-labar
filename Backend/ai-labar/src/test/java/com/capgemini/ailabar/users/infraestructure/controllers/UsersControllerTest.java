package com.capgemini.ailabar.users.infraestructure.controllers;

import com.capgemini.ailabar.commons.utils.RSAKeyPairGeneratorService;
import com.capgemini.ailabar.commons.utils.SpecialResponse;
import com.capgemini.ailabar.users.application.services.UsersService;
import com.capgemini.ailabar.users.application.usecases.LoginUseCaseImpl;
import com.capgemini.ailabar.users.domain.exceptions.*;
import com.capgemini.ailabar.users.domain.models.UsersModel;
import com.capgemini.ailabar.users.domain.ports.out.UsersRepositoryPort;
import com.capgemini.ailabar.users.infraestructure.entities.UsersEntity;
import org.apache.commons.codec.digest.DigestUtils;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.core.env.Environment;
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

    @Mock
    private UsersRepositoryPort usersRepositoryPort;

    @Mock
    private Environment environment;

    @Mock
    private RSAKeyPairGeneratorService rsaService;

    @InjectMocks
    private LoginUseCaseImpl loginUseCase;

    @Test
    void testLoginSuccess() {
        UsersModel usersModel = new UsersModel();
        usersModel.setUser("testUser");
        usersModel.setPassword("testPassword");

        UsersEntity usersEntity = new UsersEntity();
        usersEntity.setToken("validToken");
        usersEntity.setVisualization("visualization");
        usersEntity.setLanguage("language");
        usersEntity.setPhoto("photo");

        when(environment.getProperty("login.cap.active")).thenReturn("false");
        when(usersRepositoryPort.login(usersModel.getUser(), DigestUtils.sha256Hex(usersModel.getPassword()))).thenReturn(true);
        when(usersRepositoryPort.getUserByName(usersModel.getUser())).thenReturn(usersEntity);

        List<String> result = loginUseCase.login(usersModel, rsaService.getPrivateKey());

        assertEquals(4, result.size());
        assertEquals("validToken", result.get(0));
        assertEquals("visualization", result.get(1));
        assertEquals("language", result.get(2));
        assertEquals("photo", result.get(3));

        verify(usersRepositoryPort, times(1)).login(usersModel.getUser(), DigestUtils.sha256Hex(usersModel.getPassword()));
        verify(usersRepositoryPort, times(1)).getUserByName(usersModel.getUser());
        verifyNoMoreInteractions(usersRepositoryPort);
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
    void testEditVisualizationSuccess() {
        UsersModel usersModel = new UsersModel();
        usersModel.setUser("existingUser");
        usersModel.setToken("validToken");
        usersModel.setVisualization("newVisualization");

        doNothing().when(usersService).editVisualization(usersModel);

        String expectedMessage = "Visualization edited successfully";

        ResponseEntity<SpecialResponse> actualResponse = usersController.editVisualization(usersModel);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        JSONObject expectedJson = new JSONObject();
        expectedJson.put("message", expectedMessage);
        assertEquals(expectedJson.getString("message"), Objects.requireNonNull(actualResponse.getBody()).getMessage());

        verify(usersService, times(1)).editVisualization(usersModel);
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
        UsersModel user1 = new UsersModel();
        user1.setId(1);
        user1.setUser("User1");

        UsersModel user2 = new UsersModel();
        user2.setId(2);
        user2.setUser("User2");

        List<UsersModel> usersModelList = new ArrayList<>();
        usersModelList.add(user1);
        usersModelList.add(user2);

        when(usersService.getUsersDatabase()).thenReturn(usersModelList);

        String expectedMessage = "OK";

        ResponseEntity<SpecialResponse> actualResponse = usersController.getUsersDatabase();

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        SpecialResponse specialResponse = actualResponse.getBody();
        assertNotNull(specialResponse);
        assertEquals(expectedMessage, specialResponse.getMessage());
        assertEquals(usersModelList, specialResponse.getEntity());

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
    void testHandlerEditVisualizationException() {
        EditVisualizationException exception = new EditVisualizationException("Edit visualization error message");

        ResponseEntity<SpecialResponse> response = usersController.handlerEditVisualizationException(exception);

        assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
        JSONObject expectedJson = new JSONObject();
        expectedJson.put("message", "Edit visualization error message");
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
