package com.capgemini.ailabar.infrastructure.controller;

import com.capgemini.ailabar.application.service.UsersService;
import com.capgemini.ailabar.infrastructure.utils.SpecialResponse;
import com.capgemini.ailabar.domain.model.UsersModel;
import com.capgemini.ailabar.infrastructure.entity.UsersEntity;
import org.apache.commons.codec.digest.DigestUtils;
import org.json.JSONObject;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;

@ExtendWith(SpringExtension.class)
@SpringBootTest
class UsersControllerTest {
    @Mock
    private UsersService usersService;

    @InjectMocks
    private UsersController usersController;

    @BeforeEach
    void setUp() {
        Mockito.reset(usersService);
    }

    @Test
    void testCreateUser_DataMissing_ReturnsBadRequest() {
        UsersModel userModel = new UsersModel();
        userModel.setUser("");
        userModel.setPassword("examplePassword");
        userModel.setEmail("example@example.com");
        userModel.setGender("H");
        userModel.setPhoto("photo");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "All data is required to create a new user");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(usersController.specialResponse(null, expectedResponseJson), HttpStatus.BAD_GATEWAY);

        ResponseEntity<SpecialResponse> actualResponse = usersController.createUser(userModel);

        assertEquals(HttpStatus.BAD_GATEWAY, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verifyNoInteractions(usersService);
    }

    @Test
    void testCreateUser_UserAlreadyExists_ReturnsUserAlreadyExists() {
        UsersModel userModel = new UsersModel();
        userModel.setUser("exampleUser");
        userModel.setPassword("examplePassword");
        userModel.setEmail("example@example.com");
        userModel.setGender("H");
        userModel.setPhoto("photo");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "The user already exists");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(usersController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkUser(userModel.getUser())).thenReturn(true);

        ResponseEntity<SpecialResponse> actualResponse = usersController.createUser(userModel);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkUser(userModel.getUser());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testCreateUser_EmailAlreadyExists_ReturnsEmailAlreadyExists() {
        UsersModel userModel = new UsersModel();
        userModel.setUser("exampleUser");
        userModel.setPassword("examplePassword");
        userModel.setEmail("example@example.com");
        userModel.setGender("H");
        userModel.setPhoto("photo");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "The email already exists");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(usersController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkUser(userModel.getUser())).thenReturn(false);
        when(usersService.existsByEmail(userModel.getEmail())).thenReturn(true);

        ResponseEntity<SpecialResponse> actualResponse = usersController.createUser(userModel);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkUser(userModel.getUser());
        verify(usersService, times(1)).existsByEmail(userModel.getEmail());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void createUser_UserNotFound_ReturnsNotFoundResponse() {
        UsersModel userModel = new UsersModel();
        userModel.setUser("testUser");
        userModel.setPassword("testPassword");
        userModel.setEmail("test@example.com");
        userModel.setGender("H");
        userModel.setPhoto("photo");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "User not found");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(usersController.specialResponse(null, expectedResponseJson), HttpStatus.NOT_FOUND);

        when(usersService.checkUser(userModel.getUser())).thenReturn(false);
        when(usersService.existsByEmail(userModel.getEmail())).thenReturn(false);
        when(usersService.findByUser(userModel.getUser())).thenReturn(null);

        ResponseEntity<SpecialResponse> actualResponse = usersController.createUser(userModel);

        assertEquals(HttpStatus.NOT_FOUND, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkUser(userModel.getUser());
        verify(usersService, times(1)).existsByEmail(userModel.getEmail());
        verify(usersService, times(1)).findByUser(userModel.getUser());
    }

    @Test
    void testCreateUser_AllDataProvided_UserCreatedSuccessfully() {
        UsersModel userModel = new UsersModel();
        userModel.setUser("exampleUser");
        userModel.setPassword("examplePassword");
        userModel.setEmail("example@example.com");
        userModel.setGender("H");
        userModel.setPhoto("photo");

        UsersEntity userEntity = new UsersEntity();
        userEntity.setId(1);
        userEntity.setUser("exampleUser");
        userEntity.setPassword(DigestUtils.sha256Hex("examplePassword"));
        userEntity.setEmail("example@example.com");
        userEntity.setGender("H");
        userEntity.setPhoto("photo");
        userEntity.setToken("");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "User created successfully");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(usersController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkUser(userModel.getUser())).thenReturn(false);
        when(usersService.existsByEmail(userModel.getEmail())).thenReturn(false);
        when(usersService.findByUser(userModel.getUser())).thenReturn(userEntity);
        doNothing().when(usersService).saveUser(any(UsersEntity.class));

        ResponseEntity<SpecialResponse> actualResponse = usersController.createUser(userModel);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkUser(userModel.getUser());
        verify(usersService, times(1)).existsByEmail(userModel.getEmail());
        verify(usersService, times(1)).findByUser(userModel.getUser());
        verify(usersService, times(2)).saveUser(any(UsersEntity.class));
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testEditUser_DataMissing_ReturnsBadRequest() {
        UsersModel userModel = new UsersModel();
        userModel.setUser("");
        userModel.setPassword("");
        userModel.setToken("");
        userModel.setGender("");
        userModel.setPhoto("");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "All data is required to edit a user");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(usersController.specialResponse(null, expectedResponseJson), HttpStatus.BAD_GATEWAY);

        UsersService usersService = mock(UsersService.class);
        ResponseEntity<SpecialResponse> actualResponse = usersController.editUser(userModel);

        assertEquals(HttpStatus.BAD_GATEWAY, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testEditUser_InvalidToken_ReturnsNotFound() {
        UsersModel userModel = new UsersModel();
        userModel.setUser("existingUser");
        userModel.setPassword("existingPassword");
        userModel.setToken("invalidToken");
        userModel.setGender("H");
        userModel.setPhoto("photo");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "Unauthorized user");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(usersController.specialResponse(null, expectedResponseJson), HttpStatus.NOT_FOUND);

        when(usersService.checkToken(userModel.getUser(), userModel.getToken())).thenReturn(false);

        ResponseEntity<SpecialResponse> actualResponse = usersController.editUser(userModel);

        assertEquals(HttpStatus.NOT_FOUND, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(userModel.getUser(), userModel.getToken());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testEditUser_NoValuesToUpdate_ReturnsBadRequest() {
        UsersModel userModel = new UsersModel();
        userModel.setUser("existingUser");
        userModel.setPassword("existingPassword");
        userModel.setToken("validToken");
        userModel.setGender("H");
        userModel.setPhoto("photo");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "There are no values to update.");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(usersController.specialResponse(null, expectedResponseJson), HttpStatus.BAD_GATEWAY);

        when(usersService.checkToken(userModel.getUser(), userModel.getToken())).thenReturn(true);

        ResponseEntity<SpecialResponse> actualResponse = usersController.editUser(userModel);

        verify(usersService, times(1)).checkToken(userModel.getUser(), userModel.getToken());

        assertEquals(HttpStatus.BAD_GATEWAY, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testEditUser_NewUsernameAlreadyExists_ReturnsBadRequest() {
        UsersModel userModel = new UsersModel();
        userModel.setUser("existingUser");
        userModel.setPassword("existingPassword");
        userModel.setToken("validToken");
        userModel.setNewUser("existingUser2");
        userModel.setGender("H");
        userModel.setPhoto("photo");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "The new username already exists");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(usersController.specialResponse(null, expectedResponseJson), HttpStatus.BAD_REQUEST);

        when(usersService.checkToken(userModel.getUser(), userModel.getToken())).thenReturn(true);
        when(usersService.checkUser(userModel.getNewUser().strip())).thenReturn(true);

        ResponseEntity<SpecialResponse> actualResponse = usersController.editUser(userModel);

        assertEquals(HttpStatus.BAD_REQUEST, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(userModel.getUser(), userModel.getToken());
        verify(usersService, times(1)).checkUser(userModel.getNewUser());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testEditUser_AllDataProvided_UserModifiedSuccessfully() {
        UsersModel userModel = new UsersModel();
        userModel.setUser("existingUser");
        userModel.setPassword("existingPassword");
        userModel.setToken("validToken");
        userModel.setNewUser("newUser");
        userModel.setNewPassword("newPassword");
        userModel.setGender("H");
        userModel.setPhoto("photo");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "User modified successfully");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(usersController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        UsersEntity userEntity = new UsersEntity();
        userEntity.setUser(userModel.getUser());
        userEntity.setPassword(userModel.getPassword());

        when(usersService.checkToken(userModel.getUser(), userModel.getToken())).thenReturn(true);
        when(usersService.checkUser(userModel.getNewUser().strip())).thenReturn(false);
        when(usersService.findByUser(userModel.getUser())).thenReturn(userEntity);
        doNothing().when(usersService).saveUser(any(UsersEntity.class));

        ResponseEntity<SpecialResponse> actualResponse = usersController.editUser(userModel);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(userModel.getUser(), userModel.getToken());
        verify(usersService, times(1)).checkUser(userModel.getNewUser());
        verify(usersService, times(1)).findByUser(userModel.getUser());
        verify(usersService, times(1)).saveUser(any(UsersEntity.class));
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testDeleteUser_UserMissing_ReturnsBadRequest() {
        UsersModel userModel = new UsersModel();
        userModel.setUser("");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "User name and token are required to delete a user");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(usersController.specialResponse(null, expectedResponseJson), HttpStatus.BAD_GATEWAY);

        ResponseEntity<SpecialResponse> actualResponse = usersController.deleteUser(userModel);

        assertEquals(HttpStatus.BAD_GATEWAY, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testDeleteUser_TokenMismatch_ReturnsNotFound() {
        UsersModel userModel = new UsersModel();
        userModel.setUser("existingUser");
        userModel.setToken("invalidToken");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "Unauthorized user");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(usersController.specialResponse(null, expectedResponseJson), HttpStatus.NOT_FOUND);

        when(usersService.checkToken(userModel.getUser(), userModel.getToken())).thenReturn(false);

        ResponseEntity<SpecialResponse> actualResponse = usersController.deleteUser(userModel);

        assertEquals(HttpStatus.NOT_FOUND, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(userModel.getUser(), userModel.getToken());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testDeleteUser_UserDeletedSuccessfully() {
        UsersModel userModel = new UsersModel();
        userModel.setUser("existingUser");
        userModel.setToken("validToken");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "User deleted successfully");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(usersController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(userModel.getUser(), userModel.getToken())).thenReturn(true);
        doNothing().when(usersService).deleteUser(userModel.getUser());

        ResponseEntity<SpecialResponse> actualResponse = usersController.deleteUser(userModel);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(userModel.getUser(), userModel.getToken());
        verify(usersService, times(1)).deleteUser(userModel.getUser());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testGetUsers_MissingFields() {
        UsersModel userModel = new UsersModel();
        userModel.setUser("");
        userModel.setMatcher("");
        userModel.setToken("");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "User and token are required");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(usersController.specialResponse(null, expectedResponseJson), HttpStatus.BAD_GATEWAY);

        ResponseEntity<SpecialResponse> actualResponse = usersController.getUsers(userModel);

        assertEquals(HttpStatus.BAD_GATEWAY, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verifyNoInteractions(usersService);
    }

    @Test
    void testGetUsers_UnauthorizedUser() {
        UsersModel userModel = new UsersModel();
        userModel.setUser("exampleUser");
        userModel.setMatcher("exampleMatcher");
        userModel.setToken("exampleToken");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "Unauthorized user");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(usersController.specialResponse(null, expectedResponseJson), HttpStatus.NOT_FOUND);

        when(usersService.checkToken(userModel.getUser(), userModel.getToken())).thenReturn(false);

        ResponseEntity<SpecialResponse> actualResponse = usersController.getUsers(userModel);

        assertEquals(HttpStatus.NOT_FOUND, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(userModel.getUser(), userModel.getToken());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testGetUsers_NoMatchesFound() {
        UsersModel userModel = new UsersModel();
        userModel.setUser("exampleUser");
        userModel.setMatcher("exampleMatcher");
        userModel.setToken("exampleToken");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "Not matches");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(usersController.specialResponse(null, expectedResponseJson), HttpStatus.NOT_FOUND);

        when(usersService.checkToken(userModel.getUser(), userModel.getToken())).thenReturn(true);
        when(usersService.userMatches(userModel.getMatcher())).thenReturn(null);

        ResponseEntity<SpecialResponse> actualResponse = usersController.getUsers(userModel);

        assertEquals(HttpStatus.NOT_FOUND, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(userModel.getUser(), userModel.getToken());
        verify(usersService, times(1)).userMatches(userModel.getMatcher());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testGetUsers_ValidUser_MatchesFound() {
        UsersModel userModel = new UsersModel();
        userModel.setUser("exampleUser");
        userModel.setMatcher("exampleMatcher");
        userModel.setToken("exampleToken");

        List<String> userMatchesList = Arrays.asList("Match1", "Match2");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "2 matches");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(usersController.specialResponse(userMatchesList, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(userModel.getUser(), userModel.getToken())).thenReturn(true);
        when(usersService.userMatches(userModel.getMatcher())).thenReturn(userMatchesList);

        ResponseEntity<SpecialResponse> actualResponse = usersController.getUsers(userModel);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(userModel.getUser(), userModel.getToken());
        verify(usersService, times(1)).userMatches(userModel.getMatcher());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testGetAllUsers_InsufficientData() {
        UsersModel userModel = new UsersModel();
        userModel.setUser("john");
        userModel.setToken("");

        ResponseEntity<SpecialResponse> response = usersController.getAllUsers(userModel);

        assertEquals(HttpStatus.BAD_GATEWAY, response.getStatusCode());
        assertEquals("User name and token are required to delete a user", Objects.requireNonNull(response.getBody()).getMessage());
        verifyNoInteractions(usersService);
    }

    @Test
    void testGetAllUsers_UnauthorizedUser() {
        UsersModel userModel = new UsersModel();
        userModel.setUser("john");
        userModel.setToken("invalidToken");

        when(usersService.checkToken(userModel.getUser(), userModel.getToken())).thenReturn(false);

        ResponseEntity<SpecialResponse> response = usersController.getAllUsers(userModel);

        assertEquals(HttpStatus.NOT_FOUND, response.getStatusCode());
        assertEquals("Unauthorized user", Objects.requireNonNull(response.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(userModel.getUser(), userModel.getToken());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testGetAllUsers_Success() {
        UsersModel userModel = new UsersModel();
        userModel.setUser("john");
        userModel.setToken("validToken");

        List<String> expectedUsersList = Arrays.asList("user1", "user2");

        when(usersService.checkToken(userModel.getUser(), userModel.getToken())).thenReturn(true);
        when(usersService.getAllUsers()).thenReturn(expectedUsersList);

        ResponseEntity<SpecialResponse> response = usersController.getAllUsers(userModel);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertEquals("List of users obtained successfully", Objects.requireNonNull(response.getBody()).getMessage());

        verify(usersService, times(1)).checkToken(userModel.getUser(), userModel.getToken());
        verify(usersService, times(1)).getAllUsers();
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testGetAllUsersData_NoUsersInDatabase_ReturnsEmptyList() {
        List<UsersEntity> usersList = new ArrayList<>();

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "There are no users in database");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(usersController.specialResponse(usersList, expectedResponseJson), HttpStatus.OK);

        when(usersService.getAllUsersData()).thenReturn(usersList);

        ResponseEntity<SpecialResponse> actualResponse = usersController.getAllUsersDataAll();

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).getAllUsersData();
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testGetAllUsersData_UsersInDatabase_ReturnsUserList() {
        List<UsersEntity> usersList = new ArrayList<>();

        UsersEntity usersEntity = new UsersEntity();
        usersEntity.setId(1);
        usersEntity.setUser("user");
        usersEntity.setPassword("pass");

        usersList.add(usersEntity);

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "OK");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(usersController.specialResponse(usersList, expectedResponseJson), HttpStatus.OK);

        when(usersService.getAllUsersData()).thenReturn(usersList);

        ResponseEntity<SpecialResponse> actualResponse = usersController.getAllUsersDataAll();

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).getAllUsersData();
        verifyNoMoreInteractions(usersService);
    }


}

