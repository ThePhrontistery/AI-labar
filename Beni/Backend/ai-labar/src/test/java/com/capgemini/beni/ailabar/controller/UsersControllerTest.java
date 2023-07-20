package com.capgemini.beni.ailabar.controller;

import com.capgemini.beni.ailabar.dto.UsersDto;
import com.capgemini.beni.ailabar.entity.UsersEntity;
import com.capgemini.beni.ailabar.service.UsersService;
import com.capgemini.beni.ailabar.utils.SpecialResponse;
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
        UsersDto userDto = new UsersDto();
        userDto.setUser("");
        userDto.setPassword("examplePassword");
        userDto.setEmail("example@example.com");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "All data is required to create a new user");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(usersController.specialResponse(null, expectedResponseJson), HttpStatus.BAD_GATEWAY);

        ResponseEntity<SpecialResponse> actualResponse = usersController.createUser(userDto);

        assertEquals(HttpStatus.BAD_GATEWAY, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verifyNoInteractions(usersService);
    }

    @Test
    void testCreateUser_UserAlreadyExists_ReturnsUserAlreadyExists() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("exampleUser");
        userDto.setPassword("examplePassword");
        userDto.setEmail("example@example.com");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "The user already exists");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(usersController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkUser(userDto.getUser())).thenReturn(true);

        ResponseEntity<SpecialResponse> actualResponse = usersController.createUser(userDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkUser(userDto.getUser());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testCreateUser_EmailAlreadyExists_ReturnsEmailAlreadyExists() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("exampleUser");
        userDto.setPassword("examplePassword");
        userDto.setEmail("example@example.com");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "The email already exists");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(usersController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkUser(userDto.getUser())).thenReturn(false);
        when(usersService.existsByEmail(userDto.getEmail())).thenReturn(true);

        ResponseEntity<SpecialResponse> actualResponse = usersController.createUser(userDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkUser(userDto.getUser());
        verify(usersService, times(1)).existsByEmail(userDto.getEmail());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void createUser_UserNotFound_ReturnsNotFoundResponse() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("testUser");
        userDto.setPassword("testPassword");
        userDto.setEmail("test@example.com");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "User not found");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(usersController.specialResponse(null, expectedResponseJson), HttpStatus.NOT_FOUND);

        when(usersService.checkUser(userDto.getUser())).thenReturn(false);
        when(usersService.existsByEmail(userDto.getEmail())).thenReturn(false);
        when(usersService.findByUser(userDto.getUser())).thenReturn(null);

        ResponseEntity<SpecialResponse> actualResponse = usersController.createUser(userDto);

        assertEquals(HttpStatus.NOT_FOUND, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkUser(userDto.getUser());
        verify(usersService, times(1)).existsByEmail(userDto.getEmail());
        verify(usersService, times(1)).findByUser(userDto.getUser());
    }

    @Test
    void testCreateUser_AllDataProvided_UserCreatedSuccessfully() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("exampleUser");
        userDto.setPassword("examplePassword");
        userDto.setEmail("example@example.com");

        UsersEntity userEntity = new UsersEntity();
        userEntity.setId(1);
        userEntity.setUser("exampleUser");
        userEntity.setPassword(DigestUtils.sha256Hex("examplePassword"));
        userEntity.setEmail("example@example.com");
        userEntity.setToken("");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "User created successfully");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(usersController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkUser(userDto.getUser())).thenReturn(false);
        when(usersService.existsByEmail(userDto.getEmail())).thenReturn(false);
        when(usersService.findByUser(userDto.getUser())).thenReturn(userEntity);
        doNothing().when(usersService).saveUser(any(UsersEntity.class));

        ResponseEntity<SpecialResponse> actualResponse = usersController.createUser(userDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkUser(userDto.getUser());
        verify(usersService, times(1)).existsByEmail(userDto.getEmail());
        verify(usersService, times(1)).findByUser(userDto.getUser());
        verify(usersService, times(2)).saveUser(any(UsersEntity.class));
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testEditUser_DataMissing_ReturnsBadRequest() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("");
        userDto.setPassword("");
        userDto.setToken("");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "All data is required to edit a user");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(usersController.specialResponse(null, expectedResponseJson), HttpStatus.BAD_GATEWAY);

        UsersService usersService = mock(UsersService.class);
        ResponseEntity<SpecialResponse> actualResponse = usersController.editUser(userDto);

        assertEquals(HttpStatus.BAD_GATEWAY, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testEditUser_InvalidToken_ReturnsNotFound() {
        // Arrange
        UsersDto userDto = new UsersDto();
        userDto.setUser("existingUser");
        userDto.setPassword("existingPassword");
        userDto.setToken("invalidToken");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "Unauthorized user");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(usersController.specialResponse(null, expectedResponseJson), HttpStatus.NOT_FOUND);

        when(usersService.checkToken(userDto.getUser(), userDto.getToken())).thenReturn(false);

        ResponseEntity<SpecialResponse> actualResponse = usersController.editUser(userDto);

        assertEquals(HttpStatus.NOT_FOUND, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(userDto.getUser(), userDto.getToken());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testEditUser_NoValuesToUpdate_ReturnsBadRequest() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("existingUser");
        userDto.setPassword("existingPassword");
        userDto.setToken("validToken");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "There are no values to update.");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(usersController.specialResponse(null, expectedResponseJson), HttpStatus.BAD_GATEWAY);

        when(usersService.checkToken(userDto.getUser(), userDto.getToken())).thenReturn(true);

        ResponseEntity<SpecialResponse> actualResponse = usersController.editUser(userDto);

        verify(usersService, times(1)).checkToken(userDto.getUser(), userDto.getToken());

        assertEquals(HttpStatus.BAD_GATEWAY, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testEditUser_NewUsernameAlreadyExists_ReturnsBadRequest() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("existingUser");
        userDto.setPassword("existingPassword");
        userDto.setToken("validToken");
        userDto.setNewUser("existingUser2");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "The new username already exists");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(usersController.specialResponse(null, expectedResponseJson), HttpStatus.BAD_REQUEST);

        when(usersService.checkToken(userDto.getUser(), userDto.getToken())).thenReturn(true);
        when(usersService.checkUser(userDto.getNewUser().strip())).thenReturn(true);

        ResponseEntity<SpecialResponse> actualResponse = usersController.editUser(userDto);

        assertEquals(HttpStatus.BAD_REQUEST, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(userDto.getUser(), userDto.getToken());
        verify(usersService, times(1)).checkUser(userDto.getNewUser());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testEditUser_AllDataProvided_UserModifiedSuccessfully() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("existingUser");
        userDto.setPassword("existingPassword");
        userDto.setToken("validToken");
        userDto.setNewUser("newUser");
        userDto.setNewPassword("newPassword");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "User modified successfully");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(usersController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        UsersEntity userEntity = new UsersEntity();
        userEntity.setUser(userDto.getUser());
        userEntity.setPassword(userDto.getPassword());

        when(usersService.checkToken(userDto.getUser(), userDto.getToken())).thenReturn(true);
        when(usersService.checkUser(userDto.getNewUser().strip())).thenReturn(false);
        when(usersService.findByUser(userDto.getUser())).thenReturn(userEntity);
        doNothing().when(usersService).saveUser(any(UsersEntity.class));

        ResponseEntity<SpecialResponse> actualResponse = usersController.editUser(userDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(userDto.getUser(), userDto.getToken());
        verify(usersService, times(1)).checkUser(userDto.getNewUser());
        verify(usersService, times(1)).findByUser(userDto.getUser());
        verify(usersService, times(1)).saveUser(any(UsersEntity.class));
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testDeleteUser_UserMissing_ReturnsBadRequest() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "User name and token are required to delete a user");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(usersController.specialResponse(null, expectedResponseJson), HttpStatus.BAD_GATEWAY);

        ResponseEntity<SpecialResponse> actualResponse = usersController.deleteUser(userDto);

        assertEquals(HttpStatus.BAD_GATEWAY, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testDeleteUser_TokenMismatch_ReturnsNotFound() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("existingUser");
        userDto.setToken("invalidToken");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "Unauthorized user");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(usersController.specialResponse(null, expectedResponseJson), HttpStatus.NOT_FOUND);

        when(usersService.checkToken(userDto.getUser(), userDto.getToken())).thenReturn(false);

        ResponseEntity<SpecialResponse> actualResponse = usersController.deleteUser(userDto);

        assertEquals(HttpStatus.NOT_FOUND, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(userDto.getUser(), userDto.getToken());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testDeleteUser_UserDeletedSuccessfully() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("existingUser");
        userDto.setToken("validToken");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "User deleted successfully");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(usersController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(userDto.getUser(), userDto.getToken())).thenReturn(true);
        doNothing().when(usersService).deleteUser(userDto.getUser());

        ResponseEntity<SpecialResponse> actualResponse = usersController.deleteUser(userDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(userDto.getUser(), userDto.getToken());
        verify(usersService, times(1)).deleteUser(userDto.getUser());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testGetUsers_MissingFields() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("");
        userDto.setMatcher("");
        userDto.setToken("");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "User and token are required");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(usersController.specialResponse(null, expectedResponseJson), HttpStatus.BAD_GATEWAY);

        ResponseEntity<SpecialResponse> actualResponse = usersController.getUsers(userDto);

        assertEquals(HttpStatus.BAD_GATEWAY, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verifyNoInteractions(usersService);
    }

    @Test
    void testGetUsers_UnauthorizedUser() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("exampleUser");
        userDto.setMatcher("exampleMatcher");
        userDto.setToken("exampleToken");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "Unauthorized user");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(usersController.specialResponse(null, expectedResponseJson), HttpStatus.NOT_FOUND);

        when(usersService.checkToken(userDto.getUser(), userDto.getToken())).thenReturn(false);

        ResponseEntity<SpecialResponse> actualResponse = usersController.getUsers(userDto);

        assertEquals(HttpStatus.NOT_FOUND, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(userDto.getUser(), userDto.getToken());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testGetUsers_NoMatchesFound() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("exampleUser");
        userDto.setMatcher("exampleMatcher");
        userDto.setToken("exampleToken");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "Not matches");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(usersController.specialResponse(null, expectedResponseJson), HttpStatus.NOT_FOUND);

        when(usersService.checkToken(userDto.getUser(), userDto.getToken())).thenReturn(true);
        when(usersService.userMatches(userDto.getMatcher())).thenReturn(null);

        ResponseEntity<SpecialResponse> actualResponse = usersController.getUsers(userDto);

        assertEquals(HttpStatus.NOT_FOUND, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(userDto.getUser(), userDto.getToken());
        verify(usersService, times(1)).userMatches(userDto.getMatcher());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testGetUsers_ValidUser_MatchesFound() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("exampleUser");
        userDto.setMatcher("exampleMatcher");
        userDto.setToken("exampleToken");

        List<String> userMatchesList = Arrays.asList("Match1", "Match2");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "2 matches");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(usersController.specialResponse(userMatchesList, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(userDto.getUser(), userDto.getToken())).thenReturn(true);
        when(usersService.userMatches(userDto.getMatcher())).thenReturn(userMatchesList);

        ResponseEntity<SpecialResponse> actualResponse = usersController.getUsers(userDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(userDto.getUser(), userDto.getToken());
        verify(usersService, times(1)).userMatches(userDto.getMatcher());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testGetAllUsers_InsufficientData() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("john");
        userDto.setToken("");

        ResponseEntity<SpecialResponse> response = usersController.getAllUsers(userDto);

        assertEquals(HttpStatus.BAD_GATEWAY, response.getStatusCode());
        assertEquals("User name and token are required to delete a user", Objects.requireNonNull(response.getBody()).getMessage());
        verifyNoInteractions(usersService);
    }

    @Test
    void testGetAllUsers_UnauthorizedUser() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("john");
        userDto.setToken("invalidToken");

        when(usersService.checkToken(userDto.getUser(), userDto.getToken())).thenReturn(false);

        ResponseEntity<SpecialResponse> response = usersController.getAllUsers(userDto);

        assertEquals(HttpStatus.NOT_FOUND, response.getStatusCode());
        assertEquals("Unauthorized user", Objects.requireNonNull(response.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(userDto.getUser(), userDto.getToken());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testGetAllUsers_Success() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("john");
        userDto.setToken("validToken");

        List<String> expectedUsersList = Arrays.asList("user1", "user2");

        when(usersService.checkToken(userDto.getUser(), userDto.getToken())).thenReturn(true);
        when(usersService.getAllUsers()).thenReturn(expectedUsersList);

        ResponseEntity<SpecialResponse> response = usersController.getAllUsers(userDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertEquals("List of users obtained successfully", Objects.requireNonNull(response.getBody()).getMessage());

        verify(usersService, times(1)).checkToken(userDto.getUser(), userDto.getToken());
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

