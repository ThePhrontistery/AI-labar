package com.capgemini.beni.ailabar.controller;

import com.capgemini.beni.ailabar.dto.UsersDto;
import com.capgemini.beni.ailabar.entity.UsersEntity;
import com.capgemini.beni.ailabar.service.UsersService;
import com.capgemini.beni.ailabar.utils.SpecialResponse;
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
import java.util.List;

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
    void testCreateUser_AllDataRequired() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("");
        userDto.setPassword("");
        userDto.setEmail("");

        JSONObject expectedJsonResponse = new JSONObject();
        expectedJsonResponse.put("message", "All data is required to create a new user");
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(expectedJsonResponse.toString(), HttpStatus.BAD_GATEWAY);

        ResponseEntity<String> response = usersController.createUser(userDto);

        assertEquals(HttpStatus.BAD_GATEWAY, response.getStatusCode());
        assertEquals(expectedResponse.getBody(), response.getBody());
        verifyNoInteractions(usersService);
    }

    @Test
    void testCreateUser_UserAlreadyExists() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("existingUser");
        userDto.setPassword("password");
        userDto.setEmail("email@example.com");

        JSONObject expectedJsonResponse = new JSONObject();
        expectedJsonResponse.put("message", "The user already exists");
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(expectedJsonResponse.toString(), HttpStatus.OK);

        when(usersService.checkUser(userDto.getUser())).thenReturn(true);

        ResponseEntity<String> response = usersController.createUser(userDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertEquals(expectedResponse.getBody(), response.getBody());
        verify(usersService, times(1)).checkUser(userDto.getUser());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testCreateUser_EmailAlreadyExists() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("newUser");
        userDto.setPassword("password");
        userDto.setEmail("existing@example.com");

        JSONObject expectedJsonResponse = new JSONObject();
        expectedJsonResponse.put("message", "The email already exists");
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(expectedJsonResponse.toString(), HttpStatus.OK);

        when(usersService.checkUser(userDto.getUser())).thenReturn(false);
        when(usersService.existsByEmail(userDto.getEmail())).thenReturn(true);

        ResponseEntity<String> response = usersController.createUser(userDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertEquals(expectedResponse.getBody(), response.getBody());
        verify(usersService, times(1)).checkUser(userDto.getUser());
        verify(usersService, times(1)).existsByEmail(userDto.getEmail());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testCreateUser_Success() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("newUser");
        userDto.setPassword("password");
        userDto.setEmail("newuser@example.com");
        userDto.setToken("token");

        when(usersService.checkUser(userDto.getUser())).thenReturn(false);
        when(usersService.existsByEmail(userDto.getEmail())).thenReturn(false);
        doNothing().when(usersService).saveUser(any(UsersEntity.class));

        JSONObject expectedJsonResponse = new JSONObject();
        expectedJsonResponse.put("message", "User created successfully");
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(expectedJsonResponse.toString(), HttpStatus.OK);

        ResponseEntity<String> response = usersController.createUser(userDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertEquals(expectedResponse.getBody(), response.getBody());
        verify(usersService, times(1)).checkUser(userDto.getUser());
        verify(usersService, times(1)).existsByEmail(userDto.getEmail());
        verify(usersService, times(1)).saveUser(any(UsersEntity.class));
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testEditUser_Successful() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("existingUser");
        userDto.setPassword("password");
        userDto.setToken("token");
        userDto.setNewUser("newUser");
        userDto.setNewPassword("newPassword");

        UsersEntity existingUserEntity = new UsersEntity();
        existingUserEntity.setUser("existingUser");
        existingUserEntity.setPassword("hashedPassword");
        existingUserEntity.setEmail("email");
        existingUserEntity.setToken("token");

        when(usersService.checkUser(userDto.getUser())).thenReturn(true);
        when(usersService.checkUser(userDto.getNewUser().strip())).thenReturn(false);
        when(usersService.checkToken(userDto.getUser(), userDto.getToken())).thenReturn(true);
        when(usersService.findByUser(userDto.getUser())).thenReturn(existingUserEntity);

        JSONObject expectedJsonResponse = new JSONObject();
        expectedJsonResponse.put("message", "User modified successfully");
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(expectedJsonResponse.toString(), HttpStatus.OK);

        ResponseEntity<String> response = usersController.editUser(userDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertEquals(expectedResponse.getBody(), response.getBody());
        verify(usersService, times(1)).checkUser(userDto.getUser());
        verify(usersService, times(1)).checkUser(userDto.getNewUser().strip());
        verify(usersService, times(1)).checkToken(userDto.getUser(), userDto.getToken());
        verify(usersService, times(1)).findByUser(userDto.getUser());
        verify(usersService, times(1)).saveUser(existingUserEntity);
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testEditUser_BlankUserData() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("");
        userDto.setPassword("");
        userDto.setToken("");
        userDto.setNewUser("");
        userDto.setNewPassword("");

        JSONObject expectedJsonResponse = new JSONObject();
        expectedJsonResponse.put("message", "All data is required to edit a user");
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(expectedJsonResponse.toString(), HttpStatus.BAD_GATEWAY);

        ResponseEntity<String> response = usersController.editUser(userDto);

        assertEquals(HttpStatus.BAD_GATEWAY, response.getStatusCode());
        assertEquals(expectedResponse.getBody(), response.getBody());
        verifyNoInteractions(usersService);
    }

    @Test
    void testEditUser_NoValuesToUpdate() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("existingUser");
        userDto.setPassword("password");
        userDto.setToken("token");
        userDto.setNewUser("");
        userDto.setNewPassword("");

        JSONObject expectedJsonResponse = new JSONObject();
        expectedJsonResponse.put("message", "There are no values to update.");
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(expectedJsonResponse.toString(), HttpStatus.BAD_GATEWAY);

        ResponseEntity<String> response = usersController.editUser(userDto);

        assertEquals(HttpStatus.BAD_GATEWAY, response.getStatusCode());
        assertEquals(expectedResponse.getBody(), response.getBody());
        verifyNoInteractions(usersService);
    }

    @Test
    void testEditUser_UserNotFound() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("nonExistingUser");
        userDto.setPassword("password");
        userDto.setToken("token");
        userDto.setNewUser("newUser");
        userDto.setNewPassword("");

        when(usersService.checkUser(userDto.getUser())).thenReturn(false);

        JSONObject expectedJsonResponse = new JSONObject();
        expectedJsonResponse.put("message", "The user does not exist");
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(expectedJsonResponse.toString(), HttpStatus.BAD_REQUEST);

        ResponseEntity<String> response = usersController.editUser(userDto);

        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
        assertEquals(expectedResponse.getBody(), response.getBody());
        verify(usersService, times(1)).checkUser(userDto.getUser());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testEditUser_NewUsernameAlreadyExists() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("existingUser");
        userDto.setPassword("password");
        userDto.setToken("token");
        userDto.setNewUser("existingUser");
        userDto.setNewPassword("");

        when(usersService.checkUser(userDto.getUser())).thenReturn(true);
        when(usersService.checkUser(userDto.getNewUser().strip())).thenReturn(true);

        JSONObject expectedJsonResponse = new JSONObject();
        expectedJsonResponse.put("message", "The new username already exists");
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(expectedJsonResponse.toString(), HttpStatus.BAD_REQUEST);

        ResponseEntity<String> response = usersController.editUser(userDto);

        assertEquals(HttpStatus.BAD_REQUEST, response.getStatusCode());
        assertEquals(expectedResponse.getBody(), response.getBody());
        verify(usersService, times(2)).checkUser(userDto.getUser());
        verify(usersService, times(2)).checkUser(userDto.getNewUser().strip());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testEditUser_TokenDoesNotMatch() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("oldUser");
        userDto.setPassword("password");
        userDto.setNewUser("newUser");
        userDto.setToken("incorrectToken");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "The token does not match");
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(expectedResponseJson.toString(), HttpStatus.NOT_FOUND);

        when(usersService.checkUser(userDto.getUser())).thenReturn(true);
        when(usersService.checkToken(userDto.getUser(), userDto.getToken())).thenReturn(false);

        ResponseEntity<String> actualResponse = usersController.editUser(userDto);

        assertEquals(HttpStatus.NOT_FOUND, actualResponse.getStatusCode());
        assertEquals(expectedResponse.getBody(), actualResponse.getBody());
        verify(usersService, times(1)).checkUser(userDto.getUser());
        verify(usersService, times(1)).checkUser(userDto.getNewUser().strip());
        verify(usersService, times(1)).checkToken(userDto.getUser(), userDto.getToken());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testDeleteUser_UserNameRequired() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("");

        JSONObject expectedJsonResponse = new JSONObject();
        expectedJsonResponse.put("message", "User name is required to delete a user");
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(expectedJsonResponse.toString(), HttpStatus.BAD_GATEWAY);

        ResponseEntity<String> response = usersController.deleteUser(userDto);

        assertEquals(HttpStatus.BAD_GATEWAY, response.getStatusCode());
        assertEquals(expectedResponse.getBody(), response.getBody());
        verifyNoInteractions(usersService);
    }

    @Test
    void testDeleteUser_UserNotFound() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("nonExistingUser");

        JSONObject expectedJsonResponse = new JSONObject();
        expectedJsonResponse.put("message", "The user does not exist");
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(expectedJsonResponse.toString(), HttpStatus.NOT_FOUND);

        when(usersService.checkUser(userDto.getUser())).thenReturn(false);

        ResponseEntity<String> response = usersController.deleteUser(userDto);

        assertEquals(HttpStatus.NOT_FOUND, response.getStatusCode());
        assertEquals(expectedResponse.getBody(), response.getBody());
        verify(usersService, times(1)).checkUser(userDto.getUser());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testDeleteUser_TokenDoesNotMatch() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("exampleUser");
        userDto.setToken("incorrectToken");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "The token does not match");
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(expectedResponseJson.toString(), HttpStatus.NOT_FOUND);

        when(usersService.checkUser(userDto.getUser())).thenReturn(true);
        when(usersService.checkToken(userDto.getUser(), userDto.getToken())).thenReturn(false);

        ResponseEntity<String> actualResponse = usersController.deleteUser(userDto);

        assertEquals(HttpStatus.NOT_FOUND, actualResponse.getStatusCode());
        assertEquals(expectedResponse.getBody(), actualResponse.getBody());
        verify(usersService, times(1)).checkUser(userDto.getUser());
        verify(usersService, times(1)).checkToken(userDto.getUser(), userDto.getToken());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testDeleteUser_SuccessfulDeletion() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("existingUser");
        userDto.setToken("token");

        JSONObject expectedJsonResponse = new JSONObject();
        expectedJsonResponse.put("message", "User deleted successfully");
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(expectedJsonResponse.toString(), HttpStatus.OK);

        when(usersService.checkUser(userDto.getUser())).thenReturn(true);
        when(usersService.checkToken(userDto.getUser(), userDto.getToken())).thenReturn(true);

        ResponseEntity<String> response = usersController.deleteUser(userDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertEquals(expectedResponse.getBody(), response.getBody());
        verify(usersService, times(1)).checkUser(userDto.getUser());
        verify(usersService, times(1)).checkToken(userDto.getUser(), userDto.getToken());
        verify(usersService, times(1)).deleteUser(userDto.getUser());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testGetAllUsersData_NoUsersFound() {
        List<UsersEntity> usersList = new ArrayList<>();

        JSONObject expectedJsonResponse = new JSONObject();
        expectedJsonResponse.put("message", "There are no users in database");
        SpecialResponse expectedSpecialResponse = new SpecialResponse(usersList, expectedJsonResponse.toString());
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(expectedSpecialResponse, HttpStatus.OK);

        when(usersService.getAllUsersData()).thenReturn(usersList);

        ResponseEntity<SpecialResponse> response = usersController.getAllUsersDataAll();

        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertEquals(expectedResponse.getBody().getMessage(), response.getBody().getMessage());
        assertEquals(expectedResponse.getBody().getEntity(), response.getBody().getEntity());
        verify(usersService, times(1)).getAllUsersData();
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testGetAllUsersDataAll_Successful() {
        UsersEntity userEntity1 = new UsersEntity();
        userEntity1.setId(1);
        userEntity1.setUser("user1");
        userEntity1.setPassword("password1");
        userEntity1.setEmail("email1");
        userEntity1.setToken("token1");

        UsersEntity userEntity2 = new UsersEntity();
        userEntity2.setId(2);
        userEntity2.setUser("user2");
        userEntity2.setPassword("password2");
        userEntity2.setEmail("email2");
        userEntity2.setToken("token2");

        List<UsersEntity> usersList = new ArrayList<>();
        usersList.add(userEntity1);
        usersList.add(userEntity2);

        SpecialResponse expectedSpecialResponse = new SpecialResponse(usersList, "{\"message\":\"OK\"}");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(expectedSpecialResponse, HttpStatus.OK);

        when(usersService.getAllUsersData()).thenReturn(usersList);

        ResponseEntity<SpecialResponse> response = usersController.getAllUsersDataAll();

        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertEquals(expectedResponse.getBody().getMessage(), response.getBody().getMessage());
        verify(usersService, times(1)).getAllUsersData();
        verifyNoMoreInteractions(usersService);
    }

}

