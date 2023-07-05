package com.capgemini.beni.ailabar.controller;

import com.capgemini.beni.ailabar.dto.GroupsDto;
import com.capgemini.beni.ailabar.dto.UsersDto;
import com.capgemini.beni.ailabar.entity.GroupsEntity;
import com.capgemini.beni.ailabar.entity.UsersEntity;
import com.capgemini.beni.ailabar.service.GroupsService;
import com.capgemini.beni.ailabar.service.UsersService;
import com.capgemini.beni.ailabar.utils.SpecialResponse;
import org.json.JSONObject;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@SpringBootTest
class GroupsControllerTest {
    @Mock
    private GroupsService groupsService;

    @Mock
    private UsersService usersService;

    @InjectMocks
    private GroupsController groupsController;

    @BeforeEach
    void setUp() {
        Mockito.reset(groupsService, usersService);
    }

    @Test
    void testGetUser_ValidUser() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("validUser");

        UsersEntity userEntity = new UsersEntity();
        userEntity.setUser("validUser");

        when(usersService.checkUser(userDto.getUser())).thenReturn(true);
        when(usersService.findByUser(userDto.getUser())).thenReturn(userEntity);

        ResponseEntity<String> response = groupsController.getUser(userDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        JSONObject responseJson = new JSONObject(response.getBody());
        assertEquals("validUser", responseJson.getString("message"));
        verify(usersService, times(1)).checkUser(userDto.getUser());
        verify(usersService, times(1)).findByUser(userDto.getUser());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testGetUser_BlankUser() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("");

        ResponseEntity<String> response = groupsController.getUser(userDto);

        assertEquals(HttpStatus.BAD_GATEWAY, response.getStatusCode());
        JSONObject responseJson = new JSONObject(response.getBody());
        assertEquals("User name is required", responseJson.getString("message"));
        verifyNoInteractions(usersService);
    }

    @Test
    void testGetUser_NonexistentUser() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("nonexistentUser");

        when(usersService.checkUser(userDto.getUser())).thenReturn(false);

        ResponseEntity<String> response = groupsController.getUser(userDto);

        assertEquals(HttpStatus.NOT_FOUND, response.getStatusCode());
        JSONObject responseJson = new JSONObject(response.getBody());
        assertEquals("The user does not exist", responseJson.getString("message"));
        verify(usersService, times(1)).checkUser(userDto.getUser());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testCreateGroup_ValidGroup() {
        GroupsDto groupDto = new GroupsDto();
        groupDto.setGroupName("exampleGroup");
        groupDto.setMembers("exampleMembers");
        groupDto.setAdmin("exampleAdmin");

        when(usersService.checkUser(groupDto.getAdmin())).thenReturn(true);
        when(groupsService.existsByGroupNameAndAdmin(groupDto.getGroupName().strip(), groupDto.getAdmin())).thenReturn(false);

        ResponseEntity<String> response = groupsController.createGroup(groupDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        JSONObject responseJson = new JSONObject(response.getBody());
        assertEquals("Group created successfully", responseJson.getString("message"));
        verify(usersService, times(1)).checkUser(groupDto.getAdmin());
        verify(groupsService, times(1)).existsByGroupNameAndAdmin(groupDto.getGroupName().strip(), groupDto.getAdmin());
        verify(groupsService, times(1)).saveGroup(any(GroupsEntity.class));
        verifyNoMoreInteractions(usersService);
        verifyNoMoreInteractions(groupsService);
    }

    @Test
    void testCreateGroup_InvalidGroup() {
        GroupsDto groupDto = new GroupsDto();
        groupDto.setGroupName("");
        groupDto.setMembers("");
        groupDto.setAdmin("");

        ResponseEntity<String> response = groupsController.createGroup(groupDto);

        assertEquals(HttpStatus.BAD_GATEWAY, response.getStatusCode());
        JSONObject responseJson = new JSONObject(response.getBody());
        assertEquals("All data is required to save a group", responseJson.getString("message"));
        verifyNoInteractions(usersService);
        verifyNoInteractions(groupsService);
    }

    @Test
    void testCreateGroup_NonexistentAdmin() {
        GroupsDto groupDto = new GroupsDto();
        groupDto.setGroupName("exampleGroup");
        groupDto.setMembers("exampleMembers");
        groupDto.setAdmin("nonexistentAdmin");

        when(usersService.checkUser(groupDto.getAdmin())).thenReturn(false);

        ResponseEntity<String> response = groupsController.createGroup(groupDto);

        assertEquals(HttpStatus.NOT_FOUND, response.getStatusCode());
        JSONObject responseJson = new JSONObject(response.getBody());
        assertEquals("The admin does not exist", responseJson.getString("message"));
        verify(usersService, times(1)).checkUser(groupDto.getAdmin());
        verifyNoMoreInteractions(usersService);
        verifyNoInteractions(groupsService);
    }

    @Test
    void testCreateGroup_GroupExistsForAdmin() {
        GroupsDto groupDto = new GroupsDto();
        groupDto.setGroupName("exampleGroup");
        groupDto.setMembers("exampleMembers");
        groupDto.setAdmin("exampleAdmin");

        when(usersService.checkUser(groupDto.getAdmin())).thenReturn(true);
        when(groupsService.existsByGroupNameAndAdmin(groupDto.getGroupName().strip(), groupDto.getAdmin())).thenReturn(true);

        ResponseEntity<String> response = groupsController.createGroup(groupDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        JSONObject responseJson = new JSONObject(response.getBody());
        assertEquals("The administrator already has a group with that name", responseJson.getString("message"));
        verify(usersService, times(1)).checkUser(groupDto.getAdmin());
        verify(groupsService, times(1)).existsByGroupNameAndAdmin(groupDto.getGroupName().strip(), groupDto.getAdmin());
        verifyNoMoreInteractions(usersService);
        verifyNoMoreInteractions(groupsService);
    }

    @Test
    void testEditGroup_ValidGroup() {
        GroupsDto groupDto = new GroupsDto();
        groupDto.setId(1);
        groupDto.setGroupName("exampleGroup");
        groupDto.setMembers("exampleMembers");
        groupDto.setAdmin("exampleAdmin");
        groupDto.setNewGroupName("newGroupName");

        GroupsEntity groupEntity = new GroupsEntity();
        groupEntity.setId(1);
        groupEntity.setGroupName("exampleGroup");
        groupEntity.setMembers("exampleMembers");
        groupEntity.setAdmin("exampleAdmin");

        when(usersService.checkUser(groupDto.getAdmin())).thenReturn(true);
        when(groupsService.findGroupsEntityById(groupDto.getId())).thenReturn(groupEntity);
        when(groupsService.existsByGroupNameAndAdmin(groupDto.getNewGroupName().strip(), groupDto.getAdmin())).thenReturn(false);

        ResponseEntity<String> response = groupsController.editGroup(groupDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        JSONObject responseJson = new JSONObject(response.getBody());
        assertEquals("Group created successfully", responseJson.getString("message"));
        assertEquals("newGroupName", groupEntity.getGroupName());
        verify(usersService, times(1)).checkUser(groupDto.getAdmin());
        verify(groupsService, times(1)).findGroupsEntityById(groupDto.getId());
        verify(groupsService, times(1)).existsByGroupNameAndAdmin(groupDto.getNewGroupName().strip(), groupDto.getAdmin());
        verify(groupsService, times(1)).saveGroup(groupEntity);
        verifyNoMoreInteractions(usersService);
        verifyNoMoreInteractions(groupsService);
    }

    @Test
    void testEditGroup_InvalidGroup() {
        GroupsDto groupDto = new GroupsDto();
        groupDto.setId(null);
        groupDto.setGroupName("");
        groupDto.setMembers("");
        groupDto.setAdmin("");

        ResponseEntity<String> response = groupsController.editGroup(groupDto);

        assertEquals(HttpStatus.BAD_GATEWAY, response.getStatusCode());
        JSONObject responseJson = new JSONObject(response.getBody());
        assertEquals("All data is required to save a group", responseJson.getString("message"));
        verifyNoInteractions(usersService);
        verifyNoInteractions(groupsService);
    }

    @Test
    void testEditGroup_NonexistentAdmin() {
        GroupsDto groupDto = new GroupsDto();
        groupDto.setId(1);
        groupDto.setGroupName("exampleGroup");
        groupDto.setMembers("exampleMembers");
        groupDto.setAdmin("nonexistentAdmin");

        when(usersService.checkUser(groupDto.getAdmin())).thenReturn(false);

        ResponseEntity<String> response = groupsController.editGroup(groupDto);

        assertEquals(HttpStatus.NOT_FOUND, response.getStatusCode());
        JSONObject responseJson = new JSONObject(response.getBody());
        assertEquals("The admin does not exist", responseJson.getString("message"));
        verify(usersService, times(1)).checkUser(groupDto.getAdmin());
        verifyNoMoreInteractions(usersService);
        verifyNoInteractions(groupsService);
    }

    @Test
    void testEditGroup_UserNotAdmin() {
        GroupsDto groupDto = new GroupsDto();
        groupDto.setId(1);
        groupDto.setGroupName("exampleGroup");
        groupDto.setMembers("exampleMembers");
        groupDto.setAdmin("exampleAdmin");

        GroupsEntity groupEntity = new GroupsEntity();
        groupEntity.setId(1);
        groupEntity.setGroupName("exampleGroup");
        groupEntity.setMembers("exampleMembers");
        groupEntity.setAdmin("otherAdmin");

        when(usersService.checkUser(groupDto.getAdmin())).thenReturn(true);
        when(groupsService.findGroupsEntityById(groupDto.getId())).thenReturn(groupEntity);

        ResponseEntity<String> response = groupsController.editGroup(groupDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        JSONObject responseJson = new JSONObject(response.getBody());
        assertEquals("The user is not the group administrator", responseJson.getString("message"));
        verify(usersService, times(1)).checkUser(groupDto.getAdmin());
        verify(groupsService, times(1)).findGroupsEntityById(groupDto.getId());
        verifyNoMoreInteractions(usersService);
        verifyNoMoreInteractions(groupsService);
    }

    @Test
    void testEditGroup_GroupExistsForAdmin() {
        GroupsDto groupDto = new GroupsDto();
        groupDto.setId(1);
        groupDto.setGroupName("exampleGroup");
        groupDto.setMembers("exampleMembers");
        groupDto.setAdmin("exampleAdmin");
        groupDto.setNewGroupName("newGroupName");

        GroupsEntity groupEntity = new GroupsEntity();
        groupEntity.setId(1);
        groupEntity.setGroupName("exampleGroup");
        groupEntity.setMembers("exampleMembers");
        groupEntity.setAdmin("exampleAdmin");

        when(usersService.checkUser(groupDto.getAdmin())).thenReturn(true);
        when(groupsService.findGroupsEntityById(groupDto.getId())).thenReturn(groupEntity);
        when(groupsService.existsByGroupNameAndAdmin(groupDto.getNewGroupName().strip(), groupDto.getAdmin())).thenReturn(true);

        ResponseEntity<String> response = groupsController.editGroup(groupDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        JSONObject responseJson = new JSONObject(response.getBody());
        assertEquals("The administrator already has a group with that name", responseJson.getString("message"));
        verify(usersService, times(1)).checkUser(groupDto.getAdmin());
        verify(groupsService, times(1)).findGroupsEntityById(groupDto.getId());
        verify(groupsService, times(1)).existsByGroupNameAndAdmin(groupDto.getNewGroupName().strip(), groupDto.getAdmin());
        verifyNoMoreInteractions(usersService);
        verifyNoMoreInteractions(groupsService);
    }

    @Test
    void testGetGroup_InvalidGroupNameAndAdmin() {
        GroupsDto groupDto = new GroupsDto();
        groupDto.setGroupName("");
        groupDto.setAdmin("");
        SpecialResponse expectedSpecialResponse = new SpecialResponse(null, "{\"message\":\"Group name and admin are required\"}");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(expectedSpecialResponse, HttpStatus.BAD_GATEWAY);

        ResponseEntity<SpecialResponse> response = groupsController.getGroup(groupDto);

        assertEquals(HttpStatus.BAD_GATEWAY, response.getStatusCode());
        assertEquals(expectedResponse.getBody().getMessage(), response.getBody().getMessage());
        verifyNoInteractions(groupsService);
    }

    @Test
    void testGetGroup_NonExistingGroup() {
        GroupsDto groupDto = new GroupsDto();
        groupDto.setGroupName("exampleGroup");
        groupDto.setAdmin("exampleAdmin");
        GroupsEntity groupEntity = null;
        SpecialResponse expectedSpecialResponse = new SpecialResponse(null, "{\"message\":\"The admin does not have a group with that name\"}");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(expectedSpecialResponse, HttpStatus.OK);

        when(groupsService.getGroup(groupDto.getGroupName(), groupDto.getAdmin())).thenReturn(groupEntity);

        ResponseEntity<SpecialResponse> response = groupsController.getGroup(groupDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertEquals(expectedResponse.getBody().getMessage(), response.getBody().getMessage());
        verify(groupsService, times(1)).getGroup(groupDto.getGroupName(), groupDto.getAdmin());
        verifyNoMoreInteractions(groupsService);
    }

    @Test
    void testGetGroup_ValidGroup() {
        GroupsDto groupDto = new GroupsDto();
        groupDto.setGroupName("exampleGroup");
        groupDto.setAdmin("exampleAdmin");
        GroupsEntity groupEntity = new GroupsEntity();

        SpecialResponse expectedSpecialResponse = new SpecialResponse(groupEntity, "{\"message\":\"OK\"}");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(expectedSpecialResponse, HttpStatus.OK);

        when(groupsService.getGroup(groupDto.getGroupName(), groupDto.getAdmin())).thenReturn(groupEntity);

        ResponseEntity<SpecialResponse> response = groupsController.getGroup(groupDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertEquals(expectedResponse.getBody().getMessage(), response.getBody().getMessage());
        verify(groupsService, times(1)).getGroup(groupDto.getGroupName(), groupDto.getAdmin());
        verifyNoMoreInteractions(groupsService);
    }

    @Test
    void testGetGroupForEdit_NoAdmin() {
        GroupsDto groupDto = new GroupsDto();
        groupDto.setAdmin("");
        SpecialResponse expectedSpecialResponse = new SpecialResponse(null, "{\"message\":\"Admin is required\"}");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(expectedSpecialResponse, HttpStatus.BAD_GATEWAY);

        ResponseEntity<SpecialResponse> response = groupsController.getGroupForEdit(groupDto);

        assertEquals(HttpStatus.BAD_GATEWAY, response.getStatusCode());
        assertEquals(expectedResponse.getBody().getMessage(), response.getBody().getMessage());
        verifyNoInteractions(groupsService);
    }

    @Test
    void testGetGroupForEdit_NoGroupsFound() {
        GroupsDto groupDto = new GroupsDto();
        groupDto.setAdmin("exampleAdmin");
        List<String> groupsList = new ArrayList<>();
        SpecialResponse expectedSpecialResponse = new SpecialResponse(null, "{\"message\":\"The admin is not part of any group\"}");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(expectedSpecialResponse, HttpStatus.OK);

        when(groupsService.getGroupForEdit(groupDto.getAdmin())).thenReturn(groupsList);

        ResponseEntity<SpecialResponse> response = groupsController.getGroupForEdit(groupDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertEquals(expectedResponse.getBody().getMessage(), response.getBody().getMessage());
        verify(groupsService, times(1)).getGroupForEdit(groupDto.getAdmin());
        verifyNoMoreInteractions(groupsService);
    }

    @Test
    void testGetGroupForEdit_ValidAdminWithGroups() {
        GroupsDto groupDto = new GroupsDto();
        groupDto.setAdmin("exampleAdmin");
        List<String> groupsList = new ArrayList<>();
        groupsList.add("Group1");
        groupsList.add("Group2");

        SpecialResponse expectedSpecialResponse = new SpecialResponse(groupsList, "{\"message\":\"OK\"}");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(expectedSpecialResponse, HttpStatus.OK);

        when(groupsService.getGroupForEdit(groupDto.getAdmin())).thenReturn(groupsList);

        ResponseEntity<SpecialResponse> response = groupsController.getGroupForEdit(groupDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertEquals(expectedResponse.getBody().getMessage(), response.getBody().getMessage());
        assertEquals(expectedResponse.getBody().getEntity(), response.getBody().getEntity());
        verify(groupsService, times(1)).getGroupForEdit(groupDto.getAdmin());
        verifyNoMoreInteractions(groupsService);
    }

    @Test
    void testDeleteGroup_InvalidGroupNameAndAdmin() {
        GroupsDto groupDto = new GroupsDto();
        groupDto.setGroupName("");
        groupDto.setAdmin("");
        JSONObject expectedJsonResponse = new JSONObject();
        expectedJsonResponse.put("message", "Group name and administrator are required to delete a group");
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(expectedJsonResponse.toString(), HttpStatus.BAD_GATEWAY);

        ResponseEntity<String> response = groupsController.deleteGroup(groupDto);

        assertEquals(HttpStatus.BAD_GATEWAY, response.getStatusCode());
        assertEquals(expectedResponse.getBody(), response.getBody());
        verifyNoInteractions(groupsService);
    }

    @Test
    void testDeleteGroup_NonExistingGroup() {
        GroupsDto groupDto = new GroupsDto();
        groupDto.setGroupName("exampleGroup");
        groupDto.setAdmin("exampleAdmin");
        JSONObject expectedJsonResponse = new JSONObject();
        expectedJsonResponse.put("message", "The administrator does not have a group with that name");
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(expectedJsonResponse.toString(), HttpStatus.OK);

        when(groupsService.existsByGroupNameAndAdmin(groupDto.getGroupName().strip(), groupDto.getAdmin())).thenReturn(false);

        ResponseEntity<String> response = groupsController.deleteGroup(groupDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertEquals(expectedResponse.getBody(), response.getBody());
        verify(groupsService, times(1)).existsByGroupNameAndAdmin(groupDto.getGroupName().strip(), groupDto.getAdmin());
        verifyNoMoreInteractions(groupsService);
    }

    @Test
    void testDeleteGroup_ValidGroup() {
        GroupsDto groupDto = new GroupsDto();
        groupDto.setGroupName("exampleGroup");
        groupDto.setAdmin("exampleAdmin");
        JSONObject expectedJsonResponse = new JSONObject();
        expectedJsonResponse.put("message", "Group deleted successfully");
        ResponseEntity<String> expectedResponse = new ResponseEntity<>(expectedJsonResponse.toString(), HttpStatus.OK);

        when(groupsService.existsByGroupNameAndAdmin(groupDto.getGroupName().strip(), groupDto.getAdmin())).thenReturn(true);

        ResponseEntity<String> response = groupsController.deleteGroup(groupDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertEquals(expectedResponse.getBody(), response.getBody());
        verify(groupsService, times(1)).existsByGroupNameAndAdmin(groupDto.getGroupName().strip(), groupDto.getAdmin());
        verify(groupsService, times(1)).deleteGroup(groupDto.getGroupName().strip(), groupDto.getAdmin());
        verifyNoMoreInteractions(groupsService);
    }

    @Test
    void testGetAllGroupsData_NoGroupsFound() {
        List<GroupsEntity> groupsList = new ArrayList<>();
        SpecialResponse expectedSpecialResponse = new SpecialResponse(groupsList, "{\"message\":\"There are no groups in database\"}");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(expectedSpecialResponse, HttpStatus.OK);

        when(groupsService.getAllGroupsData()).thenReturn(groupsList);

        ResponseEntity<SpecialResponse> response = groupsController.getAllGroupsData();

        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertEquals(expectedResponse.getBody().getMessage(), response.getBody().getMessage());
        assertEquals(expectedResponse.getBody().getEntity(), response.getBody().getEntity());
        verify(groupsService, times(1)).getAllGroupsData();
        verifyNoMoreInteractions(groupsService);
    }

    @Test
    void testGetAllGroupsData_GroupsFound() {
        GroupsEntity groupEntity = new GroupsEntity();
        groupEntity.setId(1);
        groupEntity.setGroupName("groupName");
        groupEntity.setMembers("members");
        groupEntity.setAdmin("admin");

        List<GroupsEntity> groupsList = new ArrayList<>();
        groupsList.add(groupEntity);

        SpecialResponse expectedSpecialResponse = new SpecialResponse(groupsList, "{\"message\":\"OK\"}");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(expectedSpecialResponse, HttpStatus.OK);

        when(groupsService.getAllGroupsData()).thenReturn(groupsList);

        ResponseEntity<SpecialResponse> response = groupsController.getAllGroupsData();

        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertEquals(expectedResponse.getBody().getMessage(), response.getBody().getMessage());
        assertEquals(expectedResponse.getBody().getEntity(), response.getBody().getEntity());
        verify(groupsService, times(1)).getAllGroupsData();
        verifyNoMoreInteractions(groupsService);
    }

}

