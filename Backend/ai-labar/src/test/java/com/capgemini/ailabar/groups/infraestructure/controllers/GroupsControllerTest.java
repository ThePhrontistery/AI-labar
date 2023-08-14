package com.capgemini.ailabar.groups.infraestructure.controllers;

import com.capgemini.ailabar.commons.utils.SpecialResponse;
import com.capgemini.ailabar.groups.application.services.GroupsService;
import com.capgemini.ailabar.groups.domain.exceptions.*;
import com.capgemini.ailabar.groups.domain.models.GroupsModel;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.util.*;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class GroupsControllerTest {

    @Mock
    private GroupsService groupsService;

    @InjectMocks
    private GroupsController groupsController;

    @Test
    void testCreateGroupSuccess() {
        GroupsModel groupModel = new GroupsModel();
        groupModel.setGroupName("TestGroup");
        groupModel.setMembers(Collections.singletonList("Member1"));
        groupModel.setUser("AdminUser");
        groupModel.setToken("AdminToken");

        JSONObject responseJson = new JSONObject();
        responseJson.put("message", "Group created successfully");

        doNothing().when(groupsService).createGroup(any());

        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(groupsController.specialResponse(null, responseJson), HttpStatus.OK);

        ResponseEntity<SpecialResponse> actualResponse = groupsController.createGroup(groupModel);

        assertEquals(expectedResponse.getStatusCode(), actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());

        verify(groupsService, times(1)).createGroup(groupModel);
    }

    @Test
    void testGetGroupSuccess() {
        GroupsModel groupModel = new GroupsModel();
        groupModel.setGroupName("TestGroup");
        groupModel.setUser("AdminUser");
        groupModel.setToken("AdminToken");

        GroupsModel matchedGroup = new GroupsModel();
        matchedGroup.setGroupName("TestGroup");
        matchedGroup.setUser("AdminUser");
        matchedGroup.setMembers(Arrays.asList("Member1", "Member2"));

        JSONObject responseJson = new JSONObject();
        responseJson.put("message", "OK");

        when(groupsService.getGroup(any())).thenReturn(matchedGroup);

        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(groupsController.specialResponse(matchedGroup, responseJson), HttpStatus.OK);

        ResponseEntity<SpecialResponse> actualResponse = groupsController.getGroup(groupModel);

        assertEquals(expectedResponse.getStatusCode(), actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getEntity(), Objects.requireNonNull(actualResponse.getBody()).getEntity());

        verify(groupsService, times(1)).getGroup(groupModel);
    }

    @Test
    void testEditGroupSuccess() {
        GroupsModel groupModel = new GroupsModel();
        groupModel.setId(1);
        groupModel.setGroupName("TestGroup");
        groupModel.setMembers(Arrays.asList("Member1", "Member2"));
        groupModel.setUser("AdminUser");
        groupModel.setToken("AdminToken");

        JSONObject responseJson = new JSONObject();
        responseJson.put("message", "Group edited successfully");

        doNothing().when(groupsService).editGroup(any());

        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(groupsController.specialResponse(null, responseJson), HttpStatus.OK);

        ResponseEntity<SpecialResponse> actualResponse = groupsController.editGroup(groupModel);

        assertEquals(expectedResponse.getStatusCode(), actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());

        verify(groupsService, times(1)).editGroup(groupModel);
    }

    @Test
    void testGetGroupsByUserSuccess() {
        GroupsModel groupModel = new GroupsModel();
        groupModel.setUser("TestUser");
        groupModel.setToken("TestToken");

        List<String> groupsList = Arrays.asList("Group1", "Group2", "Group3");

        JSONObject responseJson = new JSONObject();
        responseJson.put("message", "OK");

        when(groupsService.getGroupsByUser(any())).thenReturn(groupsList);

        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(groupsController.specialResponse(groupsList, responseJson), HttpStatus.OK);

        ResponseEntity<SpecialResponse> actualResponse = groupsController.getGroupsByUser(groupModel);

        assertEquals(expectedResponse.getStatusCode(), actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getEntity(), Objects.requireNonNull(actualResponse.getBody()).getEntity());

        verify(groupsService, times(1)).getGroupsByUser(groupModel);
    }

    @Test
    void testDeleteGroupSuccess() {
        GroupsModel groupModel = new GroupsModel();
        groupModel.setId(1);
        groupModel.setUser("AdminUser");
        groupModel.setToken("AdminToken");

        JSONObject responseJson = new JSONObject();
        responseJson.put("message", "Group deleted successfully");

        doNothing().when(groupsService).deleteGroup(any());

        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(groupsController.specialResponse(null, responseJson), HttpStatus.OK);

        ResponseEntity<SpecialResponse> actualResponse = groupsController.deleteGroup(groupModel);

        assertEquals(expectedResponse.getStatusCode(), actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());

        verify(groupsService, times(1)).deleteGroup(groupModel);
    }

    @Test
    void testGetGroupsDatabaseSuccess() {
        List<GroupsModel> groupsList = new ArrayList<>();

        GroupsModel group1 = new GroupsModel();
        group1.setId(1);
        group1.setGroupName("Group1");
        group1.setMembers(Arrays.asList("Member1", "Member2"));
        group1.setAdmin("AdminUser1");
        groupsList.add(group1);

        GroupsModel group2 = new GroupsModel();
        group2.setId(2);
        group2.setGroupName("Group2");
        group2.setMembers(Arrays.asList("Member3", "Member4"));
        group2.setAdmin("AdminUser2");
        groupsList.add(group2);

        JSONObject responseJson = new JSONObject();
        responseJson.put("message", "OK");

        when(groupsService.getGroupsDatabase()).thenReturn(groupsList);

        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(groupsController.specialResponse(groupsList, responseJson), HttpStatus.OK);

        ResponseEntity<SpecialResponse> actualResponse = groupsController.getGroupsDatabase();

        assertEquals(expectedResponse.getStatusCode(), actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getEntity(), Objects.requireNonNull(actualResponse.getBody()).getEntity());

        verify(groupsService, times(1)).getGroupsDatabase();
    }

    @Test
    void testHandlerCreateGroupException() {
        CreateGroupException exception = new CreateGroupException("Create group error message");

        ResponseEntity<SpecialResponse> response = groupsController.handlerCreateGroupException(exception);

        assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
        JSONObject expectedJson = new JSONObject();
        expectedJson.put("message", "Create group error message");
        assertEquals(expectedJson.getString("message"), Objects.requireNonNull(response.getBody()).getMessage());
    }

    @Test
    void testHandlerGetGroupException() {
        GetGroupException exception = new GetGroupException("Get group error message");

        ResponseEntity<SpecialResponse> response = groupsController.handlerGetGroupException(exception);

        assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
        JSONObject expectedJson = new JSONObject();
        expectedJson.put("message", "Get group error message");
        assertEquals(expectedJson.getString("message"), Objects.requireNonNull(response.getBody()).getMessage());
    }

    @Test
    void testHandlerEditGroupException() {
        EditGroupException exception = new EditGroupException("Edit group error message");

        ResponseEntity<SpecialResponse> response = groupsController.handlerEditGroupException(exception);

        assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
        JSONObject expectedJson = new JSONObject();
        expectedJson.put("message", "Edit group error message");
        assertEquals(expectedJson.getString("message"), Objects.requireNonNull(response.getBody()).getMessage());
    }

    @Test
    void testHandlerGetGroupsByUserException() {
        GetGroupsByUserException exception = new GetGroupsByUserException("Get groups by user error message");

        ResponseEntity<SpecialResponse> response = groupsController.handlerGetGroupsByUserException(exception);

        assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
        JSONObject expectedJson = new JSONObject();
        expectedJson.put("message", "Get groups by user error message");
        assertEquals(expectedJson.getString("message"), Objects.requireNonNull(response.getBody()).getMessage());
    }

    @Test
    void testHandlerDeleteGroupException() {
        DeleteGroupException exception = new DeleteGroupException("Delete group error message");

        ResponseEntity<SpecialResponse> response = groupsController.handlerDeleteGroupException(exception);

        assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
        JSONObject expectedJson = new JSONObject();
        expectedJson.put("message", "Delete group error message");
        assertEquals(expectedJson.getString("message"), Objects.requireNonNull(response.getBody()).getMessage());
    }

    @Test
    void testHandlerGetGroupsDatabaseException() {
        GetGroupsDatabaseException exception = new GetGroupsDatabaseException("Get groups database error message");

        ResponseEntity<SpecialResponse> response = groupsController.handlerGetGroupsDatabaseException(exception);

        assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
        JSONObject expectedJson = new JSONObject();
        expectedJson.put("message", "Get groups database error message");
        assertEquals(expectedJson.getString("message"), Objects.requireNonNull(response.getBody()).getMessage());
    }
}