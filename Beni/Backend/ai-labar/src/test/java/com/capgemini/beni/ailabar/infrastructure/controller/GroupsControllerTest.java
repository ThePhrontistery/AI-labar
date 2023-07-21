package com.capgemini.beni.ailabar.infrastructure.controller;

import com.capgemini.beni.ailabar.domain.model.GroupsModel;
import com.capgemini.beni.ailabar.infrastructure.entity.GroupsEntity;
import com.capgemini.beni.ailabar.application.service.GroupsService;
import com.capgemini.beni.ailabar.application.service.UsersService;
import com.capgemini.beni.ailabar.infrastructure.utils.SpecialResponse;
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

import java.util.*;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;

@ExtendWith(SpringExtension.class)
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
    void testCreateGroup_MissingData() {
        GroupsModel groupDto = new GroupsModel();
        groupDto.setGroupName("");
        groupDto.setMembers(Collections.emptyList());
        groupDto.setUser("");
        groupDto.setToken("");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "All data is required to save a group");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(groupsController.specialResponse(null, expectedResponseJson), HttpStatus.BAD_GATEWAY);

        ResponseEntity<SpecialResponse> actualResponse = groupsController.createGroup(groupDto);

        assertEquals(HttpStatus.BAD_GATEWAY, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verifyNoInteractions(usersService, groupsService);
    }

    @Test
    void testCreateGroup_UnauthorizedUser() {
        GroupsModel groupDto = new GroupsModel();
        groupDto.setGroupName("exampleGroup");
        groupDto.setMembers(Arrays.asList("Member1", "Member2"));
        groupDto.setUser("exampleUser");
        groupDto.setToken("exampleToken");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "Unauthorized user");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(groupsController.specialResponse(null, expectedResponseJson), HttpStatus.NOT_FOUND);

        when(usersService.checkToken(groupDto.getUser(), groupDto.getToken())).thenReturn(false);

        ResponseEntity<SpecialResponse> actualResponse = groupsController.createGroup(groupDto);

        assertEquals(HttpStatus.NOT_FOUND, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(groupDto.getUser(), groupDto.getToken());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testCreateGroup_DuplicateGroupNameForUser() {
        GroupsModel groupDto = new GroupsModel();
        groupDto.setGroupName("exampleGroup");
        groupDto.setMembers(Arrays.asList("Member1", "Member2"));
        groupDto.setUser("exampleUser");
        groupDto.setToken("exampleToken");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "The user already has a group with that name");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(groupsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(groupDto.getUser(), groupDto.getToken())).thenReturn(true);
        when(groupsService.existsByGroupNameAndAdmin(groupDto.getGroupName().strip(), groupDto.getUser())).thenReturn(true);

        ResponseEntity<SpecialResponse> actualResponse = groupsController.createGroup(groupDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(groupDto.getUser(), groupDto.getToken());
        verify(groupsService, times(1)).existsByGroupNameAndAdmin(groupDto.getGroupName().strip(), groupDto.getUser());
        verifyNoMoreInteractions(usersService, groupsService);
    }

    @Test
    void testCreateGroup_ValidData_GroupCreatedSuccessfully() {
        GroupsModel groupDto = new GroupsModel();
        groupDto.setGroupName("exampleGroup");
        groupDto.setMembers(Arrays.asList("Member1", "Member2"));
        groupDto.setUser("exampleUser");
        groupDto.setToken("exampleToken");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "Group created successfully");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(groupsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(groupDto.getUser(), groupDto.getToken())).thenReturn(true);
        when(groupsService.existsByGroupNameAndAdmin(groupDto.getGroupName().strip(), groupDto.getUser())).thenReturn(false);

        ResponseEntity<SpecialResponse> actualResponse = groupsController.createGroup(groupDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(groupDto.getUser(), groupDto.getToken());
        verify(groupsService, times(1)).existsByGroupNameAndAdmin(groupDto.getGroupName().strip(), groupDto.getUser());
        verify(groupsService, times(1)).saveGroup(any(GroupsEntity.class));
        verifyNoMoreInteractions(usersService, groupsService);
    }

    @Test
    void testGetGroup_MissingData() {
        GroupsModel groupDto = new GroupsModel();
        groupDto.setGroupName("");
        groupDto.setUser("");
        groupDto.setToken("");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "Group name and user are required");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(groupsController.specialResponse(null, expectedResponseJson), HttpStatus.BAD_GATEWAY);

        ResponseEntity<SpecialResponse> actualResponse = groupsController.getGroup(groupDto);

        assertEquals(HttpStatus.BAD_GATEWAY, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verifyNoInteractions(usersService, groupsService);
    }

    @Test
    void testGetGroup_UnauthorizedUser() {
        GroupsModel groupDto = new GroupsModel();
        groupDto.setGroupName("exampleGroup");
        groupDto.setUser("exampleUser");
        groupDto.setToken("exampleToken");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "Unauthorized user");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(groupsController.specialResponse(null, expectedResponseJson), HttpStatus.NOT_FOUND);

        when(usersService.checkToken(groupDto.getUser(), groupDto.getToken())).thenReturn(false);

        ResponseEntity<SpecialResponse> actualResponse = groupsController.getGroup(groupDto);

        assertEquals(HttpStatus.NOT_FOUND, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(groupDto.getUser(), groupDto.getToken());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testGetGroup_GroupNotFound() {
        GroupsModel groupDto = new GroupsModel();
        groupDto.setGroupName("exampleGroup");
        groupDto.setUser("exampleUser");
        groupDto.setToken("exampleToken");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "The user does not have a group with that name");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(groupsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(groupDto.getUser(), groupDto.getToken())).thenReturn(true);
        when(groupsService.getGroup(groupDto.getGroupName(), groupDto.getUser())).thenReturn(null);

        ResponseEntity<SpecialResponse> actualResponse = groupsController.getGroup(groupDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(groupDto.getUser(), groupDto.getToken());
        verify(groupsService, times(1)).getGroup(groupDto.getGroupName(), groupDto.getUser());
        verifyNoMoreInteractions(usersService, groupsService);
    }

    @Test
    void testGetGroup_ValidData_GroupFound() {
        GroupsModel groupDto = new GroupsModel();
        groupDto.setGroupName("exampleGroup");
        groupDto.setUser("exampleUser");
        groupDto.setToken("exampleToken");

        GroupsEntity groupEntity = new GroupsEntity();
        groupEntity.setId(1);
        groupEntity.setGroupName("exampleGroup");
        groupEntity.setMembers("[\"Member1\", \"Member2\"]");
        groupEntity.setAdmin("exampleUser");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "OK");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(groupsController.specialResponse(groupDto, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(groupDto.getUser(), groupDto.getToken())).thenReturn(true);
        when(groupsService.getGroup(groupDto.getGroupName(), groupDto.getUser())).thenReturn(groupEntity);

        ResponseEntity<SpecialResponse> actualResponse = groupsController.getGroup(groupDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(groupDto.getUser(), groupDto.getToken());
        verify(groupsService, times(1)).getGroup(groupDto.getGroupName(), groupDto.getUser());
        verifyNoMoreInteractions(usersService, groupsService);
    }

    @Test
    void testEditGroup_MissingData() {
        GroupsModel groupDto = new GroupsModel();
        groupDto.setId(null);
        groupDto.setGroupName("");
        groupDto.setMembers(Collections.emptyList());
        groupDto.setUser("");
        groupDto.setToken("");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "All data is required to save a group");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(groupsController.specialResponse(null, expectedResponseJson), HttpStatus.BAD_GATEWAY);

        ResponseEntity<SpecialResponse> actualResponse = groupsController.editGroup(groupDto);

        assertEquals(HttpStatus.BAD_GATEWAY, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verifyNoInteractions(usersService, groupsService);
    }

    @Test
    void testEditGroup_UnauthorizedUser() {
        GroupsModel groupDto = new GroupsModel();
        groupDto.setId(1);
        groupDto.setGroupName("exampleGroup");
        groupDto.setMembers(Arrays.asList("Member1", "Member2"));
        groupDto.setUser("exampleUser");
        groupDto.setToken("exampleToken");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "Unauthorized user");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(groupsController.specialResponse(null, expectedResponseJson), HttpStatus.NOT_FOUND);

        when(usersService.checkToken(groupDto.getUser(), groupDto.getToken())).thenReturn(false);

        ResponseEntity<SpecialResponse> actualResponse = groupsController.editGroup(groupDto);

        assertEquals(HttpStatus.NOT_FOUND, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(groupDto.getUser(), groupDto.getToken());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testEditGroup_NonAdminUser() {
        GroupsModel groupDto = new GroupsModel();
        groupDto.setId(1);
        groupDto.setGroupName("exampleGroup");
        groupDto.setMembers(Arrays.asList("Member1", "Member2"));
        groupDto.setUser("exampleUser");
        groupDto.setToken("exampleToken");

        GroupsEntity groupEntity = new GroupsEntity();
        groupEntity.setId(1);
        groupEntity.setGroupName("exampleGroup");
        groupEntity.setMembers("[\"Member1\", \"Member2\"]");
        groupEntity.setAdmin("adminUser");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "The user is not the group administrator");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(groupsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(groupDto.getUser(), groupDto.getToken())).thenReturn(true);
        when(groupsService.findGroupsEntityById(groupDto.getId())).thenReturn(groupEntity);

        ResponseEntity<SpecialResponse> actualResponse = groupsController.editGroup(groupDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(groupDto.getUser(), groupDto.getToken());
        verify(groupsService, times(1)).findGroupsEntityById(groupDto.getId());
        verifyNoMoreInteractions(usersService, groupsService);
    }

    @Test
    void testEditGroup_ExistingGroupName() {
        GroupsModel groupDto = new GroupsModel();
        groupDto.setId(1);
        groupDto.setGroupName("exampleGroup");
        groupDto.setMembers(Arrays.asList("Member1", "Member2"));
        groupDto.setUser("exampleUser");
        groupDto.setToken("exampleToken");
        groupDto.setNewGroupName("newGroup");

        GroupsEntity groupEntity = new GroupsEntity();
        groupEntity.setId(1);
        groupEntity.setGroupName("exampleGroup");
        groupEntity.setMembers("[\"Member1\", \"Member2\"]");
        groupEntity.setAdmin("exampleUser");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "The user already has a group with that name");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(groupsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(groupDto.getUser(), groupDto.getToken())).thenReturn(true);
        when(groupsService.findGroupsEntityById(groupDto.getId())).thenReturn(groupEntity);
        when(groupsService.existsByGroupNameAndAdmin(groupDto.getNewGroupName().strip(), groupDto.getUser())).thenReturn(true);

        ResponseEntity<SpecialResponse> actualResponse = groupsController.editGroup(groupDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(groupDto.getUser(), groupDto.getToken());
        verify(groupsService, times(1)).findGroupsEntityById(groupDto.getId());
        verify(groupsService, times(1)).existsByGroupNameAndAdmin(groupDto.getNewGroupName().strip(), groupDto.getUser());
        verifyNoMoreInteractions(usersService, groupsService);
    }

    @Test
    void testEditGroup_ValidData_GroupEdited() {
        GroupsModel groupDto = new GroupsModel();
        groupDto.setId(1);
        groupDto.setGroupName("exampleGroup");
        groupDto.setMembers(Arrays.asList("Member1", "Member2"));
        groupDto.setUser("exampleUser");
        groupDto.setToken("exampleToken");
        groupDto.setNewGroupName("exampleNewGroup");

        GroupsEntity groupEntity = new GroupsEntity();
        groupEntity.setId(1);
        groupEntity.setGroupName("exampleGroup");
        groupEntity.setMembers("[\"Member1\", \"Member2\"]");
        groupEntity.setAdmin("exampleUser");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "Group edited successfully");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(groupsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(groupDto.getUser(), groupDto.getToken())).thenReturn(true);
        when(groupsService.findGroupsEntityById(groupDto.getId())).thenReturn(groupEntity);
        when(groupsService.existsByGroupNameAndAdmin(groupDto.getNewGroupName().strip(), groupDto.getUser())).thenReturn(false);

        ResponseEntity<SpecialResponse> actualResponse = groupsController.editGroup(groupDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(groupDto.getUser(), groupDto.getToken());
        verify(groupsService, times(1)).findGroupsEntityById(groupDto.getId());
        verify(groupsService, times(1)).existsByGroupNameAndAdmin(groupDto.getNewGroupName().strip(), groupDto.getUser());
        verify(groupsService, times(1)).saveGroup(groupEntity);
        verifyNoMoreInteractions(usersService, groupsService);
    }

    @Test
    void testGetGroupsByUser_MissingData_ReturnsBadRequest() {
        GroupsModel groupDto = new GroupsModel();
        groupDto.setUser("");
        groupDto.setToken("");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "User and token are required");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(groupsController.specialResponse(null, expectedResponseJson), HttpStatus.BAD_GATEWAY);

        ResponseEntity<SpecialResponse> actualResponse = groupsController.getGroupsByUser(groupDto);

        assertEquals(HttpStatus.BAD_GATEWAY, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verifyNoInteractions(usersService, groupsService);
    }

    @Test
    void testGetGroupsByUser_UnauthorizedUser_ReturnsUnauthorizedUser() {
        GroupsModel groupDto = new GroupsModel();
        groupDto.setUser("exampleUser");
        groupDto.setToken("exampleToken");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "Unauthorized user");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(groupsController.specialResponse(null, expectedResponseJson), HttpStatus.NOT_FOUND);

        when(usersService.checkToken(groupDto.getUser(), groupDto.getToken())).thenReturn(false);

        ResponseEntity<SpecialResponse> actualResponse = groupsController.getGroupsByUser(groupDto);

        assertEquals(HttpStatus.NOT_FOUND, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(groupDto.getUser(), groupDto.getToken());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testGetGroupsByUser_ValidUserWithoutGroups_ReturnsNoGroups() {
        GroupsModel groupDto = new GroupsModel();
        groupDto.setUser("exampleUser");
        groupDto.setToken("exampleToken");

        List<String> groupsList = Collections.emptyList();

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "The user is not part of any group");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(groupsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(groupDto.getUser(), groupDto.getToken())).thenReturn(true);
        when(groupsService.getGroupForEdit(groupDto.getUser())).thenReturn(groupsList);

        ResponseEntity<SpecialResponse> actualResponse = groupsController.getGroupsByUser(groupDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(groupDto.getUser(), groupDto.getToken());
        verify(groupsService, times(1)).getGroupForEdit(groupDto.getUser());
        verifyNoMoreInteractions(usersService, groupsService);
    }

    @Test
    void testGetGroupsByUser_ValidUserWithGroups_ReturnsGroupsList() {
        GroupsModel groupDto = new GroupsModel();
        groupDto.setUser("exampleUser");
        groupDto.setToken("exampleToken");

        List<String> groupsList = Arrays.asList("Group1", "Group2", "Group3");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "OK");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(groupsController.specialResponse(groupsList, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(groupDto.getUser(), groupDto.getToken())).thenReturn(true);
        when(groupsService.getGroupForEdit(groupDto.getUser())).thenReturn(groupsList);

        ResponseEntity<SpecialResponse> actualResponse = groupsController.getGroupsByUser(groupDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(groupDto.getUser(), groupDto.getToken());
        verify(groupsService, times(1)).getGroupForEdit(groupDto.getUser());
        verifyNoMoreInteractions(usersService, groupsService);
    }

    @Test
    void testDeleteGroup_MissingData_ReturnsBadRequest() {
        GroupsModel groupDto = new GroupsModel();
        groupDto.setGroupName("");
        groupDto.setUser("");
        groupDto.setToken("");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "Group name and administrator are required to delete a group");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(groupsController.specialResponse(null, expectedResponseJson), HttpStatus.BAD_GATEWAY);

        ResponseEntity<SpecialResponse> actualResponse = groupsController.deleteGroup(groupDto);

        assertEquals(HttpStatus.BAD_GATEWAY, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verifyNoInteractions(usersService, groupsService);
    }

    @Test
    void testDeleteGroup_UnauthorizedUser_ReturnsUnauthorizedUser() {
        GroupsModel groupDto = new GroupsModel();
        groupDto.setGroupName("exampleGroup");
        groupDto.setUser("exampleUser");
        groupDto.setToken("exampleToken");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "Unauthorized user");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(groupsController.specialResponse(null, expectedResponseJson), HttpStatus.NOT_FOUND);

        when(usersService.checkToken(groupDto.getUser(), groupDto.getToken())).thenReturn(false);

        ResponseEntity<SpecialResponse> actualResponse = groupsController.deleteGroup(groupDto);

        assertEquals(HttpStatus.NOT_FOUND, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(groupDto.getUser(), groupDto.getToken());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testDeleteGroup_GroupDoesNotExist_ReturnsGroupNotFound() {
        GroupsModel groupDto = new GroupsModel();
        groupDto.setGroupName("exampleGroup");
        groupDto.setUser("exampleUser");
        groupDto.setToken("exampleToken");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "The user does not have a group with that name");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(groupsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(groupDto.getUser(), groupDto.getToken())).thenReturn(true);
        when(groupsService.existsByGroupNameAndAdmin(groupDto.getGroupName().strip(), groupDto.getUser())).thenReturn(false);

        ResponseEntity<SpecialResponse> actualResponse = groupsController.deleteGroup(groupDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(groupDto.getUser(), groupDto.getToken());
        verify(groupsService, times(1)).existsByGroupNameAndAdmin(groupDto.getGroupName().strip(), groupDto.getUser());
        verifyNoMoreInteractions(usersService, groupsService);
    }

    @Test
    void testDeleteGroup_GroupExists_ReturnsGroupDeletedSuccessfully() {
        GroupsModel groupDto = new GroupsModel();
        groupDto.setGroupName("exampleGroup");
        groupDto.setUser("exampleUser");
        groupDto.setToken("exampleToken");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "Group deleted successfully");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(groupsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(groupDto.getUser(), groupDto.getToken())).thenReturn(true);
        when(groupsService.existsByGroupNameAndAdmin(groupDto.getGroupName().strip(), groupDto.getUser())).thenReturn(true);

        ResponseEntity<SpecialResponse> actualResponse = groupsController.deleteGroup(groupDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(groupDto.getUser(), groupDto.getToken());
        verify(groupsService, times(1)).existsByGroupNameAndAdmin(groupDto.getGroupName().strip(), groupDto.getUser());
        verify(groupsService, times(1)).deleteGroup(groupDto.getGroupName().strip(), groupDto.getUser());
        verifyNoMoreInteractions(usersService, groupsService);
    }

    @Test
    void testGetAllGroupsData_NoGroupsExist_ReturnsNoGroupsInDatabase() {
        List<GroupsEntity> groupsList = new ArrayList<>();

        List<GroupsModel> expectedGroupsDtoList = new ArrayList<>();

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "There are no groups in the database");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(groupsController.specialResponse(expectedGroupsDtoList, expectedResponseJson), HttpStatus.OK);

        when(groupsService.getAllGroupsData()).thenReturn(groupsList);

        ResponseEntity<SpecialResponse> actualResponse = groupsController.getAllGroupsData();

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(groupsService, times(1)).getAllGroupsData();
        verifyNoMoreInteractions(groupsService);
    }

    @Test
    void testGetAllGroupsData_GroupsExist_ReturnsGroupsData() {
        List<GroupsEntity> groupsList = new ArrayList<>();
        GroupsEntity groupEntity1 = new GroupsEntity();
        groupEntity1.setId(1);
        groupEntity1.setGroupName("Group 1");
        groupEntity1.setMembers("[\"User 1\", \"User 2\"]");
        groupEntity1.setAdmin("Admin 1");
        groupsList.add(groupEntity1);

        List<GroupsModel> expectedGroupsDtoList = new ArrayList<>();
        GroupsModel groupDto1 = new GroupsModel();
        groupDto1.setId(1);
        groupDto1.setGroupName("Group 1");
        groupDto1.setMembers(Arrays.asList("User 1", "User 2"));
        groupDto1.setAdmin("Admin 1");
        expectedGroupsDtoList.add(groupDto1);

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "OK");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(groupsController.specialResponse(expectedGroupsDtoList, expectedResponseJson), HttpStatus.OK);

        when(groupsService.getAllGroupsData()).thenReturn(groupsList);

        ResponseEntity<SpecialResponse> actualResponse = groupsController.getAllGroupsData();

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(groupsService, times(1)).getAllGroupsData();
        verifyNoMoreInteractions(groupsService);
    }

}

