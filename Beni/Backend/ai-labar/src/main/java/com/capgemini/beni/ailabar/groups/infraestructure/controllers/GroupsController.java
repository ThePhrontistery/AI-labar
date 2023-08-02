package com.capgemini.beni.ailabar.groups.infraestructure.controllers;

import com.capgemini.beni.ailabar.groups.domain.models.GroupsModel;
import com.capgemini.beni.ailabar.groups.infraestructure.entities.GroupsEntity;
import com.capgemini.beni.ailabar.groups.application.services.GroupsService;
import com.capgemini.beni.ailabar.users.application.services.UsersService;
import com.capgemini.beni.ailabar.commons.utils.SpecialResponse;
import com.capgemini.beni.ailabar.commons.adapters.out.SpecialResponseInterface;
import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;

@RestController
@RequestMapping("/groups")
public class GroupsController implements SpecialResponseInterface {
    private final GroupsService groupsService;
    private final UsersService usersService;

    @Autowired
    public GroupsController(GroupsService groupsService, UsersService usersService) {
        this.groupsService = groupsService;
        this.usersService = usersService;
    }

    @PostMapping("/createGroup")
    public ResponseEntity<SpecialResponse> createGroup(@RequestBody GroupsModel groupModel) {
        JSONObject responseJson = new JSONObject();

        if(groupModel.getGroupName().isBlank() || groupModel.getMembers().isEmpty() || groupModel.getUser().isBlank() || groupModel.getToken().isBlank()) {
            responseJson.put("message", "All data is required to save a group");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.BAD_GATEWAY);
        }

        if(Boolean.FALSE.equals(usersService.checkToken(groupModel.getUser(), groupModel.getToken()))) {
            responseJson.put("message", "Unauthorized user");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.NOT_FOUND);
        }

        if(Boolean.TRUE.equals(groupsService.existsByGroupNameAndAdmin(groupModel.getGroupName().strip(), groupModel.getUser()))) {
            responseJson.put("message", "The user already has a group with that name");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
        }

        GroupsEntity groupEntity = new GroupsEntity(groupModel);
        groupEntity.setAdmin(groupModel.getUser().strip());
        groupEntity.setMembers(new Gson().toJson(groupModel.getMembers()));
        groupsService.saveGroup(groupEntity);
        responseJson.put("message", "Group created successfully");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    @PostMapping("/getGroup")
    public ResponseEntity<SpecialResponse> getGroup(@RequestBody GroupsModel groupModel) {
        JSONObject responseJson = new JSONObject();

        if(groupModel.getGroupName().isBlank() || groupModel.getUser().isBlank() || groupModel.getToken().isBlank()) {
            responseJson.put("message", "Group name and user are required");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.BAD_GATEWAY);
        }

        if(Boolean.FALSE.equals(usersService.checkToken(groupModel.getUser(), groupModel.getToken()))) {
            responseJson.put("message", "Unauthorized user");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.NOT_FOUND);
        }

        GroupsEntity groupEntity = groupsService.getGroup(groupModel.getGroupName(), groupModel.getUser());

        if(groupEntity == null) {
            responseJson.put("message", "The user does not have a group with that name");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
        }

        GroupsModel matchedGroup = new GroupsModel();
        matchedGroup.setId(groupEntity.getId());
        matchedGroup.setGroupName(groupEntity.getGroupName());

        Gson gson = new Gson();
        Type listType = new TypeToken<List<String>>() {}.getType();
        matchedGroup.setMembers(gson.fromJson(groupEntity.getMembers(), listType));
        matchedGroup.setAdmin(groupEntity.getAdmin());

        responseJson.put("message", "OK");
        return new ResponseEntity<>(specialResponse(matchedGroup, responseJson), HttpStatus.OK);
    }

    @PutMapping("/editGroup")
    public ResponseEntity<SpecialResponse> editGroup(@RequestBody GroupsModel groupModel) {
        JSONObject responseJson = new JSONObject();

        if(groupModel.getId() == null || groupModel.getGroupName().isBlank() || groupModel.getMembers().isEmpty() || groupModel.getUser().isBlank() || groupModel.getToken().isBlank()) {
            responseJson.put("message", "All data is required to save a group");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.BAD_GATEWAY);
        }

        if(Boolean.FALSE.equals(usersService.checkToken(groupModel.getUser(), groupModel.getToken()))) {
            responseJson.put("message", "Unauthorized user");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.NOT_FOUND);
        }

        GroupsEntity groupEntity = groupsService.findGroupsEntityById(groupModel.getId());

        if(!groupEntity.getAdmin().equals(groupModel.getUser())) {
            responseJson.put("message", "The user is not the group administrator");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
        }

        groupEntity.setMembers(new Gson().toJson(groupModel.getMembers()));

        if(groupModel.getNewGroupName() != null && !groupModel.getNewGroupName().isBlank()) {
            if(Boolean.TRUE.equals(groupsService.existsByGroupNameAndAdmin(groupModel.getNewGroupName().strip(), groupModel.getUser()))) {
                responseJson.put("message", "The user already has a group with that name");
                return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
            } else {
                groupEntity.setGroupName(groupModel.getNewGroupName().strip());
            }
        }
        groupsService.saveGroup(groupEntity);
        responseJson.put("message", "Group edited successfully");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    @PostMapping("/getGroupsByUser")
    public ResponseEntity<SpecialResponse> getGroupsByUser(@RequestBody GroupsModel groupModel) {
        JSONObject responseJson = new JSONObject();

        if(groupModel.getUser().isBlank() || groupModel.getToken().isBlank()) {
            responseJson.put("message", "User and token are required");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.BAD_GATEWAY);
        }

        if(Boolean.FALSE.equals(usersService.checkToken(groupModel.getUser(), groupModel.getToken()))) {
            responseJson.put("message", "Unauthorized user");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.NOT_FOUND);
        }

        List<String> groupsList = groupsService.getGroupForEdit(groupModel.getUser());
        if(groupsList.isEmpty()) {
            responseJson.put("message", "The user is not part of any group");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
        }

        responseJson.put("message", "OK");
        return new ResponseEntity<>(specialResponse(groupsList, responseJson), HttpStatus.OK);
    }

    @DeleteMapping("/deleteGroup")
    public ResponseEntity<SpecialResponse> deleteGroup(@RequestBody GroupsModel groupModel) {
        JSONObject responseJson = new JSONObject();

        if(groupModel.getGroupName().isBlank() || groupModel.getUser().isBlank() || groupModel.getToken().isBlank()) {
            responseJson.put("message", "Group name and administrator are required to delete a group");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.BAD_GATEWAY);
        }

        if(Boolean.FALSE.equals(usersService.checkToken(groupModel.getUser(), groupModel.getToken()))) {
            responseJson.put("message", "Unauthorized user");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.NOT_FOUND);
        }

        if(Boolean.FALSE.equals(groupsService.existsByGroupNameAndAdmin(groupModel.getGroupName().strip(), groupModel.getUser()))) {
            responseJson.put("message", "The user does not have a group with that name");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
        }

        groupsService.deleteGroup(groupModel.getGroupName().strip(), groupModel.getUser());
        responseJson.put("message", "Group deleted successfully");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    /* Inicio métodos sólo para pruebas */
    @GetMapping("/getAllGroupsData")
    public ResponseEntity<SpecialResponse> getAllGroupsData() {
        JSONObject responseJson = new JSONObject();

        List<GroupsEntity> groupsList = groupsService.getAllGroupsData();
        List<GroupsModel> groupsModelList = new ArrayList<>();

        if (groupsList.isEmpty()) {
            responseJson.put("message", "There are no groups in the database");
            return new ResponseEntity<>(specialResponse(groupsModelList, responseJson), HttpStatus.OK);
        }

        for (GroupsEntity groupEntity : groupsList) {
            GroupsModel groupModel = new GroupsModel();
            groupModel.setId(groupEntity.getId());
            groupModel.setGroupName(groupEntity.getGroupName());

            Gson gson = new Gson();
            Type listType = new TypeToken<List<String>>() {}.getType();
            groupModel.setMembers(gson.fromJson(groupEntity.getMembers(), listType));
            groupModel.setAdmin(groupEntity.getAdmin());

            groupsModelList.add(groupModel);
        }

        responseJson.put("message", "OK");
        return new ResponseEntity<>(specialResponse(groupsModelList, responseJson), HttpStatus.OK);
    }
    /* Fin métodos sólo para pruebas */
}
