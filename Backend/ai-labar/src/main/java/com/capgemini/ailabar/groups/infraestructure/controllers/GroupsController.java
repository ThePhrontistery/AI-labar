package com.capgemini.ailabar.groups.infraestructure.controllers;

import com.capgemini.ailabar.commons.adapters.out.SpecialResponseInterface;
import com.capgemini.ailabar.commons.utils.SpecialResponse;
import com.capgemini.ailabar.groups.domain.exceptions.*;
import com.capgemini.ailabar.groups.infraestructure.entities.GroupsEntity;
import com.capgemini.ailabar.groups.domain.models.GroupsModel;
import com.capgemini.ailabar.groups.application.services.GroupsService;
import com.capgemini.ailabar.users.application.services.UsersService;
import com.capgemini.ailabar.users.domain.exceptions.CreateUserException;
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
import java.util.Map;

@RestController
@RequestMapping("/groups")
public class GroupsController implements SpecialResponseInterface {
    private final GroupsService groupsService;

    @Autowired
    public GroupsController(GroupsService groupsService) {
        this.groupsService = groupsService;
    }

    @PostMapping("/createGroup")
    public ResponseEntity<SpecialResponse> createGroup(@RequestBody GroupsModel groupModel) {
        JSONObject responseJson = new JSONObject();
        groupsService.createGroup(groupModel);
        responseJson.put("message", "Group created successfully");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    @PostMapping("/getGroup")
    public ResponseEntity<SpecialResponse> getGroup(@RequestBody GroupsModel groupModel) {
        JSONObject responseJson = new JSONObject();
        GroupsModel matchedGroup = groupsService.getGroup(groupModel);
        responseJson.put("message", "OK");
        return new ResponseEntity<>(specialResponse(matchedGroup, responseJson), HttpStatus.OK);
    }

    @PutMapping("/editGroup")
    public ResponseEntity<SpecialResponse> editGroup(@RequestBody GroupsModel groupModel) {
        JSONObject responseJson = new JSONObject();
        groupsService.editGroup(groupModel);
        responseJson.put("message", "Group edited successfully");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    @PostMapping("/getGroupsByUser")
    public ResponseEntity<SpecialResponse> getGroupsByUser(@RequestBody GroupsModel groupModel) {
        JSONObject responseJson = new JSONObject();
        List<String> groupsList = groupsService.getGroupsByUser(groupModel);
        responseJson.put("message", "OK");
        return new ResponseEntity<>(specialResponse(groupsList, responseJson), HttpStatus.OK);
    }

    @DeleteMapping("/deleteGroup")
    public ResponseEntity<SpecialResponse> deleteGroup(@RequestBody GroupsModel groupModel) {
        JSONObject responseJson = new JSONObject();
        groupsService.deleteGroup(groupModel);
        responseJson.put("message", "Group deleted successfully");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    /* Inicio métodos sólo para pruebas */
    @GetMapping("/getGroupsDatabase")
    public ResponseEntity<SpecialResponse> getGroupsDatabase() {
        JSONObject responseJson = new JSONObject();
        List<GroupsModel> groupsList = groupsService.getGroupsDatabase();
        responseJson.put("message", "OK");
        return new ResponseEntity<>(specialResponse(groupsList, responseJson), HttpStatus.OK);
    }
    /* Fin métodos sólo para pruebas */

    @ExceptionHandler(CreateGroupException.class)
    ResponseEntity<SpecialResponse> handlerCreateGroupException (CreateGroupException createGroupException){
        JSONObject responseJson = new JSONObject();
        responseJson.put("message", createGroupException.getMessage());
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.INTERNAL_SERVER_ERROR);
    }

    @ExceptionHandler(GetGroupException.class)
    ResponseEntity<SpecialResponse> handlerGetGroupException (GetGroupException getGroupException){
        JSONObject responseJson = new JSONObject();
        responseJson.put("message", getGroupException.getMessage());
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.INTERNAL_SERVER_ERROR);
    }

    @ExceptionHandler(EditGroupException.class)
    ResponseEntity<SpecialResponse> handlerEditGroupException (EditGroupException editGroupException){
        JSONObject responseJson = new JSONObject();
        responseJson.put("message", editGroupException.getMessage());
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.INTERNAL_SERVER_ERROR);
    }

    @ExceptionHandler(GetGroupsByUserException.class)
    ResponseEntity<SpecialResponse> handlerGetGroupsByUserException (GetGroupsByUserException getGroupsByUserException){
        JSONObject responseJson = new JSONObject();
        responseJson.put("message", getGroupsByUserException.getMessage());
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.INTERNAL_SERVER_ERROR);
    }

    @ExceptionHandler(DeleteGroupException.class)
    ResponseEntity<SpecialResponse> handlerDeleteGroupException (DeleteGroupException deleteGroupException){
        JSONObject responseJson = new JSONObject();
        responseJson.put("message", deleteGroupException.getMessage());
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.INTERNAL_SERVER_ERROR);
    }

    @ExceptionHandler(GetGroupsDatabaseException.class)
    ResponseEntity<SpecialResponse> handlerGetGroupsDatabaseException (GetGroupsDatabaseException getGroupsDatabaseException){
        JSONObject responseJson = new JSONObject();
        responseJson.put("message", getGroupsDatabaseException.getMessage());
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.INTERNAL_SERVER_ERROR);
    }
}
