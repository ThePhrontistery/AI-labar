package com.capgemini.ailabar.groups.infraestructure.controllers;

import com.capgemini.ailabar.commons.adapters.out.SpecialResponseInterface;
import com.capgemini.ailabar.commons.utils.SpecialResponse;
import com.capgemini.ailabar.groups.domain.exceptions.CreateGroupException;
import com.capgemini.ailabar.groups.domain.exceptions.EditGroupException;
import com.capgemini.ailabar.groups.domain.exceptions.GetGroupException;
import com.capgemini.ailabar.groups.domain.exceptions.GetGroupsByUserException;
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
        GroupsEntity matchedGroup = groupsService.getGroup(groupModel);
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

//    @DeleteMapping("/deleteGroup")
//    public ResponseEntity<SpecialResponse> deleteGroup(@RequestBody GroupsModel groupModel) {
//        JSONObject responseJson = new JSONObject();
//
//        if(groupModel.getGroupName().isBlank() || groupModel.getUser().isBlank() || groupModel.getToken().isBlank()) {
//            responseJson.put("message", "Group name and administrator are required to delete a group");
//            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.BAD_GATEWAY);
//        }
//
//        if(Boolean.FALSE.equals(usersService.checkAuthorization(groupModel.getUser(), groupModel.getToken()))) {
//            responseJson.put("message", "Unauthorized user");
//            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.NOT_FOUND);
//        }
//
//        if(Boolean.FALSE.equals(groupsService.existsByGroupNameAndAdmin(groupModel.getGroupName().strip(), groupModel.getUser()))) {
//            responseJson.put("message", "The user does not have a group with that name");
//            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
//        }
//
//        groupsService.deleteGroup(groupModel.getGroupName().strip(), groupModel.getUser());
//        responseJson.put("message", "Group deleted successfully");
//        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
//    }
//
//    /* Inicio métodos sólo para pruebas */
//    @GetMapping("/getAllGroupsData")
//    public ResponseEntity<SpecialResponse> getAllGroupsData() {
//        JSONObject responseJson = new JSONObject();
//
//        List<GroupsEntity> groupsList = groupsService.getAllGroupsData();
//        List<GroupsModel> groupsModelList = new ArrayList<>();
//
//        if (groupsList.isEmpty()) {
//            responseJson.put("message", "There are no groups in the database");
//            return new ResponseEntity<>(specialResponse(groupsModelList, responseJson), HttpStatus.OK);
//        }
//
//        for (GroupsEntity groupEntity : groupsList) {
//            GroupsModel groupModel = new GroupsModel();
//            groupModel.setId(groupEntity.getId());
//            groupModel.setGroupName(groupEntity.getGroupName());
//
//            Gson gson = new Gson();
//            Type listType = new TypeToken<List<String>>() {}.getType();
//            groupModel.setMembers(gson.fromJson(groupEntity.getMembers(), listType));
//            groupModel.setAdmin(groupEntity.getAdmin());
//
//            groupsModelList.add(groupModel);
//        }
//
//        responseJson.put("message", "OK");
//        return new ResponseEntity<>(specialResponse(groupsModelList, responseJson), HttpStatus.OK);
//    }
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
}
