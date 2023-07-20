package com.capgemini.beni.ailabar.controller;

import com.capgemini.beni.ailabar.dto.GroupsDto;
import com.capgemini.beni.ailabar.dto.UsersDto;
import com.capgemini.beni.ailabar.entity.GroupsEntity;
import com.capgemini.beni.ailabar.service.GroupsService;
import com.capgemini.beni.ailabar.service.UsersService;
import com.capgemini.beni.ailabar.utils.SpecialResponse;
import com.capgemini.beni.ailabar.utils.SpecialResponseInterface;
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
    public ResponseEntity<SpecialResponse> createGroup(@RequestBody GroupsDto groupDto) {
        JSONObject responseJson = new JSONObject();

        if(groupDto.getGroupName().isBlank() || groupDto.getMembers().isEmpty() || groupDto.getUser().isBlank() || groupDto.getToken().isBlank()) {
            responseJson.put("message", "All data is required to save a group");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.BAD_GATEWAY);
        }

        if(Boolean.FALSE.equals(usersService.checkToken(groupDto.getUser(), groupDto.getToken()))) {
            responseJson.put("message", "Unauthorized user");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.NOT_FOUND);
        }

        if(Boolean.TRUE.equals(groupsService.existsByGroupNameAndAdmin(groupDto.getGroupName().strip(), groupDto.getUser()))) {
            responseJson.put("message", "The user already has a group with that name");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
        }

        GroupsEntity groupEntity = new GroupsEntity(groupDto);
        groupEntity.setAdmin(groupDto.getUser().strip());
        groupEntity.setMembers(new Gson().toJson(groupDto.getMembers()));
        groupsService.saveGroup(groupEntity);
        responseJson.put("message", "Group created successfully");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    @PostMapping("/getGroup")
    public ResponseEntity<SpecialResponse> getGroup(@RequestBody GroupsDto groupDto) {
        JSONObject responseJson = new JSONObject();

        if(groupDto.getGroupName().isBlank() || groupDto.getUser().isBlank() || groupDto.getToken().isBlank()) {
            responseJson.put("message", "Group name and user are required");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.BAD_GATEWAY);
        }

        if(Boolean.FALSE.equals(usersService.checkToken(groupDto.getUser(), groupDto.getToken()))) {
            responseJson.put("message", "Unauthorized user");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.NOT_FOUND);
        }

        GroupsEntity groupEntity = groupsService.getGroup(groupDto.getGroupName(), groupDto.getUser());

        if(groupEntity == null) {
            responseJson.put("message", "The user does not have a group with that name");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
        }

        GroupsDto matchedGroup = new GroupsDto();
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
    public ResponseEntity<SpecialResponse> editGroup(@RequestBody GroupsDto groupDto) {
        JSONObject responseJson = new JSONObject();

        if(groupDto.getId() == null || groupDto.getGroupName().isBlank() || groupDto.getMembers().isEmpty() || groupDto.getUser().isBlank() || groupDto.getToken().isBlank()) {
            responseJson.put("message", "All data is required to save a group");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.BAD_GATEWAY);
        }

        if(Boolean.FALSE.equals(usersService.checkToken(groupDto.getUser(), groupDto.getToken()))) {
            responseJson.put("message", "Unauthorized user");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.NOT_FOUND);
        }

        GroupsEntity groupEntity = groupsService.findGroupsEntityById(groupDto.getId());

        if(!groupEntity.getAdmin().equals(groupDto.getUser())) {
            responseJson.put("message", "The user is not the group administrator");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
        }

        groupEntity.setMembers(new Gson().toJson(groupDto.getMembers()));

        if(groupDto.getNewGroupName() != null && !groupDto.getNewGroupName().isBlank()) {
            if(Boolean.TRUE.equals(groupsService.existsByGroupNameAndAdmin(groupDto.getNewGroupName().strip(), groupDto.getUser()))) {
                responseJson.put("message", "The user already has a group with that name");
                return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
            } else {
                groupEntity.setGroupName(groupDto.getNewGroupName().strip());
            }
        }
        groupsService.saveGroup(groupEntity);
        responseJson.put("message", "Group edited successfully");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    @PostMapping("/getGroupsByUser")
    public ResponseEntity<SpecialResponse> getGroupsByUser(@RequestBody GroupsDto groupDto) {
        JSONObject responseJson = new JSONObject();

        if(groupDto.getUser().isBlank() || groupDto.getToken().isBlank()) {
            responseJson.put("message", "User and token are required");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.BAD_GATEWAY);
        }

        if(Boolean.FALSE.equals(usersService.checkToken(groupDto.getUser(), groupDto.getToken()))) {
            responseJson.put("message", "Unauthorized user");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.NOT_FOUND);
        }

        List<String> groupsList = groupsService.getGroupForEdit(groupDto.getUser());
        if(groupsList.isEmpty()) {
            responseJson.put("message", "The user is not part of any group");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
        }

        responseJson.put("message", "OK");
        return new ResponseEntity<>(specialResponse(groupsList, responseJson), HttpStatus.OK);
    }

    @DeleteMapping("/deleteGroup")
    public ResponseEntity<SpecialResponse> deleteGroup(@RequestBody GroupsDto groupDto) {
        JSONObject responseJson = new JSONObject();

        if(groupDto.getGroupName().isBlank() || groupDto.getUser().isBlank() || groupDto.getToken().isBlank()) {
            responseJson.put("message", "Group name and administrator are required to delete a group");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.BAD_GATEWAY);
        }

        if(Boolean.FALSE.equals(usersService.checkToken(groupDto.getUser(), groupDto.getToken()))) {
            responseJson.put("message", "Unauthorized user");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.NOT_FOUND);
        }

        if(Boolean.FALSE.equals(groupsService.existsByGroupNameAndAdmin(groupDto.getGroupName().strip(), groupDto.getUser()))) {
            responseJson.put("message", "The user does not have a group with that name");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
        }

        groupsService.deleteGroup(groupDto.getGroupName().strip(), groupDto.getUser());
        responseJson.put("message", "Group deleted successfully");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    /* Inicio métodos sólo para pruebas */
    @GetMapping("/getAllGroupsData")
    public ResponseEntity<SpecialResponse> getAllGroupsData() {
        JSONObject responseJson = new JSONObject();

        List<GroupsEntity> groupsList = groupsService.getAllGroupsData();
        List<GroupsDto> groupsDtoList = new ArrayList<>();

        if (groupsList.isEmpty()) {
            responseJson.put("message", "There are no groups in the database");
            return new ResponseEntity<>(specialResponse(groupsDtoList, responseJson), HttpStatus.OK);
        }

        for (GroupsEntity groupEntity : groupsList) {
            GroupsDto groupDto = new GroupsDto();
            groupDto.setId(groupEntity.getId());
            groupDto.setGroupName(groupEntity.getGroupName());

            Gson gson = new Gson();
            Type listType = new TypeToken<List<String>>() {}.getType();
            groupDto.setMembers(gson.fromJson(groupEntity.getMembers(), listType));
            groupDto.setAdmin(groupEntity.getAdmin());

            groupsDtoList.add(groupDto);
        }

        responseJson.put("message", "OK");
        return new ResponseEntity<>(specialResponse(groupsDtoList, responseJson), HttpStatus.OK);
    }
    /* Fin métodos sólo para pruebas */
}
