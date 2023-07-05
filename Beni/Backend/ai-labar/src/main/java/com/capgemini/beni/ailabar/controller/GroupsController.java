package com.capgemini.beni.ailabar.controller;

import com.capgemini.beni.ailabar.dto.GroupsDto;
import com.capgemini.beni.ailabar.dto.UsersDto;
import com.capgemini.beni.ailabar.entity.GroupsEntity;
import com.capgemini.beni.ailabar.service.GroupsService;
import com.capgemini.beni.ailabar.service.UsersService;
import com.capgemini.beni.ailabar.utils.SpecialResponse;
import com.capgemini.beni.ailabar.utils.SpecialResponseInterface;
import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

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

    @PostMapping("/getUser")
    public ResponseEntity<String> getUser(@RequestBody UsersDto userDto) {
        JSONObject responseJson = new JSONObject();

        if(userDto.getUser().isBlank()) {
            responseJson.put("message", "User name is required");
            return new ResponseEntity<>(responseJson.toString(), HttpStatus.BAD_GATEWAY);
        }

        if (Boolean.FALSE.equals(usersService.checkUser(userDto.getUser()))) {
            responseJson.put("message", "The user does not exist");
            return new ResponseEntity<>(responseJson.toString(), HttpStatus.NOT_FOUND);
        }

        responseJson.put("message", usersService.findByUser(userDto.getUser()).getUser());
        return new ResponseEntity<>(responseJson.toString(), HttpStatus.OK);
    }

    @PostMapping("/createGroup")
    public ResponseEntity<String> createGroup(@RequestBody GroupsDto groupDto) {
        JSONObject responseJson = new JSONObject();

        if(groupDto.getGroupName().isBlank() || groupDto.getMembers().isBlank() || groupDto.getAdmin().isBlank()) {
            responseJson.put("message", "All data is required to save a group");
            return new ResponseEntity<>(responseJson.toString(), HttpStatus.BAD_GATEWAY);
        }

        if (Boolean.FALSE.equals(usersService.checkUser(groupDto.getAdmin()))) {
            responseJson.put("message", "The admin does not exist");
            return new ResponseEntity<>(responseJson.toString(), HttpStatus.NOT_FOUND);
        }

        if(Boolean.TRUE.equals(groupsService.existsByGroupNameAndAdmin(groupDto.getGroupName().strip(), groupDto.getAdmin()))) {
            responseJson.put("message", "The administrator already has a group with that name");
            return new ResponseEntity<>(responseJson.toString(), HttpStatus.OK);
        }

        GroupsEntity groupEntity = new GroupsEntity(groupDto);
        groupsService.saveGroup(groupEntity);
        responseJson.put("message", "Group created successfully");
        return new ResponseEntity<>(responseJson.toString(), HttpStatus.OK);
    }

    @PutMapping("/editGroup")
    public ResponseEntity<String> editGroup(@RequestBody GroupsDto groupDto) {
        JSONObject responseJson = new JSONObject();

        if(groupDto.getId() == null || groupDto.getGroupName().isBlank() || groupDto.getMembers().isBlank() || groupDto.getAdmin().isBlank()) {
            responseJson.put("message", "All data is required to save a group");
            return new ResponseEntity<>(responseJson.toString(), HttpStatus.BAD_GATEWAY);
        }

        if (Boolean.FALSE.equals(usersService.checkUser(groupDto.getAdmin()))) {
            responseJson.put("message", "The admin does not exist");
            return new ResponseEntity<>(responseJson.toString(), HttpStatus.NOT_FOUND);
        }

        GroupsEntity groupEntity = groupsService.findGroupsEntityById(groupDto.getId());

        if(!groupEntity.getAdmin().equals(groupDto.getAdmin())) {
            responseJson.put("message", "The user is not the group administrator");
            return new ResponseEntity<>(responseJson.toString(), HttpStatus.OK);
        }

        if(!groupDto.getNewGroupName().isBlank()) {
            if(Boolean.TRUE.equals(groupsService.existsByGroupNameAndAdmin(groupDto.getNewGroupName().strip(), groupDto.getAdmin()))) {
                responseJson.put("message", "The administrator already has a group with that name");
                return new ResponseEntity<>(responseJson.toString(), HttpStatus.OK);
            } else {
                groupEntity.setGroupName(groupDto.getNewGroupName().strip());
            }
        }
        groupsService.saveGroup(groupEntity);
        responseJson.put("message", "Group created successfully");
        return new ResponseEntity<>(responseJson.toString(), HttpStatus.OK);
    }

    @PostMapping("/getGroup")
    public ResponseEntity<SpecialResponse> getGroup(@RequestBody GroupsDto groupDto) {
        JSONObject responseJson = new JSONObject();

        if(groupDto.getGroupName().isBlank() || groupDto.getAdmin().isBlank()) {
            responseJson.put("message", "Group name and admin are required");
            return new ResponseEntity<>(specialResponse(null, responseJson.toString()), HttpStatus.BAD_GATEWAY);
        }

        GroupsEntity groupEntity = groupsService.getGroup(groupDto.getGroupName(), groupDto.getAdmin());

        if(groupEntity == null) {
            responseJson.put("message", "The admin does not have a group with that name");
            return new ResponseEntity<>(specialResponse(null, responseJson.toString()), HttpStatus.OK);
        }

        responseJson.put("message", "OK");
        return new ResponseEntity<>(specialResponse(groupEntity, responseJson.toString()), HttpStatus.OK);
    }

    @PostMapping("/getGroupForEdit")
    public ResponseEntity<SpecialResponse> getGroupForEdit(@RequestBody GroupsDto groupDto) {
        JSONObject responseJson = new JSONObject();

        if(groupDto.getAdmin().isBlank()) {
            responseJson.put("message", "Admin is required");
            return new ResponseEntity<>(specialResponse(null, responseJson.toString()), HttpStatus.BAD_GATEWAY);
        }

        List<String> groupsList = groupsService.getGroupForEdit(groupDto.getAdmin());
        if(groupsList.isEmpty()) {
            responseJson.put("message", "The admin is not part of any group");
            return new ResponseEntity<>(specialResponse(null, responseJson.toString()), HttpStatus.OK);
        }

        responseJson.put("message", "OK");
        return new ResponseEntity<>(specialResponse(groupsList, responseJson.toString()), HttpStatus.OK);
    }

    @DeleteMapping("/deleteGroup")
    public ResponseEntity<String> deleteGroup(@RequestBody GroupsDto groupDto) {
        JSONObject responseJson = new JSONObject();

        if(groupDto.getGroupName().isBlank() || groupDto.getAdmin().isBlank()) {
            responseJson.put("message", "Group name and administrator are required to delete a group");
            return new ResponseEntity<>(responseJson.toString(), HttpStatus.BAD_GATEWAY);
        }

        if(Boolean.FALSE.equals(groupsService.existsByGroupNameAndAdmin(groupDto.getGroupName().strip(), groupDto.getAdmin()))) {
            responseJson.put("message", "The administrator does not have a group with that name");
            return new ResponseEntity<>(responseJson.toString(), HttpStatus.OK);
        }

        groupsService.deleteGroup(groupDto.getGroupName().strip(), groupDto.getAdmin());
        responseJson.put("message", "Group deleted successfully");
        return new ResponseEntity<>(responseJson.toString(), HttpStatus.OK);
    }

    /* Inicio métodos sólo para pruebas */
    @GetMapping("/getAllGroupsData")
    public ResponseEntity<SpecialResponse> getAllGroupsData() {
        JSONObject responseJson = new JSONObject();

        List<GroupsEntity> groupsList = groupsService.getAllGroupsData();

        if(groupsList.isEmpty()) {
            responseJson.put("message", "There are no groups in database");
            return new ResponseEntity<>(specialResponse(groupsList, responseJson.toString()), HttpStatus.OK);
        }

        responseJson.put("message", "OK");
        return new ResponseEntity<>(specialResponse(groupsList, responseJson.toString()), HttpStatus.OK);
    }
    /* Fin métodos sólo para pruebas */
}
