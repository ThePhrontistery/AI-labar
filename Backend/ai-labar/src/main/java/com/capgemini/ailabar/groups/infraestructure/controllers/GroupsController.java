package com.capgemini.ailabar.groups.infraestructure.controllers;

import com.capgemini.ailabar.commons.adapters.out.SpecialResponseInterface;
import com.capgemini.ailabar.commons.utils.SpecialResponse;
import com.capgemini.ailabar.groups.domain.exceptions.*;
import com.capgemini.ailabar.groups.domain.models.GroupsModel;
import com.capgemini.ailabar.groups.application.services.GroupsService;
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

    @Autowired
    public GroupsController(GroupsService groupsService) {
        this.groupsService = groupsService;
    }

    /*
     * CREATES A GROUP IN THE DATABASE:
     * 1. Members must be sent as an array of strings.
     * 2. The user creating the group will be the group's admin.
     * 3. The user creating the group cannot assign themselves as a member of the group; they are already a member by default.
     */
    @PostMapping("/createGroup")
    public ResponseEntity<SpecialResponse> createGroup(@RequestBody GroupsModel groupModel) {
        JSONObject responseJson = new JSONObject();
        groupsService.createGroup(groupModel);
        responseJson.put("message", "Group created successfully");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    /*
     * RETURNS GROUP DATA IF THERE IS AN EXACT MATCH IN THE NAME:
     * 1. Retrieves group data if the user has a group that exactly matches the received 'groupName.'
     */
    @PostMapping("/getGroup")
    public ResponseEntity<SpecialResponse> getGroup(@RequestBody GroupsModel groupModel) {
        JSONObject responseJson = new JSONObject();
        GroupsModel matchedGroup = groupsService.getGroup(groupModel);
        responseJson.put("message", "OK");
        return new ResponseEntity<>(specialResponse(matchedGroup, responseJson), HttpStatus.OK);
    }

    /*
     * EDITS A GROUP IN THE DATABASE:
     * 1. All necessary data for editing the group can be obtained using 'getGroup(),' and it's only necessary to send the 'newGroupName' attribute if the group's name changes.
     * 2. Group members can be modified, but they do not require an additional variable for that purpose.
     * 3. The use of 'newGroupName' is mandatory only if the group's name has been modified, not for modifying the group's members.
     */
    @PutMapping("/editGroup")
    public ResponseEntity<SpecialResponse> editGroup(@RequestBody GroupsModel groupModel) {
        JSONObject responseJson = new JSONObject();
        groupsService.editGroup(groupModel);
        responseJson.put("message", "Group edited successfully");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    /*
     * RETURNS A LIST WITH ALL THE NAMES OF GROUPS BELONGING TO THE RECEIVED USER
     */
    @PostMapping("/getGroupsByUser")
    public ResponseEntity<SpecialResponse> getGroupsByUser(@RequestBody GroupsModel groupModel) {
        JSONObject responseJson = new JSONObject();
        List<String> groupsList = groupsService.getGroupsByUser(groupModel);
        responseJson.put("message", "OK");
        return new ResponseEntity<>(specialResponse(groupsList, responseJson), HttpStatus.OK);
    }

    /*
     * DELETES A GROUP FROM THE DATABASE:
     * 1. Deletes the group if there is a match with the group's name and the user as the author of the group.
     * 2. References in intermediary tables like 'members' will also be deleted.
     */
    @DeleteMapping("/deleteGroup")
    public ResponseEntity<SpecialResponse> deleteGroup(@RequestBody GroupsModel groupModel) {
        JSONObject responseJson = new JSONObject();
        groupsService.deleteGroup(groupModel);
        responseJson.put("message", "Group deleted successfully");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    /* Start of methods for testing purposes */
    /*
     * RETURNS ALL DATA FOR ALL GROUPS FROM THE DATABASE (EXCLUSIVE FOR DEVELOPMENT TESTING, SHOULD NOT BE INCLUDED IN THE FINAL VERSION)
     */
    @GetMapping("/getGroupsDatabase")
    public ResponseEntity<SpecialResponse> getGroupsDatabase() {
        JSONObject responseJson = new JSONObject();
        List<GroupsModel> groupsList = groupsService.getGroupsDatabase();
        responseJson.put("message", "OK");
        return new ResponseEntity<>(specialResponse(groupsList, responseJson), HttpStatus.OK);
    }
    /* End of methods for testing purposes */

    // Exception handling for each use case
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
