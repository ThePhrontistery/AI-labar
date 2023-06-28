package com.capgemini.beni.ailabar.controller;

import com.capgemini.beni.ailabar.dto.GroupsDto;
import com.capgemini.beni.ailabar.entity.GroupsEntity;
import com.capgemini.beni.ailabar.service.GroupsService;
import com.capgemini.beni.ailabar.service.UsersService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/groups")
public class GroupsController {
    private final GroupsService groupsService;
    private final UsersService usersService;

    @Autowired
    public GroupsController(GroupsService groupsService, UsersService usersService) {
        this.groupsService = groupsService;
        this.usersService = usersService;
    }

    /* Este método devolverá los usuarios que coincidan con los caracteres introducidos en front (debe ser ajustado aún) */
    @GetMapping("/getUser/{user}")
    public ResponseEntity<String> getUser(@PathVariable("user") String user) {
        if(user.isBlank()) {
            return new ResponseEntity<>("User name is required", HttpStatus.BAD_GATEWAY);
        }

        if (Boolean.FALSE.equals(usersService.existsByUser(user))) {
            return new ResponseEntity<>("The user does not exist", HttpStatus.NOT_FOUND);
        }

        return new ResponseEntity<>(usersService.findByUser(user).getUser(), HttpStatus.OK);
    }

    @PostMapping("/saveGroup")
    public ResponseEntity<String> saveGroup(@RequestBody GroupsDto groupDto) {
        if(groupDto.getGroupName().isBlank() || groupDto.getMembers().isBlank() || groupDto.getAdmin().isBlank()) {
            return new ResponseEntity<>("All data is required to save a group", HttpStatus.BAD_GATEWAY);
        }

        if(Boolean.TRUE.equals(groupsService.existsByGroupNameAndAdmin(groupDto.getGroupName(), groupDto.getAdmin()))) {
            return new ResponseEntity<>("The administrator already has a group with that name", HttpStatus.OK);
        }

        GroupsEntity groupEntity = new GroupsEntity(groupDto);
        groupsService.saveGroup(groupEntity);
        return new ResponseEntity<>("Group created successfully", HttpStatus.OK);
    }

    @GetMapping("/getGroup/{groupName}/{admin}")
    public ResponseEntity<GroupsEntity> getGroup(@PathVariable("groupName") String groupName, @PathVariable("admin") String admin) {
        return new ResponseEntity<>(groupsService.getGroup(groupName, admin), HttpStatus.OK);
    }

    @GetMapping("/editGroup/{admin}")
    public ResponseEntity<List<String>> editGroup(@PathVariable("admin") String admin) {
        return new ResponseEntity<>(groupsService.editGroup(admin), HttpStatus.OK);
    }

    @DeleteMapping("/deleteGroup/{groupName}/{admin}")
    public ResponseEntity<String> deleteGroup(@PathVariable("groupName") String groupName, @PathVariable("admin") String admin) {
        if(groupName.isBlank() || admin.isBlank()) {
            return new ResponseEntity<>("Group name and administrator are required to delete a group", HttpStatus.BAD_GATEWAY);
        }

        if(Boolean.FALSE.equals(groupsService.existsByGroupNameAndAdmin(groupName, admin))) {
            return new ResponseEntity<>("The administrator does not have a group with that name", HttpStatus.OK);
        }

        groupsService.deleteGroup(groupName, admin);
        return new ResponseEntity<>("Group deleted successfully", HttpStatus.OK);
    }
}
