package com.capgemini.beni.ailabar.infrastructure.controller;

import com.capgemini.beni.ailabar.domain.model.UsersModel;
import com.capgemini.beni.ailabar.infrastructure.entity.UsersEntity;
import com.capgemini.beni.ailabar.application.service.UsersService;
import com.capgemini.beni.ailabar.infrastructure.utils.SpecialResponse;
import com.capgemini.beni.ailabar.infrastructure.utils.SpecialResponseInterface;
import org.apache.commons.codec.digest.DigestUtils;
import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/users")
public class UsersController implements SpecialResponseInterface {
    private final UsersService usersService;

    @Autowired
    public UsersController(UsersService usersService) {
        this.usersService = usersService;
    }

    /* Inicio de métodos sólo para realizar pruebas */
    // getMails es llamado desde MailService a UsersService
    private List<String> getMails(List<String> userList) {
        return usersService.getMails(userList);
    }

    @PostMapping("/createUser")
    public ResponseEntity<SpecialResponse> createUser(@RequestBody UsersModel userDto) {
        JSONObject responseJson = new JSONObject();

        if (userDto.getUser().isBlank() || userDto.getPassword().isBlank() || userDto.getEmail().isBlank()) {
            responseJson.put("message", "All data is required to create a new user");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.BAD_GATEWAY);
        }

        if (Boolean.TRUE.equals(usersService.checkUser(userDto.getUser()))) {
            responseJson.put("message", "The user already exists");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
        }

        if (Boolean.TRUE.equals(usersService.existsByEmail(userDto.getEmail()))) {
            responseJson.put("message", "The email already exists");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
        }

        String hashedPassword = DigestUtils.sha256Hex(userDto.getPassword());

        UsersEntity userEntity = new UsersEntity(userDto);
        userEntity.setPassword(hashedPassword);
        userEntity.setToken("");
        usersService.saveUser(userEntity);

        userEntity = usersService.findByUser(userDto.getUser());

        if(userEntity == null) {
            responseJson.put("message", "User not found");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.NOT_FOUND);
        }

        String token = DigestUtils.sha256Hex(userDto.getUser()+hashedPassword+userEntity.getId());
        userEntity.setToken(token);

        usersService.saveUser(userEntity);

        responseJson.put("message", "User created successfully");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    @PutMapping("/editUser")
    public ResponseEntity<SpecialResponse> editUser(@RequestBody UsersModel userDto) {
        JSONObject responseJson = new JSONObject();

        if(userDto.getUser().isBlank() || userDto.getPassword().isBlank() || userDto.getToken().isBlank()) {
            responseJson.put("message", "All data is required to edit a user");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.BAD_GATEWAY);
        }

        if(Boolean.FALSE.equals(usersService.checkToken(userDto.getUser(), userDto.getToken()))) {
            responseJson.put("message", "Unauthorized user");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.NOT_FOUND);
        }

        if((userDto.getNewUser() == null || userDto.getNewUser().isBlank()) && (userDto.getNewPassword() == null || userDto.getNewPassword().isBlank())) {
            responseJson.put("message", "There are no values to update.");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.BAD_GATEWAY);
        }

        if(Boolean.TRUE.equals(usersService.checkUser(userDto.getNewUser().strip()))) {
            responseJson.put("message", "The new username already exists");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.BAD_REQUEST);
        }

        UsersEntity userEntity = usersService.findByUser(userDto.getUser());

        if(userDto.getNewUser() != null && !userDto.getNewUser().isBlank()) {
            userEntity.setUser(userDto.getNewUser().strip());
        }

        if(userDto.getNewPassword() != null && !userDto.getNewPassword().isBlank()) {
            String hashedPassword = DigestUtils.sha256Hex(userDto.getNewPassword());
            userEntity.setPassword(hashedPassword);
        }

        String token = DigestUtils.sha256Hex(userEntity.getUser()+userEntity.getPassword()+userEntity.getId());
        userEntity.setToken(token);

        usersService.saveUser(userEntity);
        responseJson.put("message", "User modified successfully");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    @DeleteMapping("/deleteUser")
    public ResponseEntity<SpecialResponse> deleteUser(@RequestBody UsersModel userDto) {
        JSONObject responseJson = new JSONObject();

        if(userDto.getUser().isBlank() || userDto.getToken().isBlank()) {
            responseJson.put("message", "User name and token are required to delete a user");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.BAD_GATEWAY);
        }

        if(Boolean.FALSE.equals(usersService.checkToken(userDto.getUser(), userDto.getToken()))) {
            responseJson.put("message", "Unauthorized user");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.NOT_FOUND);
        }

        usersService.deleteUser(userDto.getUser());
        responseJson.put("message", "User deleted successfully");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    @PostMapping("/getUsers")
    public ResponseEntity<SpecialResponse> getUsers(@RequestBody UsersModel userDto) {
        JSONObject responseJson = new JSONObject();

        if(userDto.getUser().isBlank() || userDto.getToken().isBlank()) {
            responseJson.put("message", "User and token are required");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.BAD_GATEWAY);
        }

        if(Boolean.FALSE.equals(usersService.checkToken(userDto.getUser(), userDto.getToken()))) {
            responseJson.put("message", "Unauthorized user");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.NOT_FOUND);
        }

        List<String> userMatchesList = usersService.userMatches(userDto.getMatcher());
        if(userMatchesList == null) {
            responseJson.put("message", "Not matches");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.NOT_FOUND);
        }

        responseJson.put("message", userMatchesList.size() + " matches");
        return new ResponseEntity<>(specialResponse(userMatchesList, responseJson), HttpStatus.OK);
    }

    @PostMapping("/getAllUsers")
    public ResponseEntity<SpecialResponse> getAllUsers(@RequestBody UsersModel userDto) {
        JSONObject responseJson = new JSONObject();

        if(userDto.getUser().isBlank() || userDto.getToken().isBlank()) {
            responseJson.put("message", "User name and token are required to delete a user");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.BAD_GATEWAY);
        }

        if(Boolean.FALSE.equals(usersService.checkToken(userDto.getUser(), userDto.getToken()))) {
            responseJson.put("message", "Unauthorized user");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.NOT_FOUND);
        }

        List<String> usersList = usersService.getAllUsers();
        responseJson.put("message", "List of users obtained successfully");
        return new ResponseEntity<>(specialResponse(usersList, responseJson), HttpStatus.OK);
    }

    @GetMapping("/getAllUsersData")
    public ResponseEntity<SpecialResponse> getAllUsersDataAll() {
        JSONObject responseJson = new JSONObject();

        List<UsersEntity> usersList = usersService.getAllUsersData();

        if(usersList.isEmpty()) {
            responseJson.put("message", "There are no users in database");
            return new ResponseEntity<>(specialResponse(usersList, responseJson), HttpStatus.OK);
        }

        responseJson.put("message", "OK");
        return new ResponseEntity<>(specialResponse(usersList, responseJson), HttpStatus.OK);
    }
    /* Fin métodos sólo para realizar pruebas */

}
