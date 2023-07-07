package com.capgemini.beni.ailabar.controller;

import com.capgemini.beni.ailabar.dto.UsersDto;
import com.capgemini.beni.ailabar.entity.UsersEntity;
import com.capgemini.beni.ailabar.service.UsersService;
import com.capgemini.beni.ailabar.utils.SpecialResponse;
import com.capgemini.beni.ailabar.utils.SpecialResponseInterface;
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

    private Boolean checkUser(String user) {
        if(user.isBlank()) {
            return false;
        }

        return usersService.checkUser(user);
    }

    /* Inicio de métodos sólo para realizar pruebas */
    // getMails es llamado desde MailService a UsersService
    private List<String> getMails(List<String> userList) {
        return usersService.getMails(userList);
    }

    @PostMapping("/createUser")
    public ResponseEntity<String> createUser(@RequestBody UsersDto userDto) {
        JSONObject responseJson = new JSONObject();

        if (userDto.getUser().isBlank() || userDto.getPassword().isBlank() || userDto.getEmail().isBlank()) {
            responseJson.put("message", "All data is required to create a new user");
            return new ResponseEntity<>(responseJson.toString(), HttpStatus.BAD_GATEWAY);
        }

        if (Boolean.TRUE.equals(usersService.checkUser(userDto.getUser()))) {
            responseJson.put("message", "The user already exists");
            return new ResponseEntity<>(responseJson.toString(), HttpStatus.OK);
        }

        if (Boolean.TRUE.equals(usersService.existsByEmail(userDto.getEmail()))) {
            responseJson.put("message", "The email already exists");
            return new ResponseEntity<>(responseJson.toString(), HttpStatus.OK);
        }

        String hashedPassword = DigestUtils.sha256Hex(userDto.getPassword());

        String token = DigestUtils.sha256Hex(userDto.getUser()+hashedPassword);

        UsersEntity userEntity = new UsersEntity(userDto);
        userEntity.setPassword(hashedPassword);
        userEntity.setToken(token);
        usersService.saveUser(userEntity);
        responseJson.put("message", "User created successfully");
        return new ResponseEntity<>(responseJson.toString(), HttpStatus.OK);
    }

    @PutMapping("/editUser")
    public ResponseEntity<String> editUser(@RequestBody UsersDto userDto) {
        JSONObject responseJson = new JSONObject();

        if(userDto.getUser().isBlank() || userDto.getPassword().isBlank() || userDto.getToken().isBlank()) {
            responseJson.put("message", "All data is required to edit a user");
            return new ResponseEntity<>(responseJson.toString(), HttpStatus.BAD_GATEWAY);
        }

        if(userDto.getNewUser().isBlank() && userDto.getNewPassword().isBlank()) {
            responseJson.put("message", "There are no values to update.");
            return new ResponseEntity<>(responseJson.toString(), HttpStatus.BAD_GATEWAY);
        }

        if(Boolean.FALSE.equals(usersService.checkUser(userDto.getUser()))) {
            responseJson.put("message", "The user does not exist");
            return new ResponseEntity<>(responseJson.toString(), HttpStatus.BAD_REQUEST);
        }

        if(Boolean.TRUE.equals(usersService.checkUser(userDto.getNewUser().strip()))) {
            responseJson.put("message", "The new username already exists");
            return new ResponseEntity<>(responseJson.toString(), HttpStatus.BAD_REQUEST);
        }

        if(Boolean.FALSE.equals(usersService.checkToken(userDto.getUser(), userDto.getToken()))) {
            responseJson.put("message", "The token does not match");
            return new ResponseEntity<>(responseJson.toString(), HttpStatus.NOT_FOUND);
        }

        UsersEntity userEntity = usersService.findByUser(userDto.getUser());

        if(!userDto.getNewUser().isBlank()) {
            userEntity.setUser(userDto.getNewUser().strip());
        }

        if(!userDto.getNewPassword().isBlank()) {
            String hashedPassword = DigestUtils.sha256Hex(userDto.getNewPassword());
            userEntity.setPassword(hashedPassword);
            userEntity.setToken(userDto.getUser()+hashedPassword);
        }

        usersService.saveUser(userEntity);
        responseJson.put("message", "User modified successfully");
        return new ResponseEntity<>(responseJson.toString(), HttpStatus.OK);
    }

    @DeleteMapping("/deleteUser")
    public ResponseEntity<String> deleteUser(@RequestBody UsersDto userDto) {
        JSONObject responseJson = new JSONObject();

        if(userDto.getUser().isBlank()) {
            responseJson.put("message", "User name is required to delete a user");
            return new ResponseEntity<>(responseJson.toString(), HttpStatus.BAD_GATEWAY);
        }

        if (Boolean.FALSE.equals(usersService.checkUser(userDto.getUser()))) {
            responseJson.put("message", "The user does not exist");
            return new ResponseEntity<>(responseJson.toString(), HttpStatus.NOT_FOUND);
        }

        if(Boolean.FALSE.equals(usersService.checkToken(userDto.getUser(), userDto.getToken()))) {
            responseJson.put("message", "The token does not match");
            return new ResponseEntity<>(responseJson.toString(), HttpStatus.NOT_FOUND);
        }

        usersService.deleteUser(userDto.getUser());
        responseJson.put("message", "User deleted successfully");
        return new ResponseEntity<>(responseJson.toString(), HttpStatus.OK);
    }

    @GetMapping("/getAllUsersData")
    public ResponseEntity<SpecialResponse> getAllUsersDataAll() {
        JSONObject responseJson = new JSONObject();

        List<UsersEntity> usersList = usersService.getAllUsersData();

        if(usersList.isEmpty()) {
            responseJson.put("message", "There are no users in database");
            return new ResponseEntity<>(specialResponse(usersList, responseJson.toString()), HttpStatus.OK);
        }

        responseJson.put("message", "OK");
        return new ResponseEntity<>(specialResponse(usersList, responseJson.toString()), HttpStatus.OK);
    }
    /* Fin métodos sólo para realizar pruebas */

}
