package com.capgemini.ailabar.users.infraestructure.controllers;

import com.capgemini.ailabar.commons.adapters.out.SpecialResponseInterface;
import com.capgemini.ailabar.commons.utils.SpecialResponse;
import com.capgemini.ailabar.users.domain.exceptions.LoginException;
import com.capgemini.ailabar.users.domain.exceptions.*;
import com.capgemini.ailabar.users.infraestructure.entities.UsersEntity;
import com.capgemini.ailabar.users.domain.models.UsersModel;
import com.capgemini.ailabar.users.application.services.UsersService;
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

    @PostMapping("/login")
    public ResponseEntity<SpecialResponse> login(@RequestBody UsersModel usersModel) {
        JSONObject responseJson = new JSONObject();
        List<String> loginData = usersService.login(usersModel);
        responseJson.put("message", "Login successful");
        return new ResponseEntity<>(specialResponse(loginData, responseJson), HttpStatus.OK);
    }

    @PostMapping("/createUser")
    public ResponseEntity<SpecialResponse> createUser(@RequestBody UsersModel usersModel) {
        JSONObject responseJson = new JSONObject();
        usersService.createUser(usersModel);
        responseJson.put("message", "User created successfully");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    @PutMapping("/editUser")
    public ResponseEntity<SpecialResponse> editUser(@RequestBody UsersModel usersModel) {
        JSONObject responseJson = new JSONObject();
        usersService.editUser(usersModel);
        responseJson.put("message", "User modified successfully");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    @PutMapping("/editVisualization")
    public ResponseEntity<SpecialResponse> editVisualization(@RequestBody UsersModel usersModel) {
        JSONObject responseJson = new JSONObject();
        usersService.editVisualization(usersModel);
        responseJson.put("message", "Visualization edited successfully");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    @DeleteMapping("/deleteUser")
    public ResponseEntity<SpecialResponse> deleteUser(@RequestBody UsersModel usersModel) {
        JSONObject responseJson = new JSONObject();
        usersService.deleteUser(usersModel);
        responseJson.put("message", "User deleted successfully");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    @PostMapping("/getUsersByMatch")
    public ResponseEntity<SpecialResponse> getUsersByMatch(@RequestBody UsersModel usersModel) {
        JSONObject responseJson = new JSONObject();
        List<String> userMatchesList = usersService.getUsersByMatch(usersModel);
        responseJson.put("message", userMatchesList.size() + " matches");
        return new ResponseEntity<>(specialResponse(userMatchesList, responseJson), HttpStatus.OK);
    }
    @PostMapping("/getAllUsers")
    public ResponseEntity<SpecialResponse> getAllUsers(@RequestBody UsersModel usersModel) {
        JSONObject responseJson = new JSONObject();
        List<String> usersList = usersService.getAllUsers(usersModel);
        responseJson.put("message", "List of users obtained successfully");
        return new ResponseEntity<>(specialResponse(usersList, responseJson), HttpStatus.OK);
    }

    /* Inicio de métodos sólo para realizar pruebas */
    @GetMapping("/getUsersDatabase")
    public ResponseEntity<SpecialResponse> getUsersDatabase() {
        JSONObject responseJson = new JSONObject();
        List<UsersEntity> usersList = usersService.getUsersDatabase();
        responseJson.put("message", "OK");
        return new ResponseEntity<>(specialResponse(usersList, responseJson), HttpStatus.OK);
    }
    /* Fin métodos sólo para realizar pruebas */

    @ExceptionHandler(LoginException.class)
    ResponseEntity<SpecialResponse> handlerLoginException (LoginException loginException){
        JSONObject responseJson = new JSONObject();
        responseJson.put("message", loginException.getMessage());
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.INTERNAL_SERVER_ERROR);
    }

    @ExceptionHandler(CreateUserException.class)
    ResponseEntity<SpecialResponse> handlerCreateUserException (CreateUserException createUserException){
        JSONObject responseJson = new JSONObject();
        responseJson.put("message", createUserException.getMessage());
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.INTERNAL_SERVER_ERROR);
    }

    @ExceptionHandler(EditUserException.class)
    ResponseEntity<SpecialResponse> handlerEditUserException (EditUserException editUserException){
        JSONObject responseJson = new JSONObject();
        responseJson.put("message", editUserException.getMessage());
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.INTERNAL_SERVER_ERROR);
    }

    @ExceptionHandler(EditVisualizationException.class)
    ResponseEntity<SpecialResponse> handlerEditVisualizationException (EditVisualizationException editVisualizationException){
        JSONObject responseJson = new JSONObject();
        responseJson.put("message", editVisualizationException.getMessage());
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.INTERNAL_SERVER_ERROR);
    }

    @ExceptionHandler(DeleteUserException.class)
    ResponseEntity<SpecialResponse> handlerDeleteUserException (DeleteUserException deleteUserException){
        JSONObject responseJson = new JSONObject();
        responseJson.put("message", deleteUserException.getMessage());
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.INTERNAL_SERVER_ERROR);
    }

    @ExceptionHandler(GetUsersByMatchException.class)
    ResponseEntity<SpecialResponse> handlerGetUsersByMatchException (GetUsersByMatchException getUsersException){
        JSONObject responseJson = new JSONObject();
        responseJson.put("message", getUsersException.getMessage());
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.INTERNAL_SERVER_ERROR);
    }

    @ExceptionHandler(GetAllUsersException.class)
    ResponseEntity<SpecialResponse> handlerGetAllUsersException (GetAllUsersException getAllUsersException){
        JSONObject responseJson = new JSONObject();
        responseJson.put("message", getAllUsersException.getMessage());
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.INTERNAL_SERVER_ERROR);
    }

    @ExceptionHandler(GetUsersDatabaseException.class)
    ResponseEntity<SpecialResponse> handlerGetUsersDatabaseException (GetUsersDatabaseException getUsersDatabaseException){
        JSONObject responseJson = new JSONObject();
        responseJson.put("message", getUsersDatabaseException.getMessage());
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.INTERNAL_SERVER_ERROR);
    }
}
