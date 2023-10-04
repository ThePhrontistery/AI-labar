package com.capgemini.ailabar.users.infraestructure.controllers;

import com.capgemini.ailabar.commons.adapters.out.SpecialResponseInterface;
import com.capgemini.ailabar.commons.utils.RSAKeyPairGeneratorService;
import com.capgemini.ailabar.commons.utils.SpecialResponse;
import com.capgemini.ailabar.users.domain.exceptions.LoginException;
import com.capgemini.ailabar.users.domain.exceptions.*;
import com.capgemini.ailabar.users.domain.models.UsersModel;
import com.capgemini.ailabar.users.application.services.UsersService;
import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.security.PrivateKey;
import java.util.List;

@RestController
@RequestMapping("/users")
public class UsersController implements SpecialResponseInterface {
    private final RSAKeyPairGeneratorService rsaKeyPairGeneratorService;
    private final UsersService usersService;
    private final Environment environment;
    private PrivateKey privateKey;

    @Autowired
    public UsersController(RSAKeyPairGeneratorService rsaKeyPairGeneratorService, UsersService usersService,
                           Environment environment) {
        this.rsaKeyPairGeneratorService = rsaKeyPairGeneratorService;
        this.usersService = usersService;
        this.environment = environment;
    }

    /*
     * GENERATE AND PROVIDE AN RSA PUBLIC KEY FOR ENCRYPTION ON THE FRONTEND
     */
    @GetMapping("/getPublicKey")
    public ResponseEntity<SpecialResponse> getPublicKey() {
        JSONObject responseJson = new JSONObject();
        String publicKey = null;
        try {
            rsaKeyPairGeneratorService.generateKeys();
            this.privateKey = rsaKeyPairGeneratorService.getPrivateKey();
            publicKey = rsaKeyPairGeneratorService.publicKeyToString();
        } catch (Exception e) {
            throw new LoginException("An error occurred while obtaining the RSA public key");
        }
        responseJson.put("message", "PublicKey obtained successful");
        return new ResponseEntity<>(specialResponse(publicKey, responseJson), HttpStatus.OK);
    }

    /*
     * LOGS IN TO THE APPLICATION:
     * 1. If SHA256 encryption is applied in the frontend, the sent password will be the hash of the user's password according to this standard.
     *    If not, it will be the plain password.
     * 2. If the login is successful, the token and the desired user interface type will be returned to the user.
     * 3. It's important to note that for security reasons, the error message will not provide hints about whether the failure was due to the user
     *    or the password; it will simply display the message "Login failed."
     */
    @PostMapping("/login")
    public ResponseEntity<SpecialResponse> login(@RequestBody UsersModel usersModel) {
        if("true".equals(environment.getProperty("login.cap.active"))) {
            try {
                usersModel.setPassword(rsaKeyPairGeneratorService.decryptWithPrivateKey(usersModel.getPassword(), privateKey));
            } catch (Exception e) {
                throw new LoginException("An error occurred while decrypting the RSA encryption");
            }
        }

        JSONObject responseJson = new JSONObject();
        List<String> loginData = usersService.login(usersModel, privateKey);
        responseJson.put("message", "Login successful");
        return new ResponseEntity<>(specialResponse(loginData, responseJson), HttpStatus.OK);
    }

    @PostMapping("/adminAccess")
    public ResponseEntity<SpecialResponse> adminAccess(@RequestBody UsersModel usersModel) {
        JSONObject responseJson = new JSONObject();
        usersService.adminAccess(usersModel);
        responseJson.put("message", "Admin access successful");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    /*
     * CREATES A USER IN THE DATABASE:
     * 1. In this case, the password should arrive encrypted with SHA256 from the frontend. In the backend, the password will be re-encrypted in SHA256, and a token will be generated with the user's unique name, password, and ID.
     * 2. The 'gender' field can have values H (male) or M (female) and is optional.
     * 3. The 'photo' field should be received in Base64 and is optional.
     * 4. There is a 'visualization' field that is not mandatory and defaults to "Pagination." This field refers to the desired type of topic visualization. Available values are: Pagination, Scroll, Cards.
     */
    @PostMapping("/createUser")
    public ResponseEntity<SpecialResponse> createUser(@RequestBody UsersModel usersModel) {
        JSONObject responseJson = new JSONObject();
        usersService.createUser(usersModel);
        responseJson.put("message", "User created successfully");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    /*
     * EDITS A USER IN THE DATABASE:
     * 1. The usage is similar to /createUser. The difference is that for security, the user's name and token are required to make the modification. This way, only the user themselves can modify their profile.
     * 2. To change the password, use the 'newPassword' field.
     * 3. In the case of modifying the user, the token will be regenerated, invalidating the previous one.
     */
    @PutMapping("/editUser")
    public ResponseEntity<SpecialResponse> editUser(@RequestBody UsersModel usersModel) {
        JSONObject responseJson = new JSONObject();
        usersService.editUser(usersModel);
        responseJson.put("message", "User modified successfully");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    /*
     * MODIFIES THE PREFERRED TOPIC VISUALIZATION TYPE FOR THE USER
     */
    @PutMapping("/editVisualization")
    public ResponseEntity<SpecialResponse> editVisualization(@RequestBody UsersModel usersModel) {
        JSONObject responseJson = new JSONObject();
        usersService.editVisualization(usersModel);
        responseJson.put("message", "Visualization edited successfully");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    /*
     * MODIFIES THE PREFERRED TOPIC LANGUAGE TYPE FOR THE USER
     */
    @PutMapping("/editLanguage")
    public ResponseEntity<SpecialResponse> editLanguage(@RequestBody UsersModel usersModel) {
        JSONObject responseJson = new JSONObject();
        usersService.editLanguage(usersModel);
        responseJson.put("message", "Language edited successfully");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    /*
     * DELETES A USER FROM THE DATABASE:
     * 1. Deletes both the user and their existence in any groups they are assigned to.
     * 2. The user and the token are essential for any action within the application.
     */
    @DeleteMapping("/deleteUser")
    public ResponseEntity<SpecialResponse> deleteUser(@RequestBody UsersModel usersModel) {
        JSONObject responseJson = new JSONObject();
        usersService.deleteUser(usersModel);
        responseJson.put("message", "User deleted successfully");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    /*
     * RETURNS ALL USERS MATCHING THE RECEIVED MATCHER TEXT:
     * 1. If the matcher is sent empty, it will return all users from the database.
     * 2. Users are returned as an array of strings of found matches (if any).
     */
    @PostMapping("/getUsersByMatch")
    public ResponseEntity<SpecialResponse> getUsersByMatch(@RequestBody UsersModel usersModel) {
        JSONObject responseJson = new JSONObject();
        List<String> userMatchesList = usersService.getUsersByMatch(usersModel);
        responseJson.put("message", userMatchesList.size() + " matches");
        return new ResponseEntity<>(specialResponse(userMatchesList, responseJson), HttpStatus.OK);
    }

    /*
     * RETURNS ALL USERS FROM THE DATABASE
     */
    @PostMapping("/getAllUsers")
    public ResponseEntity<SpecialResponse> getAllUsers(@RequestBody UsersModel usersModel) {
        JSONObject responseJson = new JSONObject();
        List<String> usersList = usersService.getAllUsers(usersModel);
        responseJson.put("message", "List of users obtained successfully");
        return new ResponseEntity<>(specialResponse(usersList, responseJson), HttpStatus.OK);
    }

    /* Start of methods for testing purposes */
    /*
     * RETURNS ALL DATA FOR ALL USERS FROM THE DATABASE (EXCLUSIVE FOR DEVELOPMENT TESTING, SHOULD NOT BE INCLUDED IN THE FINAL VERSION)
     */
    @GetMapping("/getUsersDatabase")
    public ResponseEntity<SpecialResponse> getUsersDatabase() {
        JSONObject responseJson = new JSONObject();
        List<UsersModel> usersList = usersService.getUsersDatabase();
        responseJson.put("message", "OK");
        return new ResponseEntity<>(specialResponse(usersList, responseJson), HttpStatus.OK);
    }
    /* End of methods for testing purposes */

    /*
     * LOGS OUT TO THE APPLICATION
     */
    @PostMapping("/logout")
    public ResponseEntity<SpecialResponse> logout(@RequestBody UsersModel usersModel) {
        JSONObject responseJson = new JSONObject();
        usersService.logout(usersModel);
        responseJson.put("message", "Logout successful");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    // Exception handling for each use case
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

    @ExceptionHandler(LogoutException.class)
    ResponseEntity<SpecialResponse> handlerLogoutException (LogoutException logoutException){
        JSONObject responseJson = new JSONObject();
        responseJson.put("message", logoutException.getMessage());
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.INTERNAL_SERVER_ERROR);
    }
}
