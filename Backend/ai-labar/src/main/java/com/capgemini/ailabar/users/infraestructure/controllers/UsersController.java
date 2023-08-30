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

    /*
     * INICIA SESIÓN EN LA APLICACIÓN:
     * 1. Si se aplica un cifrado SHA256 en frontend, el password enviado será el hash de la clave del usuario a través de dicho standard.
     *    Si no se aplica, será el password en plano.
     * 2. Si el login es un éxito, se devolverá el token y el tipo de visualización deseado por el usuario.
     * 3. Es importante tener en cuenta, que por motivos de seguridad, en el mensaje de error no se dan pistas de si ha fallado el usuario
     *    o la clave, sólo se indicará el mensaje “Login failed”.
     */
    @PostMapping("/login")
    public ResponseEntity<SpecialResponse> login(@RequestBody UsersModel usersModel) {
        JSONObject responseJson = new JSONObject();
        List<String> loginData = usersService.login(usersModel);
        responseJson.put("message", "Login successful");
        return new ResponseEntity<>(specialResponse(loginData, responseJson), HttpStatus.OK);
    }

    /*
     * CREA UN USUARIO EN LA BBDD:
     * 1. El password en este caso llegaría cifrado con SHA256 desde el frontend. En backend se cifrará de nuevo la clave en SHA256 y se generará
     *    un token con el nombre del usuario (es único), el password y el id.
     * 2. El campo gender puede tener los valores H (hombre) o M (mujer) y no es obligatorio.
     * 3. El campo photo se debe recibir en Base64 y no es obligatorio.
     * 4. Existe un campo visualization que no es obligatorio y por defecto tiene el valor “Paginación”. Este campo hace referencia al tipo de visualización
     *    de los topics deseado. Los valores disponibles son: Paginación, Scroll, Cards.
     */
    @PostMapping("/createUser")
    public ResponseEntity<SpecialResponse> createUser(@RequestBody UsersModel usersModel) {
        JSONObject responseJson = new JSONObject();
        usersService.createUser(usersModel);
        responseJson.put("message", "User created successfully");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    /*
     * EDITA UN USUARIO DE LA BBDD:
     * 1. El uso es similar a /createUser. La diferencia es que se requiere por seguridad el nombre del usuario y del token para realizar la modificación. De este
     *    modo sólo el propio usuario puede modificar su perfil.
     * 2. Para modificar la contraseña se usa el campo newPassword.
     * 3. En el caso de modificarse el usuario, el token será regenerado, invalidando el anterior.
     */
    @PutMapping("/editUser")
    public ResponseEntity<SpecialResponse> editUser(@RequestBody UsersModel usersModel) {
        JSONObject responseJson = new JSONObject();
        usersService.editUser(usersModel);
        responseJson.put("message", "User modified successfully");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    /*
     * MODIFICA EL TIPO DE VISUALIZACIÓN DE LOS TOPICS PREFERIDO POR EL USUARIO
     */
    @PutMapping("/editVisualization")
    public ResponseEntity<SpecialResponse> editVisualization(@RequestBody UsersModel usersModel) {
        JSONObject responseJson = new JSONObject();
        usersService.editVisualization(usersModel);
        responseJson.put("message", "Visualization edited successfully");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    /*
     * ELIMINA A UN USUAIRO DE LA BBDD:
     * 1. Se elimina tanto el usuario como también su existencia en los grupos en los que esté asignado.
     * 2. El user y el token son los datos imprescindibles para cualquier acción dentro de la aplicación.
     */
    @DeleteMapping("/deleteUser")
    public ResponseEntity<SpecialResponse> deleteUser(@RequestBody UsersModel usersModel) {
        JSONObject responseJson = new JSONObject();
        usersService.deleteUser(usersModel);
        responseJson.put("message", "User deleted successfully");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    /*
     * DEVUELVE TODOS LOS USUARIOS QUE COINCIDAN CON EL TEXTO DEL MATCHER RECIBIDO:
     * 1. Si el matcher se envía vacío devolverá todos los usuarios de la base de datos.
     * 2. Los users se devuelven a través de un array de strings de las coincidencias encontradas (si es que existe alguna).
     */
    @PostMapping("/getUsersByMatch")
    public ResponseEntity<SpecialResponse> getUsersByMatch(@RequestBody UsersModel usersModel) {
        JSONObject responseJson = new JSONObject();
        List<String> userMatchesList = usersService.getUsersByMatch(usersModel);
        responseJson.put("message", userMatchesList.size() + " matches");
        return new ResponseEntity<>(specialResponse(userMatchesList, responseJson), HttpStatus.OK);
    }

    /*
     * DEVUELVE TODOS LOS USUARIOS DE LA BBDD
     *
     */
    @PostMapping("/getAllUsers")
    public ResponseEntity<SpecialResponse> getAllUsers(@RequestBody UsersModel usersModel) {
        JSONObject responseJson = new JSONObject();
        List<String> usersList = usersService.getAllUsers(usersModel);
        responseJson.put("message", "List of users obtained successfully");
        return new ResponseEntity<>(specialResponse(usersList, responseJson), HttpStatus.OK);
    }

    /* Inicio de métodos sólo para realizar pruebas */
    /*
     * DEVUELVE TODOS LOS DATOS DE TODOS LOS USUARIOS DE LA BBDD (EXCLUSIVO PARA PRUEBAS DE DESARROLLO, NO DEBE IR EN LA VERSIÓN FINAL)
     */
    @GetMapping("/getUsersDatabase")
    public ResponseEntity<SpecialResponse> getUsersDatabase() {
        JSONObject responseJson = new JSONObject();
        List<UsersEntity> usersList = usersService.getUsersDatabase();
        responseJson.put("message", "OK");
        return new ResponseEntity<>(specialResponse(usersList, responseJson), HttpStatus.OK);
    }
    /* Fin métodos sólo para realizar pruebas */

    // Manejo de las excepciones de cada caso de uso
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
