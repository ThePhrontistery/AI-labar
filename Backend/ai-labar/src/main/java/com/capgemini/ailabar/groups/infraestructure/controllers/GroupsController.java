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
     * CREA UN GRUPO EN LA BBDD:
     * 1. Los members deben ser enviados como un array de string.
     * 2. El user creador del grupo será el admin del grupo.
     * 3. El user creador del grupo no podrá asignarse como miembro del mismo, ya lo es por defecto.
     */
    @PostMapping("/createGroup")
    public ResponseEntity<SpecialResponse> createGroup(@RequestBody GroupsModel groupModel) {
        JSONObject responseJson = new JSONObject();
        groupsService.createGroup(groupModel);
        responseJson.put("message", "Group created successfully");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    /*
     * DEVUELVE LOS DATOS DE UN GRUPO SI TIENE UNA COINCIDENCIA EXACTA EN EL NOMBRE:
     * 1. Se recuperan los datos del grupo siempre y cuando el user tenga un grupo que coincida exactamente con el groupName recibido.
     */
    @PostMapping("/getGroup")
    public ResponseEntity<SpecialResponse> getGroup(@RequestBody GroupsModel groupModel) {
        JSONObject responseJson = new JSONObject();
        GroupsModel matchedGroup = groupsService.getGroup(groupModel);
        responseJson.put("message", "OK");
        return new ResponseEntity<>(specialResponse(matchedGroup, responseJson), HttpStatus.OK);
    }

    /*
     * EDITA UN GRUPO DE LA BBDD:
     * 1. Todos los datos necesarios para la edición del grupo se pueden obtener con getGroup() y tan sólo sería necesario enviar el atributo newGroupName si el nombre del grupo cambia.
     * 2. Los miembros del grupo pueden ser modificados, pero no requieren una variable adicional para ello.
     * 3. El uso de newGroupName sólo es obligatorio si el nombre del grupo ha sido modificado, no para la modificación de los members del grupo.
     */
    @PutMapping("/editGroup")
    public ResponseEntity<SpecialResponse> editGroup(@RequestBody GroupsModel groupModel) {
        JSONObject responseJson = new JSONObject();
        groupsService.editGroup(groupModel);
        responseJson.put("message", "Group edited successfully");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    /*
     * DEVUELVE UN LISTADO CON TODOS LOS NOMBRES DE LOS GRUPOS QUE PERTENECEN AL USUARIO RECIBIDO
     */
    @PostMapping("/getGroupsByUser")
    public ResponseEntity<SpecialResponse> getGroupsByUser(@RequestBody GroupsModel groupModel) {
        JSONObject responseJson = new JSONObject();
        List<String> groupsList = groupsService.getGroupsByUser(groupModel);
        responseJson.put("message", "OK");
        return new ResponseEntity<>(specialResponse(groupsList, responseJson), HttpStatus.OK);
    }

    /*
     * ELIMINA UN GRUPO DE LA BBDD:
     * 1. Se elimina el grupo si existe una coincidencia con el nombre del grupo y el usuario como autor del mismo.
     * 2. También se borrarán las referencias en las tablas intermedias como members.
     */
    @DeleteMapping("/deleteGroup")
    public ResponseEntity<SpecialResponse> deleteGroup(@RequestBody GroupsModel groupModel) {
        JSONObject responseJson = new JSONObject();
        groupsService.deleteGroup(groupModel);
        responseJson.put("message", "Group deleted successfully");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    /* Inicio métodos sólo para pruebas */
    /*
     * DEVUELVE TODOS LOS DATOS DE TODOS LOS GRUPOS DE LA BBDD (EXCLUSIVO PARA PRUEBAS DE DESARROLLO, NO DEBE IR EN LA VERSIÓN FINAL)
     */
    @GetMapping("/getGroupsDatabase")
    public ResponseEntity<SpecialResponse> getGroupsDatabase() {
        JSONObject responseJson = new JSONObject();
        List<GroupsModel> groupsList = groupsService.getGroupsDatabase();
        responseJson.put("message", "OK");
        return new ResponseEntity<>(specialResponse(groupsList, responseJson), HttpStatus.OK);
    }
    /* Fin métodos sólo para pruebas */

    // Manejo de las excepciones de cada caso de uso
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
