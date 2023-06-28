package com.capgemini.beni.ailabar.controller;

import com.capgemini.beni.ailabar.dto.UsersDto;
import com.capgemini.beni.ailabar.entity.UsersEntity;
import com.capgemini.beni.ailabar.service.UsersService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

@RestController
@RequestMapping("/users")
public class UsersController {
    private final UsersService usersService;

    @Autowired
    public UsersController(UsersService usersService) {
        this.usersService = usersService;
    }

    /* Por implementar */
    @GetMapping("/login")
    public ResponseEntity<Boolean> login(String user, String password) {
        return null;
    }

    /* Inicio métodos no necesarios ya que deberíamos tener una fuente
     * de usuarios externa y no gestionarlo desde esta aplicación */
    @PostMapping("/createUser")
    public ResponseEntity<String> createUser(@RequestBody UsersDto userDto) {
        if(userDto.getUser().isBlank() || userDto.getPassword().isBlank() || userDto.getEmail().isBlank()) {
            return new ResponseEntity<>("All data is required to create a new user", HttpStatus.BAD_GATEWAY);
        }

        if(Boolean.TRUE.equals(usersService.existsByUser(userDto.getUser()))) {
            return new ResponseEntity<>("The user already exists", HttpStatus.OK);
        }

        if(Boolean.TRUE.equals(usersService.existsByEmail(userDto.getEmail()))) {
            return new ResponseEntity<>("The email already exists", HttpStatus.OK);
        }

        UsersEntity userEntity = new UsersEntity(userDto);
        usersService.createUser(userEntity);
        return new ResponseEntity<>("User created successfully", HttpStatus.OK);
    }

    @PutMapping("/editUser/{user}")
    public ResponseEntity<String> editUser(@PathVariable("user") String user, @RequestBody UsersDto userDto) {
        if(userDto.getUser().isBlank() || userDto.getPassword().isBlank() || userDto.getEmail().isBlank()) {
            return new ResponseEntity<>("All data is required to edit a user", HttpStatus.BAD_GATEWAY);
        }

        if (Boolean.FALSE.equals(usersService.existsByUser(user))) {
            return new ResponseEntity<>("The user does not exist", HttpStatus.NOT_FOUND);
        }

        if(Boolean.TRUE.equals(usersService.existsByEmail(userDto.getEmail()))) {
            return new ResponseEntity<>("The email already exists", HttpStatus.OK);
        }

        UsersEntity userEntity = usersService.findByUser(user);

        userEntity.setUser(userDto.getUser());
        userEntity.setPassword(userDto.getPassword());
        userEntity.setEmail(userDto.getEmail());

        usersService.createUser(userEntity);
        return new ResponseEntity<>("User modified successfully", HttpStatus.OK);
    }

    @DeleteMapping("/deleteUser/{user}")
    public ResponseEntity<String> deleteUser(@PathVariable("user") String user) {
        if(user.isBlank()) {
            return new ResponseEntity<>("User name is required to delete a user", HttpStatus.BAD_GATEWAY);
        }

        if (Boolean.FALSE.equals(usersService.existsByUser(user))) {
            return new ResponseEntity<>("The user does not exist", HttpStatus.NOT_FOUND);
        }

        usersService.deleteUser(user);
        return new ResponseEntity<>("User deleted successfully", HttpStatus.OK);
    }

    @GetMapping("/getAllUsersData")
    public ResponseEntity<List<UsersEntity>> getAllUsersDataAll() {
        return new ResponseEntity<>(usersService.getAllUsersData(), HttpStatus.OK);
    }
    /* Fin de los métodos no necesarios */

    /* Este método es sólo de prueba, no va en esta clase, si no en MailService */
    @GetMapping("/getMails/{users}")
    public ResponseEntity<String> getEmailsByUserList(@PathVariable("users") String users) {
        if(users.isBlank()) {
            return new ResponseEntity<>("The users to whom the email needs to be sent are required.", HttpStatus.BAD_GATEWAY);
        }

        String[] usersArray = users.split("[,;]");

        List<String> userList = new ArrayList<>();
        Arrays.stream(usersArray).forEach(user -> userList.add(user.strip()));

        return new ResponseEntity<>(usersService.getEmailsByUserList(userList).toString(), HttpStatus.OK);
    }
    /* ¡¡BORRAR!! */
}
