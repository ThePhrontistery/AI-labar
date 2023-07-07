package com.capgemini.beni.ailabar.dto;

import lombok.Data;

import java.util.List;

@Data
public class UsersDto {
    private Integer id;
    private String user;
    private String password;
    private String email;
    private String token;

    /* Inicio tributos sólo necesarios para realizar pruebas */
    private String newUser;
    private String newPassword;
    private List<String> usersList;
    /* Fin tributos sólo necesarios para realizar pruebas */
}

