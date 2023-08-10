package com.capgemini.ailabar.users.domain.models;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Data;

import java.util.List;

@Data
public class UsersModel {
    private Integer id;
    private String user;
    private String password;
    private String email;
    private String gender;
    private String photo;
    private String token;
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private String matcher;
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private String newUser;
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private String newPassword;
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private List<String> usersList;
}
