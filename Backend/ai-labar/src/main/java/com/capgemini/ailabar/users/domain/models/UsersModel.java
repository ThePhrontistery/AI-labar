package com.capgemini.ailabar.users.domain.models;

import com.capgemini.ailabar.commons.utils.DateTime;
import com.capgemini.ailabar.users.infraestructure.entities.UsersEntity;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Data;

import java.sql.Timestamp;
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
    private String visualization;
    private String language;
    private Integer online;
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private Timestamp registrationDate;
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private String registrationDateFormatted;
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private Timestamp lastModificationDate;
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private String lastModificationDateFormatted;
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private Timestamp deactivationDate;
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private String deactivationDateFormatted;
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private Integer page;
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private Integer elements;
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private List<String> filters;
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private String matcher;
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private String newUser;
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private String newPassword;
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private List<String> usersList;

    public UsersModel() {}

    public UsersModel(UsersEntity usersEntity) {
        this.id = usersEntity.getId();
        this.user = usersEntity.getUser();
        this.password = usersEntity.getPassword();
        this.email = usersEntity.getEmail();
        this.gender = usersEntity.getGender();
        this.photo = usersEntity.getPhoto();
        this.token = usersEntity.getToken();
        this.visualization = usersEntity.getVisualization();
        this.language = usersEntity.getLanguage();
        this.registrationDateFormatted = DateTime.timestampToString(usersEntity.getRegistrationDate());
        if(usersEntity.getLastModificationDate() != null) {
            this.lastModificationDateFormatted = DateTime.timestampToString(usersEntity.getLastModificationDate());
        }
        if(usersEntity.getDeactivationDate() != null) {
            this.deactivationDateFormatted = DateTime.timestampToString(usersEntity.getDeactivationDate());
        }
    }
}
