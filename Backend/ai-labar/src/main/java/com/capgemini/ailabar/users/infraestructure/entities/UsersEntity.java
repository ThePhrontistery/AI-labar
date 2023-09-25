package com.capgemini.ailabar.users.infraestructure.entities;

import com.capgemini.ailabar.users.domain.models.UsersModel;
import lombok.Data;

import javax.persistence.*;
import java.sql.Timestamp;

@Data
@Entity
@Table(name = "users")
public class UsersEntity {
    @Id
    @Column(name = "id")
    @GeneratedValue(strategy=GenerationType.IDENTITY)
    private Integer id;
    //cambiado porque en postgres 'user' esta reservado
    @Column(name = "user_name")
    private String user;
    @Column(name = "password")
    private String password;
    @Column(name = "email")
    private String email;
    @Column(name = "gender")
    private String gender;
    @Column(name = "photo")
    private String photo;
    @Column(name = "token")
    private String token;
    @Column(name = "visualization")
    private String visualization;
    @Column(name = "language")
    private String language;
    @Column(name = "registration_date")
    private Timestamp registrationDate;
    @Column(name = "last_modification_date")
    private Timestamp lastModificationDate;
    @Column(name = "deactivation_date")
    private Timestamp deactivationDate;

    public UsersEntity() {}

    public UsersEntity(UsersModel usersModel) {
        this.user = usersModel.getUser();
        this.password = usersModel.getPassword();
        this.email = usersModel.getEmail();
        this.gender = usersModel.getGender();
        this.photo = usersModel.getPhoto();
        this.visualization = "Paginacion";
        this.language = usersModel.getLanguage();
    }
}