package com.capgemini.ailabar.infrastructure.entity;

import com.capgemini.ailabar.domain.model.UsersModel;
import lombok.Data;

import javax.persistence.*;

@Data
@Entity
@Table(name = "users")
public class UsersEntity {
    @Id
    @Column(name = "id")
    @GeneratedValue(strategy=GenerationType.IDENTITY)
    private Integer id;
    @Column(name = "user")
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

    public UsersEntity() {}

    public UsersEntity(UsersModel usersModel) {
        this.user = usersModel.getUser();
        this.password = usersModel.getPassword();
        this.email = usersModel.getEmail();
        this.gender = usersModel.getGender();
        this.photo = usersModel.getPhoto();
    }
}