package com.capgemini.ailabar.users.infraestructure.entities;

import com.capgemini.ailabar.users.domain.models.UsersModel;
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

    public UsersEntity() {}

    public UsersEntity(UsersModel usersModel) {
        this.user = usersModel.getUser();
        this.password = usersModel.getPassword();
        this.email = usersModel.getEmail();
        this.gender = usersModel.getGender();
        this.photo = usersModel.getPhoto();
        this.visualization = "Paginacion";
    }
}