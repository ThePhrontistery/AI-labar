package com.capgemini.beni.ailabar.infrastructure.entity;

import com.capgemini.beni.ailabar.domain.model.UsersModel;
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
    @Column(name = "token")
    private String token;

    public UsersEntity() {}

    public UsersEntity(UsersModel usersDto) {
        this.user = usersDto.getUser();
        this.password = usersDto.getPassword();
        this.email = usersDto.getEmail();
    }
}