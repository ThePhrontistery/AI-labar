package com.capgemini.beni.ailabar.entity;

import com.capgemini.beni.ailabar.dto.UsersDto;
import lombok.Data;

import javax.persistence.*;

@Data
@Entity
@Table(name = "users")
public class UsersEntity {
    @Id
    @Column(name = "id")
    @GeneratedValue(strategy=GenerationType.IDENTITY)
    private String id;
    @Column(name = "user")
    private String user;
    @Column(name = "password")
    private String password;
    @Column(name = "email")
    private String email;

    public UsersEntity() {}

    public UsersEntity(UsersDto usersDto) {
        this.user = usersDto.getUser();
        this.password = usersDto.getPassword();
        this.email = usersDto.getEmail();
    }
}