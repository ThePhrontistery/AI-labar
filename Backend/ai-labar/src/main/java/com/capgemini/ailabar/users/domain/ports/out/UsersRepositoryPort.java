package com.capgemini.ailabar.users.domain.ports.out;

import com.capgemini.ailabar.users.infraestructure.entities.UsersEntity;

import java.util.List;

public interface UsersRepositoryPort {
    void createUser(UsersEntity usersEntity);
    void editUser(UsersEntity usersEntity);
    void deleteUser(String user);
    List<String> getUsersByMatch(String matcher);
    List<String> getAllUsers();
    List<UsersEntity> getUsersDatabase();


    boolean checkUser(String user);
    boolean checkEmail(String email);
    boolean checkAuthorization(String user, String token);
    UsersEntity getUserByName(String user);
}
