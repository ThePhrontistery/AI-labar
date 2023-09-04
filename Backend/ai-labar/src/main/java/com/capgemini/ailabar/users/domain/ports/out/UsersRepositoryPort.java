package com.capgemini.ailabar.users.domain.ports.out;

import com.capgemini.ailabar.users.infraestructure.entities.UsersEntity;

import java.util.List;

public interface UsersRepositoryPort {
    boolean checkAuthorization(String user, String token);

    boolean checkEmail(String email);

    boolean checkUser(String user);

    void createUser(UsersEntity usersEntity);

    void deleteUser(Integer id);

    void editUser(UsersEntity usersEntity);

    List<String> getAllUsers();

    UsersEntity getUserByName(String user);

    List<String> getUsersByMatch(String matcher);

    List<UsersEntity> getUsersDatabase();

    boolean login(String user, String password);
}
