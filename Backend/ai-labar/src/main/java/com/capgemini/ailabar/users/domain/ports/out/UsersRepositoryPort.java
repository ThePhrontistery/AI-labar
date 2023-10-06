package com.capgemini.ailabar.users.domain.ports.out;

import com.capgemini.ailabar.users.infraestructure.entities.UsersEntity;

import java.util.List;

public interface UsersRepositoryPort {
    boolean checkAuthorization(String user, String token);

    boolean checkEmail(String user);

    boolean checkUser(String user);

    void createUser(UsersEntity usersEntity);

    void editUser(UsersEntity usersEntity);

    List<String> getAllUsers();

    UsersEntity getUserByEmail(String email);

    UsersEntity getUserByName(String user);

    List<String> getUsersByMatch(String matcher);

    List<UsersEntity> getUsersDatabase();

    void disbleGroupsByUserAdmin(String admin, String deactivatedName);

    void deleteMembersByUserId(Integer userId);

    void updateUserNameAndToken(Integer userId, String user, String newToken);

    boolean login(String email, String password);
}
