package com.capgemini.ailabar.users.domain.ports.out;

import com.capgemini.ailabar.users.infraestructure.entities.UsersEntity;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface UsersRepositoryPort {
    boolean checkAuthorization(String user, String token);

    boolean checkEmail(String email);

    boolean checkUser(String user);

    void createUser(UsersEntity usersEntity);

    void editUser(UsersEntity usersEntity);

    List<String> getAllUsers();

    UsersEntity getUserByName(String user);

    List<String> getUsersByMatch(String matcher);

    List<UsersEntity> getUsersDatabase();

    void disbleGroupsByUserAdmin(String admin, String deactivatedName);

    void deleteMembersByUserId(Integer userId);

    void updateToken(@Param("userId") Integer userId, @Param("newToken") String newToken);

    boolean login(String user, String password);
}
