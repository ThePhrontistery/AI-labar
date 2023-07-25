package com.capgemini.beni.ailabar.domain.port;

import com.capgemini.beni.ailabar.infrastructure.entity.UsersEntity;

import java.util.List;

public interface UsersRepositoryPort {
    Boolean existsByUser(String user);
    Boolean existsByUserAndToken(String user, String token);
    List<String> findUsersByUsernameContaining(String matcher);
    Boolean existsByEmail(String email);
    UsersEntity findByUser(String user);
    void deleteByUser(String user);
    List<String> findAllUsers();
    List<String> getEmailsByUserList(List<String> userList);
    <S extends UsersEntity> S save(S entity);
    List<UsersEntity> findAll();
}
