package com.capgemini.ailabar.users.infraestructure.repositories;

import com.capgemini.ailabar.users.infraestructure.entities.UsersEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface  UsersRepository extends JpaRepository<UsersEntity, Integer> {
    @Query("SELECT CASE WHEN COUNT(u) > 0 THEN true ELSE false END FROM UsersEntity u WHERE u.user = :user")
    boolean checkUser(@Param("user") String user);

    @Query("SELECT CASE WHEN COUNT(u) > 0 THEN true ELSE false END FROM UsersEntity u WHERE u.email = :email")
    boolean checkEmail(@Param("email") String email);

    @Query("SELECT CASE WHEN COUNT(u) > 0 THEN TRUE ELSE FALSE END FROM UsersEntity u WHERE u.user = :user AND u.password = :password")
    boolean checkLogin(@Param("user") String user, @Param("password") String password);

    @Query("SELECT CASE WHEN COUNT(u) > 0 THEN TRUE ELSE FALSE END FROM UsersEntity u WHERE u.user = :user AND u.token = :token")
    boolean checkAuthorization(@Param("user") String user, @Param("token") String token);

    @Query("SELECT u FROM UsersEntity u WHERE u.user = :user")
    UsersEntity getUserByName(@Param("user") String user);

    @Query("SELECT u.user FROM UsersEntity u WHERE u.user LIKE %:matcher%")
    List<String> getUsersByNameMatch(@Param("matcher") String matcher);

    @Query("SELECT u.user FROM UsersEntity u")
    List<String> getAllUsers();

    @Override
    <S extends UsersEntity> S save(S entity);

    @Override
    List<UsersEntity> findAll();
}
