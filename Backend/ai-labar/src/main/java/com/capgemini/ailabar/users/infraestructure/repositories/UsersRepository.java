package com.capgemini.ailabar.users.infraestructure.repositories;

import com.capgemini.ailabar.users.infraestructure.entities.UsersEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface UsersRepository extends JpaRepository<UsersEntity, Integer> {
    @Query("SELECT CASE WHEN COUNT(u) > 0 THEN TRUE ELSE FALSE END FROM UsersEntity u WHERE u.user = :user AND u.token = :token")
    boolean checkAuthorization(@Param("user") String user, @Param("token") String token);

    @Query("SELECT CASE WHEN COUNT(u) > 0 THEN true ELSE false END FROM UsersEntity u WHERE u.email = :email")
    boolean checkEmail(@Param("email") String email);

    @Query("SELECT CASE WHEN COUNT(u) > 0 THEN TRUE ELSE FALSE END FROM UsersEntity u WHERE u.user = :user AND u.password = :password")
    boolean checkLogin(@Param("user") String user, @Param("password") String password);

    @Query("SELECT CASE WHEN COUNT(u) > 0 THEN true ELSE false END FROM UsersEntity u WHERE u.user = :user")
    boolean checkUser(@Param("user") String user);

    @Override
    List<UsersEntity> findAll();

    @Query("SELECT u.user FROM UsersEntity u WHERE u.user NOT LIKE '%Deactivated%'")
    List<String> getAllUsers();

    @Query("SELECT u FROM UsersEntity u WHERE u.email = :email")
    UsersEntity getUserByEmail(@Param("email") String email);

    @Query("SELECT u FROM UsersEntity u WHERE u.user = :user")
    UsersEntity getUserByName(@Param("user") String user);

    @Query("SELECT CONCAT(u.user, ' (', u.email, ')') AS user FROM UsersEntity u WHERE (UPPER(u.user) LIKE %:matcher% OR UPPER(u.email) LIKE %:matcher%) AND u.user NOT LIKE '%Deactivated%'")
    List<String> getUsersByNameMatch(@Param("matcher") String matcher);

    @Modifying
    @Query("UPDATE GroupsEntity g SET g.admin = :deactivatedName WHERE g.admin = :admin")
    void disbleGroupsByUserAdmin(@Param("admin") String admin, @Param("deactivatedName") String deactivatedName);

    @Modifying
    @Query("DELETE FROM MembersEntity m WHERE m.user.id = :userId")
    void deleteMembersByUserId(@Param("userId") Integer userId);

    @Modifying
    @Query("UPDATE UsersEntity u SET u.user = :user, u.token = :newToken WHERE u.id = :userId")
    void updateUserNameAndToken(@Param("userId") Integer userId, @Param("user") String user, @Param("newToken") String newToken);

    @Override
    <S extends UsersEntity> S save(S entity);
}
