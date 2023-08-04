package com.capgemini.ailabar.groups.infraestructure.repositories;

import com.capgemini.ailabar.groups.infraestructure.entities.GroupsEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface GroupsRepository extends JpaRepository<GroupsEntity, String> {
    @Query("SELECT CASE WHEN COUNT(u) > 0 THEN TRUE ELSE FALSE END FROM UsersEntity u WHERE u.user = :user AND u.token = :token")
    boolean checkAuthorization(@Param("user") String user, @Param("token") String token);

    @Query("SELECT g FROM GroupsEntity g WHERE g.id = :id")
    GroupsEntity getGroupById(@Param("id") Integer id);

    @Query("SELECT g FROM GroupsEntity g WHERE g.groupName = :groupName AND g.admin = :admin")
    GroupsEntity getGroupByGroupNameAndAdmin(@Param("groupName") String groupName, @Param("admin") String admin);

    @Query("SELECT g.groupName FROM GroupsEntity g WHERE g.admin = :admin")
    List<String> getAllGroupNamesByAdmin(@Param("admin") String admin);

    @Query("SELECT CASE WHEN COUNT(g) > 0 THEN true ELSE false END FROM GroupsEntity g WHERE g.groupName = :groupName AND g.admin = :admin")
    boolean checkByGroupNameAndAdmin(@Param("groupName") String groupName, @Param("admin") String admin);

    @Modifying
    @Query("DELETE FROM GroupsEntity g WHERE g.groupName = :groupName AND g.admin = :admin")
    void deleteByGroupNameAndAdmin(@Param("groupName") String groupName, @Param("admin") String admin);
}
