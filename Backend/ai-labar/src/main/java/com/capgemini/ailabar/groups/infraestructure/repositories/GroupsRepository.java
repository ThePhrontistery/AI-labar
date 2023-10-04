package com.capgemini.ailabar.groups.infraestructure.repositories;

import com.capgemini.ailabar.groups.infraestructure.entities.GroupsEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface GroupsRepository extends JpaRepository<GroupsEntity, Integer> {
    @Query("SELECT CASE WHEN COUNT(u) > 0 THEN TRUE ELSE FALSE END FROM UsersEntity u WHERE u.user = :user AND u.token = :token")
    boolean checkAuthorization(@Param("user") String user, @Param("token") String token);

    @Query("SELECT CASE WHEN COUNT(g) > 0 THEN true ELSE false END FROM GroupsEntity g WHERE g.id = :id AND g.admin = :admin")
    boolean checkByGroupIdAndAdmin(@Param("id") Integer id, @Param("admin") String admin);

    @Query("SELECT CASE WHEN COUNT(g) > 0 THEN true ELSE false END FROM GroupsEntity g WHERE g.groupName = :groupName AND g.admin = :admin")
    boolean checkByGroupNameAndAdmin(@Param("groupName") String groupName, @Param("admin") String admin);

    @Query("SELECT CASE WHEN COUNT(u) > 0 THEN true ELSE false END FROM UsersEntity u WHERE u.user = :member")
    boolean checkMember(@Param("member") String member);

    @Modifying
    @Query("DELETE FROM MembersEntity m WHERE m.group.id = :groupId")
    void deleteMembersByGroupId(@Param("groupId") Integer groupId);

    @Query("SELECT g.groupName FROM GroupsEntity g WHERE g.admin = :admin")
    List<String> getAllGroupNamesByAdmin(@Param("admin") String admin);

    @Query("SELECT g FROM GroupsEntity g WHERE g.groupName = :groupName AND g.admin = :admin")
    GroupsEntity getGroupByGroupNameAndAdmin(@Param("groupName") String groupName, @Param("admin") String admin);

    @Query("SELECT g FROM GroupsEntity g WHERE g.id = :id")
    GroupsEntity getGroupById(@Param("id") Integer id);

    @Query("SELECT g.id FROM GroupsEntity g WHERE g.groupName = :groupName AND g.admin = :admin")
    Integer getGroupIdByGroupNameAndAdmin(@Param("groupName") String groupName, @Param("admin") String admin);

    @Query("SELECT m.user.id FROM MembersEntity m WHERE m.group.id = :groupId")
    List<Integer> getMembersId(@Param("groupId") Integer groupId);

    @Query("SELECT u.id FROM UsersEntity u WHERE u.user = :user")
    Integer getUserIdByUserName(@Param("user") String user);

    @Query("SELECT u.user FROM UsersEntity u WHERE u.id = :id")
    String getUserNameByUserId(@Param("id") Integer id);
    @Query("SELECT CONCAT(u.user, ' (', u.email, ')') FROM UsersEntity u WHERE u.id = :id")
    String getUserNameEMailByUserId(@Param("id") Integer id);
    @Modifying
    @Query(value = "INSERT INTO members (group_id, user_id) VALUES (:groupId, :userId)", nativeQuery = true)
    void insertMember(@Param("groupId") Integer groupId, @Param("userId") Integer userId);
}
