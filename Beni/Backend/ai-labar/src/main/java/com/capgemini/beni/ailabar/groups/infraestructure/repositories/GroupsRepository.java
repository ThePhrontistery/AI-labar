package com.capgemini.beni.ailabar.groups.infraestructure.repositories;

import com.capgemini.beni.ailabar.groups.infraestructure.entities.GroupsEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface GroupsRepository extends JpaRepository<GroupsEntity, String> {
    @Query("SELECT g FROM GroupsEntity g WHERE g.groupName = :groupName AND g.admin = :admin")
    GroupsEntity findByGroupNameAndAdmin(@Param("groupName") String groupName, @Param("admin") String admin);

    @Query("SELECT g.groupName FROM GroupsEntity g WHERE g.admin = :admin")
    List<String> findAllGroupNamesByAdmin(@Param("admin") String admin);

    @Query("SELECT CASE WHEN COUNT(g) > 0 THEN true ELSE false END FROM GroupsEntity g WHERE g.groupName = :groupName AND g.admin = :admin")
    Boolean existsByGroupNameAndAdmin(@Param("groupName") String groupName, @Param("admin") String admin);

    @Query("SELECT g FROM GroupsEntity g WHERE g.id = :id")
    GroupsEntity findGroupsEntityById(@Param("id") Integer id);

    @Modifying
    @Query("DELETE FROM GroupsEntity g WHERE g.groupName = :groupName AND g.admin = :admin")
    void deleteByGroupNameAndAdmin(@Param("groupName") String groupName, @Param("admin") String admin);
}
