package com.capgemini.ailabar.groups.domain.ports.out;

import com.capgemini.ailabar.groups.infraestructure.entities.GroupsEntity;

import java.util.List;

public interface GroupsRepositoryPort {
    void createGroup(GroupsEntity groupsEntity);
    GroupsEntity getGroup(String groupName, String admin);
    void editGroup(GroupsEntity groupsEntity);
    void deleteGroup(Integer id);
    List<GroupsEntity> getGroupsDatabase();

    boolean checkAuthorization(String user, String token);
    boolean checkByGroupIdAndAdmin(Integer id, String admin);
    boolean checkByGroupNameAndAdmin(String groupName, String admin);
    GroupsEntity getGroupById(Integer id);
    Integer getGroupIdByGroupNameAndAdmin(String groupName, String admin);
    List<String> getAllGroupNamesByAdmin(String admin);
}
