package com.capgemini.ailabar.groups.domain.ports.out;

import com.capgemini.ailabar.groups.infraestructure.entities.GroupsEntity;

import java.util.List;

public interface GroupsRepositoryPort {
    boolean checkAuthorization(String user, String token);

    boolean checkByGroupIdAndAdmin(Integer id, String admin);

    boolean checkByGroupNameAndAdmin(String groupName, String admin);

    boolean checkMember(String member);

    void createGroup(GroupsEntity groupsEntity);

    void deleteGroup(Integer id);

    void deleteMembersByGroupId(Integer groupId);

    void editGroup(GroupsEntity groupsEntity);

    List<String> getAllGroupNamesByAdmin(String admin);

    GroupsEntity getGroup(String groupName, String admin);

    GroupsEntity getGroupById(Integer id);

    Integer getGroupIdByGroupNameAndAdmin(String groupName, String admin);

    List<GroupsEntity> getGroupsDatabase();

    List<Integer> getMembersId(Integer groupId);

    Integer getUserIdByUserName(String user);

    String getUserNameByUserId(Integer id);

    void insertMember(Integer groupId, Integer userId);
}
