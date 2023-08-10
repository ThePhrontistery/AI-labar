package com.capgemini.ailabar.groups.domain.ports.out;

import com.capgemini.ailabar.groups.infraestructure.entities.GroupsEntity;
import com.capgemini.ailabar.members.infraestructure.entities.MembersEntity;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface GroupsRepositoryPort {
    void createGroup(GroupsEntity groupsEntity);
    GroupsEntity getGroup(String groupName, String admin);
    void editGroup(GroupsEntity groupsEntity);
    void deleteGroup(Integer id);
    List<GroupsEntity> getGroupsDatabase();
    void insertMember(Integer groupId, Integer userId);
    void deleteMembersByGroupId(Integer groupId);
    List<Integer> getMembersId(Integer groupId);

    boolean checkAuthorization(String user, String token);
    boolean checkByGroupIdAndAdmin(Integer id, String admin);
    boolean checkByGroupNameAndAdmin(String groupName, String admin);
    boolean checkMember(String member);
    GroupsEntity getGroupById(Integer id);
    Integer getGroupIdByGroupNameAndAdmin(String groupName, String admin);
    Integer getUserIdByUserName(String user);
    String getUserNameByUserId(Integer id);
    List<String> getAllGroupNamesByAdmin(String admin);
}
