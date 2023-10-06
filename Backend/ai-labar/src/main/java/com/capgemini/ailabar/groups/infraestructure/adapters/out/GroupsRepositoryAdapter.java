package com.capgemini.ailabar.groups.infraestructure.adapters.out;

import com.capgemini.ailabar.groups.domain.ports.out.GroupsRepositoryPort;
import com.capgemini.ailabar.groups.infraestructure.entities.GroupsEntity;
import com.capgemini.ailabar.groups.infraestructure.repositories.GroupsRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public class GroupsRepositoryAdapter implements GroupsRepositoryPort {
    private final GroupsRepository groupsRepository;

    public GroupsRepositoryAdapter(GroupsRepository groupsRepository) {
        this.groupsRepository = groupsRepository;
    }

    @Override
    public boolean checkAuthorization(String user, String token) {
        return groupsRepository.checkAuthorization(user, token);
    }

    @Override
    public boolean checkByGroupIdAndAdmin(Integer id, String admin) {
        return groupsRepository.checkByGroupIdAndAdmin(id, admin);
    }

    @Override
    public boolean checkByGroupNameAndAdmin(String groupName, String admin) {
        return groupsRepository.checkByGroupNameAndAdmin(groupName, admin);
    }

    @Override
    public boolean checkMember(String member) {
        return groupsRepository.checkMember(member);
    }

    @Override
    public void createGroup(GroupsEntity groupsEntity) {
        groupsRepository.save(groupsEntity);
    }

    @Override
    public void deleteGroup(Integer id) {
        groupsRepository.deleteById(id);
    }

    @Override
    public void deleteMembersByGroupId(Integer groupId) {
        groupsRepository.deleteMembersByGroupId(groupId);
    }

    @Override
    public void editGroup(GroupsEntity groupsEntity) {
        groupsRepository.save(groupsEntity);
    }

    @Override
    public List<String> getAllGroupNamesByAdmin(String admin) {
        return groupsRepository.getAllGroupNamesByAdmin(admin);
    }

    @Override
    public GroupsEntity getGroup(String groupName, String admin) {
        return groupsRepository.getGroupByGroupNameAndAdmin(groupName, admin);
    }

    @Override
    public GroupsEntity getGroupById(Integer id) {
        return groupsRepository.getGroupById(id);
    }

    @Override
    public Integer getGroupIdByGroupNameAndAdmin(String groupName, String admin) {
        return groupsRepository.getGroupIdByGroupNameAndAdmin(groupName, admin);
    }

    @Override
    public List<GroupsEntity> getGroupsDatabase() {
        return groupsRepository.findAll();
    }

    @Override
    public List<Integer> getMembersId(Integer groupId) {
        return groupsRepository.getMembersId(groupId);
    }

    @Override
    public Integer getUserIdByUserName(String user) {
        return groupsRepository.getUserIdByUserName(user);
    }

    @Override
    public String getUserNameByUserId(Integer id) {
        return groupsRepository.getUserNameByUserId(id);
    }

    @Override
    public String getUserNameEMailByUserId(Integer id) {
        return groupsRepository.getUserNameEMailByUserId(id);
    }
    @Override
    public void insertMember(Integer groupId, Integer userId) {
        groupsRepository.insertMember(groupId, userId);
    }
}
