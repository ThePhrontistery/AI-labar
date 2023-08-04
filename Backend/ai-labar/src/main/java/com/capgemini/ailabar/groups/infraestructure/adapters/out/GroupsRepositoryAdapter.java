package com.capgemini.ailabar.groups.infraestructure.adapters.out;

import com.capgemini.ailabar.groups.domain.ports.out.GroupsRepositoryPort;
import com.capgemini.ailabar.groups.infraestructure.entities.GroupsEntity;
import com.capgemini.ailabar.groups.infraestructure.repositories.GroupsRepository;

import java.util.List;

public class GroupsRepositoryAdapter implements GroupsRepositoryPort {
    private final GroupsRepository groupsRepository;

    public GroupsRepositoryAdapter(GroupsRepository groupsRepository) {
        this.groupsRepository = groupsRepository;
    }

    @Override
    public void createGroup(GroupsEntity groupsEntity) {
        groupsRepository.save(groupsEntity);
    }

    @Override
    public GroupsEntity getGroup(String groupName, String admin) {
        return groupsRepository.getGroupByGroupNameAndAdmin(groupName, admin);
    }

    @Override
    public boolean checkAuthorization(String user, String token) {
        return groupsRepository.checkAuthorization(user, token);
    }

    @Override
    public boolean checkByGroupNameAndAdmin(String groupName, String admin) {
        return groupsRepository.checkByGroupNameAndAdmin(groupName, admin);
    }

    @Override
    public GroupsEntity getGroupById(Integer id) {
        return groupsRepository.getGroupById(id);
    }

    @Override
    public List<String> getAllGroupNamesByAdmin(String admin) {
        return groupsRepository.getAllGroupNamesByAdmin(admin);
    }
}
