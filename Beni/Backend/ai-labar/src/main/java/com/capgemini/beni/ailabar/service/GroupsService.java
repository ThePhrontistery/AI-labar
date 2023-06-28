package com.capgemini.beni.ailabar.service;

import com.capgemini.beni.ailabar.entity.GroupsEntity;
import com.capgemini.beni.ailabar.repository.GroupsRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.util.List;

@Service
@Transactional
public class GroupsService {
    private final GroupsRepository groupsRepository;

    @Autowired
    public GroupsService(GroupsRepository groupsRepository) {
        this.groupsRepository = groupsRepository;
    }

    public Boolean existsByGroupNameAndAdmin(String groupName, String admin){
        return groupsRepository.existsByGroupNameAndAdmin(groupName, admin);
    }

    public void saveGroup(GroupsEntity groupEntity) {
        groupsRepository.save(groupEntity);
    }

    public GroupsEntity getGroup(String groupName, String admin) {
        return groupsRepository.findByGroupNameAndAdmin(groupName, admin);
    }

    public List<String> editGroup(String admin) {
        return groupsRepository.findAllGroupNamesByAdmin(admin);
    }

    public void deleteGroup(String groupName, String admin) {
        groupsRepository.deleteByGroupNameAndAdmin(groupName, admin);
    }
}
