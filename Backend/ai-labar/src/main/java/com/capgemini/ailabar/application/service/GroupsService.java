package com.capgemini.ailabar.application.service;

import com.capgemini.ailabar.infrastructure.repository.GroupsRepository;
import com.capgemini.ailabar.infrastructure.entity.GroupsEntity;
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

    public List<String> getGroupForEdit(String admin) {
        return groupsRepository.findAllGroupNamesByAdmin(admin);
    }

    public void deleteGroup(String groupName, String admin) {
        groupsRepository.deleteByGroupNameAndAdmin(groupName, admin);
    }

    public GroupsEntity findGroupsEntityById(Integer id) {
        return groupsRepository.findGroupsEntityById(id);
    }

    /* Inicio métodos sólo para pruebas */
    public List<GroupsEntity> getAllGroupsData() {
        return groupsRepository.findAll();
    }
    /* Fin métodos sólo para pruebas */
}
