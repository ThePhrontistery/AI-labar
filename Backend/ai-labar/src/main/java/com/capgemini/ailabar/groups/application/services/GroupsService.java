package com.capgemini.ailabar.groups.application.services;

import com.capgemini.ailabar.groups.domain.exceptions.CreateGroupException;
import com.capgemini.ailabar.groups.domain.exceptions.EditGroupException;
import com.capgemini.ailabar.groups.domain.exceptions.GetGroupException;
import com.capgemini.ailabar.groups.domain.exceptions.GetGroupsByUserException;
import com.capgemini.ailabar.groups.domain.models.GroupsModel;
import com.capgemini.ailabar.groups.domain.ports.in.CreateGroupUseCase;
import com.capgemini.ailabar.groups.domain.ports.in.EditGroupUseCase;
import com.capgemini.ailabar.groups.domain.ports.in.GetGroupUseCase;
import com.capgemini.ailabar.groups.domain.ports.in.GetGroupsByUserUseCase;
import com.capgemini.ailabar.groups.infraestructure.entities.GroupsEntity;
import com.capgemini.ailabar.groups.infraestructure.repositories.GroupsRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import javax.transaction.Transactional;
import java.util.List;

@Service
@Transactional
public class GroupsService {
    private final CreateGroupUseCase createGroupUseCase;
    private final GetGroupUseCase getGroupUseCase;
    private final EditGroupUseCase editGroupUseCase;
    private final GetGroupsByUserUseCase getGroupsByUserUseCase;

    public GroupsService(CreateGroupUseCase createGroupUseCase, GetGroupUseCase getGroupUseCase,
                         EditGroupUseCase editGroupUseCase, GetGroupsByUserUseCase getGroupsByUserUseCase) {
        this.createGroupUseCase = createGroupUseCase;
        this.getGroupUseCase = getGroupUseCase;
        this.editGroupUseCase = editGroupUseCase;
        this.getGroupsByUserUseCase = getGroupsByUserUseCase;
    }

    public void createGroup(GroupsModel groupsModel) {
        try {
            createGroupUseCase.createGroup(groupsModel);
        } catch (CreateGroupException createGroupException) {
            throw createGroupException;
        }
    }

    public GroupsEntity getGroup(GroupsModel groupsModel) {
        try {
            return getGroupUseCase.getGroup(groupsModel);
        } catch (GetGroupException getGroupException) {
            throw getGroupException;
        }
    }

    public void editGroup(GroupsModel groupsModel) {
        try {
            editGroupUseCase.editGroup(groupsModel);
        } catch (EditGroupException editGroupException) {
            throw editGroupException;
        }
    }

    public List<String> getGroupsByUser(GroupsModel groupsModel) {
        try {
            return getGroupsByUserUseCase.getGroupsByUser(groupsModel);
        } catch (GetGroupsByUserException getGroupsByUserException) {
            throw getGroupsByUserException;
        }
    }

//    public Boolean existsByGroupNameAndAdmin(String groupName, String admin){
//        return groupsRepository.existsByGroupNameAndAdmin(groupName, admin);
//    }
//
//    public void saveGroup(GroupsEntity groupEntity) {
//        groupsRepository.save(groupEntity);
//    }
//
//    public GroupsEntity getGroup(String groupName, String admin) {
//        return groupsRepository.findByGroupNameAndAdmin(groupName, admin);
//    }
//
//    public List<String> getGroupForEdit(String admin) {
//        return groupsRepository.findAllGroupNamesByAdmin(admin);
//    }
//
//    public void deleteGroup(String groupName, String admin) {
//        groupsRepository.deleteByGroupNameAndAdmin(groupName, admin);
//    }
//
//    public GroupsEntity findGroupsEntityById(Integer id) {
//        return groupsRepository.findGroupsEntityById(id);
//    }

    /* Inicio métodos sólo para pruebas */
//    public List<GroupsEntity> getAllGroupsData() {
//        return groupsRepository.findAll();
//    }
    /* Fin métodos sólo para pruebas */


}
