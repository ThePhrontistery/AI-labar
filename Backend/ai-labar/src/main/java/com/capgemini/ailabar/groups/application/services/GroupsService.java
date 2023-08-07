package com.capgemini.ailabar.groups.application.services;

import com.capgemini.ailabar.groups.domain.exceptions.*;
import com.capgemini.ailabar.groups.domain.models.GroupsModel;
import com.capgemini.ailabar.groups.domain.ports.in.*;
import com.capgemini.ailabar.groups.infraestructure.entities.GroupsEntity;
import org.springframework.stereotype.Service;
import java.util.List;

@Service
public class GroupsService implements CreateGroupUseCase, GetGroupUseCase, EditGroupUseCase, GetGroupsByUserUseCase,
        DeleteGroupUseCase, GetGroupsDatabaseUseCase {
    private final CreateGroupUseCase createGroupUseCase;
    private final GetGroupUseCase getGroupUseCase;
    private final EditGroupUseCase editGroupUseCase;
    private final GetGroupsByUserUseCase getGroupsByUserUseCase;
    private final DeleteGroupUseCase deleteGroupUseCase;
    private final GetGroupsDatabaseUseCase getGroupsDatabaseUseCase;

    public GroupsService(CreateGroupUseCase createGroupUseCase, GetGroupUseCase getGroupUseCase,
                         EditGroupUseCase editGroupUseCase, GetGroupsByUserUseCase getGroupsByUserUseCase,
                         DeleteGroupUseCase deleteGroupUseCase, GetGroupsDatabaseUseCase getGroupsDatabaseUseCase) {
        this.createGroupUseCase = createGroupUseCase;
        this.getGroupUseCase = getGroupUseCase;
        this.editGroupUseCase = editGroupUseCase;
        this.getGroupsByUserUseCase = getGroupsByUserUseCase;
        this.deleteGroupUseCase = deleteGroupUseCase;
        this.getGroupsDatabaseUseCase = getGroupsDatabaseUseCase;
    }

    @Override
    public void createGroup(GroupsModel groupsModel) {
        try {
            createGroupUseCase.createGroup(groupsModel);
        } catch (CreateGroupException createGroupException) {
            throw createGroupException;
        }
    }

    @Override
    public GroupsEntity getGroup(GroupsModel groupsModel) {
        try {
            return getGroupUseCase.getGroup(groupsModel);
        } catch (GetGroupException getGroupException) {
            throw getGroupException;
        }
    }

    @Override
    public void editGroup(GroupsModel groupsModel) {
        try {
            editGroupUseCase.editGroup(groupsModel);
        } catch (EditGroupException editGroupException) {
            throw editGroupException;
        }
    }

    @Override
    public void deleteGroup(GroupsModel groupsModel) {
        try {
            deleteGroupUseCase.deleteGroup(groupsModel);
        } catch (DeleteGroupException deleteGroupException) {
            throw deleteGroupException;
        }
    }

    @Override
    public List<String> getGroupsByUser(GroupsModel groupsModel) {
        try {
            return getGroupsByUserUseCase.getGroupsByUser(groupsModel);
        } catch (GetGroupsByUserException getGroupsByUserException) {
            throw getGroupsByUserException;
        }
    }

    @Override
    public List<GroupsEntity> getGroupsDatabase() {
        try {
            return getGroupsDatabaseUseCase.getGroupsDatabase();
        } catch (GetGroupsDatabaseException getGroupsDatabaseException) {
            throw getGroupsDatabaseException;
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
