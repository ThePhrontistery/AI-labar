package com.capgemini.ailabar.groups.application.usecases;

import com.capgemini.ailabar.groups.domain.exceptions.EditGroupException;
import com.capgemini.ailabar.groups.domain.models.GroupsModel;
import com.capgemini.ailabar.groups.domain.ports.in.EditGroupUseCase;
import com.capgemini.ailabar.groups.domain.ports.out.GroupsRepositoryPort;
import com.capgemini.ailabar.groups.infraestructure.entities.GroupsEntity;
import com.google.gson.Gson;

public class EditGroupUseCaseImplmpl implements EditGroupUseCase {
    private final GroupsRepositoryPort groupsRepositoryPort;

    public EditGroupUseCaseImplmpl(GroupsRepositoryPort groupsRepositoryPort) {
        this.groupsRepositoryPort = groupsRepositoryPort;
    }

    @Override
    public void editGroup(GroupsModel groupsModel) {
        if(groupsModel.getId() == null || groupsModel.getGroupName().isBlank()
                || groupsModel.getMembers().isEmpty() || groupsModel.getUser().isBlank()
                || groupsModel.getToken().isBlank()) {
            throw new EditGroupException("All data is required to save a group");
        }

        if(Boolean.FALSE.equals(groupsRepositoryPort.checkAuthorization(groupsModel.getUser(), groupsModel.getToken()))) {
            throw new EditGroupException("Unauthorized user");
        }

        GroupsEntity groupEntity = groupsRepositoryPort.getGroupById(groupsModel.getId());

        if(!groupEntity.getAdmin().equals(groupsModel.getUser())) {
            throw new EditGroupException("The user is not the group administrator");
        }

        groupEntity.setMembers(new Gson().toJson(groupsModel.getMembers()));

        if(groupsModel.getNewGroupName() != null && !groupsModel.getNewGroupName().isBlank()) {
            if(Boolean.TRUE.equals(groupsRepositoryPort.checkByGroupNameAndAdmin(groupsModel.getNewGroupName().strip(), groupsModel.getUser()))) {
                throw new EditGroupException("The user already has a group with that name");
            } else {
                groupEntity.setGroupName(groupsModel.getNewGroupName().strip());
            }
        }
    }
}
