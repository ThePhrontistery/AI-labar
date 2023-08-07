package com.capgemini.ailabar.groups.application.usecases;

import com.capgemini.ailabar.groups.domain.exceptions.CreateGroupException;
import com.capgemini.ailabar.groups.domain.models.GroupsModel;
import com.capgemini.ailabar.groups.domain.ports.in.CreateGroupUseCase;
import com.capgemini.ailabar.groups.domain.ports.out.GroupsRepositoryPort;
import com.capgemini.ailabar.groups.infraestructure.entities.GroupsEntity;
import com.google.gson.Gson;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional
public class CreateGroupUseCaseImpl implements CreateGroupUseCase {
    private final GroupsRepositoryPort groupsRepositoryPort;

    public CreateGroupUseCaseImpl(GroupsRepositoryPort groupsRepositoryPort) {
        this.groupsRepositoryPort = groupsRepositoryPort;
    }

    @Override
    public void createGroup(GroupsModel groupsModel) {
        if(groupsModel.getGroupName().isBlank() || groupsModel.getMembers().isEmpty()
                || groupsModel.getUser().isBlank() || groupsModel.getToken().isBlank()) {
            throw new CreateGroupException("All data is required to save a group");
        }

        if(Boolean.FALSE.equals(groupsRepositoryPort.checkAuthorization(groupsModel.getUser(), groupsModel.getToken()))) {
            throw new CreateGroupException("Unauthorized user");
        }

        if(Boolean.TRUE.equals(groupsRepositoryPort.checkByGroupNameAndAdmin(groupsModel.getGroupName().strip(), groupsModel.getUser()))) {
            throw new CreateGroupException("The user already has a group with that name");
        }

        GroupsEntity groupEntity = new GroupsEntity(groupsModel);
        groupEntity.setAdmin(groupsModel.getUser().strip());
        groupEntity.setMembers(new Gson().toJson(groupsModel.getMembers()));

        groupsRepositoryPort.createGroup(groupEntity);
    }
}
