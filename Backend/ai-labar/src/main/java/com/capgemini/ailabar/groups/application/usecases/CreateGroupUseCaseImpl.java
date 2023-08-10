package com.capgemini.ailabar.groups.application.usecases;

import com.capgemini.ailabar.groups.domain.exceptions.CreateGroupException;
import com.capgemini.ailabar.groups.domain.models.GroupsModel;
import com.capgemini.ailabar.groups.domain.ports.in.CreateGroupUseCase;
import com.capgemini.ailabar.groups.domain.ports.out.GroupsRepositoryPort;
import com.capgemini.ailabar.groups.infraestructure.entities.GroupsEntity;
import com.capgemini.ailabar.members.domain.models.MembersModel;
import com.capgemini.ailabar.members.infraestructure.entities.MembersEntity;
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

        groupsModel.getMembers().forEach(member -> {
            if (!groupsRepositoryPort.checkMember(member)) {
                throw new CreateGroupException("The member "+ member +" is not a valid user");
            }
        });

        GroupsEntity groupsEntity = new GroupsEntity(groupsModel);

        groupsRepositoryPort.createGroup(groupsEntity);

        Integer groupId = groupsRepositoryPort.getGroupIdByGroupNameAndAdmin(groupsEntity.getGroupName(), groupsEntity.getAdmin());

        groupsModel.getMembers().forEach(member -> {
            try {
                if(!groupsEntity.getAdmin().equals(member)) {
                    groupsRepositoryPort.insertMember(groupId, groupsRepositoryPort.getUserIdByUserName(member));
                }
            } catch (CreateGroupException createGroupException) {
                groupsRepositoryPort.deleteGroup(groupId);
                throw new CreateGroupException("An error occurred during the registration of group members");
            }
        });
    }
}
