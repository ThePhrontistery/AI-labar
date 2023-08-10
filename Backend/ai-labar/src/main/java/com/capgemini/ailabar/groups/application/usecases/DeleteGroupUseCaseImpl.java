package com.capgemini.ailabar.groups.application.usecases;

import com.capgemini.ailabar.groups.domain.exceptions.DeleteGroupException;
import com.capgemini.ailabar.groups.domain.models.GroupsModel;
import com.capgemini.ailabar.groups.domain.ports.in.DeleteGroupUseCase;
import com.capgemini.ailabar.groups.domain.ports.out.GroupsRepositoryPort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional
public class DeleteGroupUseCaseImpl implements DeleteGroupUseCase {
    private final GroupsRepositoryPort groupsRepositoryPort;

    public DeleteGroupUseCaseImpl(GroupsRepositoryPort groupsRepositoryPort) {
        this.groupsRepositoryPort = groupsRepositoryPort;
    }

    @Override
    public void deleteGroup(GroupsModel groupsModel) {
        if((groupsModel.getId() == null && groupsModel.getGroupName() == null || groupsModel.getGroupName().strip().isBlank())
                || groupsModel.getUser().isBlank() || groupsModel.getToken().isBlank()) {
            throw new DeleteGroupException("Group name or group id and administrator are required to delete a group");
        }

        if(groupsModel.getId() != null && groupsModel.getId() == 0) {
            throw new DeleteGroupException("Group id cannot be zero");
        }

        if(Boolean.FALSE.equals(groupsRepositoryPort.checkAuthorization(groupsModel.getUser(), groupsModel.getToken()))) {
            throw new DeleteGroupException("Unauthorized user");
        }

        if(groupsModel.getId() == null) {
            if(Boolean.FALSE.equals(groupsRepositoryPort.checkByGroupNameAndAdmin(groupsModel.getGroupName().strip(), groupsModel.getUser()))) {
                throw new DeleteGroupException("The user does not have a group with that name");
            }

            groupsRepositoryPort.deleteMembersByGroupId(groupsRepositoryPort.getGroupIdByGroupNameAndAdmin(groupsModel.getGroupName().strip(), groupsModel.getUser()));
            groupsRepositoryPort.deleteGroup(groupsRepositoryPort.getGroupIdByGroupNameAndAdmin(groupsModel.getGroupName().strip(), groupsModel.getUser()));
            return;
        }

        if(Boolean.FALSE.equals(groupsRepositoryPort.checkByGroupIdAndAdmin(groupsModel.getId(), groupsModel.getUser()))) {
            throw new DeleteGroupException("The user does not have a group with that id");
        }

        groupsRepositoryPort.deleteMembersByGroupId(groupsModel.getId());
        groupsRepositoryPort.deleteGroup(groupsModel.getId());

    }
}
