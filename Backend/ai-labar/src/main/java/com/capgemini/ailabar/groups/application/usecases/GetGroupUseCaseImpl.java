package com.capgemini.ailabar.groups.application.usecases;

import com.capgemini.ailabar.groups.domain.exceptions.CreateGroupException;
import com.capgemini.ailabar.groups.domain.exceptions.GetGroupException;
import com.capgemini.ailabar.groups.domain.models.GroupsModel;
import com.capgemini.ailabar.groups.domain.ports.in.GetGroupUseCase;
import com.capgemini.ailabar.groups.domain.ports.out.GroupsRepositoryPort;
import com.capgemini.ailabar.groups.infraestructure.entities.GroupsEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Service
@Transactional(readOnly = true)
public class GetGroupUseCaseImpl implements GetGroupUseCase {
    private final GroupsRepositoryPort groupsRepositoryPort;

    public GetGroupUseCaseImpl(GroupsRepositoryPort groupsRepositoryPort) {
        this.groupsRepositoryPort = groupsRepositoryPort;
    }

    @Override
    public GroupsModel getGroup(GroupsModel groupsModel) {
        if(groupsModel.getGroupName().isBlank() || groupsModel.getUser().isBlank()
                || groupsModel.getToken().isBlank()) {
            throw new GetGroupException("Group name and user are required");
        }

        if(Boolean.FALSE.equals(groupsRepositoryPort.checkAuthorization(groupsModel.getUser(), groupsModel.getToken()))) {
            throw new GetGroupException("Unauthorized user");
        }

        GroupsEntity groupEntity = groupsRepositoryPort.getGroup(groupsModel.getGroupName(), groupsModel.getUser());

        if(groupEntity == null) {
            throw new GetGroupException("The user does not have a group with that name");
        }

        List<Integer> membersIdList = groupsRepositoryPort.getMembersId(groupEntity.getId());
        List<String> membersList = new ArrayList<>();
        membersIdList.forEach(id -> {
            try {
                membersList.add(groupsRepositoryPort.getUserNameByUserId(id));
            } catch (GetGroupException getGroupException) {
                throw new GetGroupException("An error occurred while retrieving group members");
            }
        });

        GroupsModel matchedGroup = new GroupsModel(groupEntity);
        matchedGroup.setMembers(membersList);

        return matchedGroup;
    }
}
