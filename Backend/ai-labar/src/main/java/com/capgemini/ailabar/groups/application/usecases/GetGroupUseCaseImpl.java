package com.capgemini.ailabar.groups.application.usecases;

import com.capgemini.ailabar.groups.domain.exceptions.GetGroupException;
import com.capgemini.ailabar.groups.domain.models.GroupsModel;
import com.capgemini.ailabar.groups.domain.ports.in.GetGroupUseCase;
import com.capgemini.ailabar.groups.domain.ports.out.GroupsRepositoryPort;
import com.capgemini.ailabar.groups.infraestructure.entities.GroupsEntity;
import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.lang.reflect.Type;
import java.util.List;

public class GetGroupUseCaseImpl implements GetGroupUseCase {
    private final GroupsRepositoryPort groupsRepositoryPort;

    public GetGroupUseCaseImpl(GroupsRepositoryPort groupsRepositoryPort) {
        this.groupsRepositoryPort = groupsRepositoryPort;
    }

    @Override
    public GroupsEntity getGroup(GroupsModel groupsModel) {
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

        GroupsEntity matchedGroup = new GroupsEntity();
        matchedGroup.setId(groupEntity.getId());
        matchedGroup.setGroupName(groupEntity.getGroupName());

        Gson gson = new Gson();
        Type listType = new TypeToken<List<String>>() {}.getType();
        matchedGroup.setMembers(gson.fromJson(groupEntity.getMembers(), listType));
        matchedGroup.setAdmin(groupEntity.getAdmin());

        return matchedGroup;
    }
}
