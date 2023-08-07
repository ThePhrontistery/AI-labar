package com.capgemini.ailabar.groups.application.usecases;

import com.capgemini.ailabar.groups.domain.exceptions.GetGroupsByUserException;
import com.capgemini.ailabar.groups.domain.models.GroupsModel;
import com.capgemini.ailabar.groups.domain.ports.in.GetGroupsByUserUseCase;
import com.capgemini.ailabar.groups.domain.ports.out.GroupsRepositoryPort;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
@Transactional(readOnly = true)
public class GetGroupsByUserUseCaseImpl implements GetGroupsByUserUseCase {
    private final GroupsRepositoryPort groupsRepositoryPort;

    public GetGroupsByUserUseCaseImpl(GroupsRepositoryPort groupsRepositoryPort) {
        this.groupsRepositoryPort = groupsRepositoryPort;
    }

    @Override
    public List<String> getGroupsByUser(GroupsModel groupsModel) {
        if(groupsModel.getUser().isBlank() || groupsModel.getToken().isBlank()) {
            throw new GetGroupsByUserException("User and token are required");
        }

        if(Boolean.FALSE.equals(groupsRepositoryPort.checkAuthorization(groupsModel.getUser(), groupsModel.getToken()))) {
            throw new GetGroupsByUserException("Unauthorized user");
        }

        List<String> groupsList = groupsRepositoryPort.getAllGroupNamesByAdmin(groupsModel.getUser());
        if(groupsList.isEmpty()) {
            throw new GetGroupsByUserException("The user is not part of any group");
        }

        return groupsList;
    }
}
