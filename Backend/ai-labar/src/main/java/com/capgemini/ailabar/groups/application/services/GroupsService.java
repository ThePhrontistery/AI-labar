package com.capgemini.ailabar.groups.application.services;

import com.capgemini.ailabar.groups.domain.exceptions.*;
import com.capgemini.ailabar.groups.domain.models.GroupsModel;
import com.capgemini.ailabar.groups.domain.ports.in.*;
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
    public GroupsModel getGroup(GroupsModel groupsModel) {
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
    public List<GroupsModel> getGroupsDatabase() {
        try {
            return getGroupsDatabaseUseCase.getGroupsDatabase();
        } catch (GetGroupsDatabaseException getGroupsDatabaseException) {
            throw getGroupsDatabaseException;
        }
    }
}
