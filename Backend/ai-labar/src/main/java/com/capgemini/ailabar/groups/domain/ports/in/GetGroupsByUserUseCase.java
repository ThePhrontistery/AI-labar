package com.capgemini.ailabar.groups.domain.ports.in;

import com.capgemini.ailabar.groups.domain.models.GroupsModel;

import java.util.List;

public interface GetGroupsByUserUseCase {
    List<String> getGroupsByUser(GroupsModel groupsModel);
}
