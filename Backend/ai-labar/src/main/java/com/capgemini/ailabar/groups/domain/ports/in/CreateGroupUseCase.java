package com.capgemini.ailabar.groups.domain.ports.in;

import com.capgemini.ailabar.groups.domain.models.GroupsModel;

public interface CreateGroupUseCase {
    void createGroup(GroupsModel groupsModel);
}
