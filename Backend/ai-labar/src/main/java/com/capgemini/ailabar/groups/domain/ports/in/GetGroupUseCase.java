package com.capgemini.ailabar.groups.domain.ports.in;

import com.capgemini.ailabar.groups.domain.models.GroupsModel;

public interface GetGroupUseCase {
    GroupsModel getGroup(GroupsModel groupsModel);
}
