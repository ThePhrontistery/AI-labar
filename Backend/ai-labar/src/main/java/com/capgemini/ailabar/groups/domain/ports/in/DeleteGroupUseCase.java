package com.capgemini.ailabar.groups.domain.ports.in;

import com.capgemini.ailabar.groups.domain.models.GroupsModel;

public interface DeleteGroupUseCase {
    void deleteGroup(GroupsModel groupsModel);
}
