package com.capgemini.ailabar.groups.domain.ports.in;

import com.capgemini.ailabar.groups.domain.models.GroupsModel;
import com.capgemini.ailabar.groups.infraestructure.entities.GroupsEntity;

public interface GetGroupUseCase {
    GroupsEntity getGroup(GroupsModel groupsModel);
}
