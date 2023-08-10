package com.capgemini.ailabar.groups.domain.ports.in;

import com.capgemini.ailabar.groups.domain.models.GroupsModel;
import com.capgemini.ailabar.groups.infraestructure.entities.GroupsEntity;

import java.util.List;

public interface GetGroupsDatabaseUseCase {
    List<GroupsModel> getGroupsDatabase();
}
