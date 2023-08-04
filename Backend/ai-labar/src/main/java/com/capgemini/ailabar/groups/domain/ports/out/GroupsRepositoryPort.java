package com.capgemini.ailabar.groups.domain.ports.out;

import com.capgemini.ailabar.groups.infraestructure.entities.GroupsEntity;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface GroupsRepositoryPort {
    void createGroup(GroupsEntity groupsEntity);
    GroupsEntity getGroup(String groupName, String admin);

    boolean checkAuthorization(String user, String token);
    boolean checkByGroupNameAndAdmin(String groupName, String admin);
    GroupsEntity getGroupById(Integer id);
    List<String> getAllGroupNamesByAdmin(String admin);
}
