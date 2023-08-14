package com.capgemini.ailabar.groups.application.usecases;

import com.capgemini.ailabar.groups.domain.exceptions.GetGroupsDatabaseException;
import com.capgemini.ailabar.groups.domain.models.GroupsModel;
import com.capgemini.ailabar.groups.domain.ports.in.GetGroupsDatabaseUseCase;
import com.capgemini.ailabar.groups.domain.ports.out.GroupsRepositoryPort;
import com.capgemini.ailabar.groups.infraestructure.entities.GroupsEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;

@Service
@Transactional(readOnly = true)
public class GetGroupsDatabaseUseCaseImpl implements GetGroupsDatabaseUseCase {
    private final GroupsRepositoryPort groupsRepositoryPort;

    public GetGroupsDatabaseUseCaseImpl(GroupsRepositoryPort groupsRepositoryPort) {
        this.groupsRepositoryPort = groupsRepositoryPort;
    }

    @Override
    public List<GroupsModel> getGroupsDatabase() {
        List<GroupsEntity> groupsList = groupsRepositoryPort.getGroupsDatabase();

        if (groupsList.isEmpty()) {
            throw new GetGroupsDatabaseException("There are no groups in the database");
        }

        List<GroupsModel> groupsModelList = new ArrayList<>();
        /* Revisar en la nueva versión ya que cambiará */
        for (GroupsEntity groupEntity : groupsList) {
            GroupsModel groupModel = new GroupsModel(groupEntity);

            List<Integer> membersIdList = groupsRepositoryPort.getMembersId(groupEntity.getId());
            List<String> membersList = new ArrayList<>();
            membersIdList.forEach(id -> {
                try {
                    membersList.add(groupsRepositoryPort.getUserNameByUserId(id));
                } catch (GetGroupsDatabaseException getGroupsDatabaseException) {
                    throw new GetGroupsDatabaseException("An error occurred while retrieving group members");
                }
            });

            groupModel.setMembers(membersList);

            groupsModelList.add(groupModel);
        }

        return groupsModelList;
    }
}
