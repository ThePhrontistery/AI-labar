package com.capgemini.ailabar.groups.application.usecases;

import com.capgemini.ailabar.groups.domain.exceptions.GetGroupsDatabaseException;
import com.capgemini.ailabar.groups.domain.ports.in.GetGroupsDatabaseUseCase;
import com.capgemini.ailabar.groups.domain.ports.out.GroupsRepositoryPort;
import com.capgemini.ailabar.groups.infraestructure.entities.GroupsEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
@Transactional(readOnly = true)
public class GetGroupsDatabaseUseCaseImpl implements GetGroupsDatabaseUseCase {
    private final GroupsRepositoryPort groupsRepositoryPort;

    public GetGroupsDatabaseUseCaseImpl(GroupsRepositoryPort groupsRepositoryPort) {
        this.groupsRepositoryPort = groupsRepositoryPort;
    }

    @Override
    public List<GroupsEntity> getGroupsDatabase() {
        List<GroupsEntity> groupsList = groupsRepositoryPort.getGroupsDatabase();

        if (groupsList.isEmpty()) {
            throw new GetGroupsDatabaseException("There are no groups in the database");
        }
        /* Revisar en la nueva versión ya que cambiará */
//        for (GroupsEntity groupEntity : groupsList) {
//            GroupsModel groupModel = new GroupsModel();
//            groupModel.setId(groupEntity.getId());
//            groupModel.setGroupName(groupEntity.getGroupName());
//
//            Gson gson = new Gson();
//            Type listType = new TypeToken<List<String>>() {}.getType();
//            groupModel.setMembers(gson.fromJson(groupEntity.getMembers(), listType));
//            groupModel.setAdmin(groupEntity.getAdmin());
//
//            groupsModelList.add(groupModel);
//        }

        return groupsList;
    }
}
