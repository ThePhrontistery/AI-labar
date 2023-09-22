package com.capgemini.ailabar.users.application.usecases;

import com.capgemini.ailabar.users.domain.exceptions.GetUsersDatabaseException;
import com.capgemini.ailabar.users.domain.models.UsersModel;
import com.capgemini.ailabar.users.domain.ports.in.GetUsersDatabaseUseCase;
import com.capgemini.ailabar.users.domain.ports.out.UsersRepositoryPort;
import com.capgemini.ailabar.users.infraestructure.entities.UsersEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;

@Service
@Transactional(readOnly = true)
public class GetUsersDatabaseUseCaseImpl implements GetUsersDatabaseUseCase {
    private final UsersRepositoryPort usersRepositoryPort;

    public GetUsersDatabaseUseCaseImpl(UsersRepositoryPort usersRepositoryPort) {
        this.usersRepositoryPort = usersRepositoryPort;
    }

    @Override
    public List<UsersModel> getUsersDatabase() {
        List<UsersEntity> usersEntitiesList = usersRepositoryPort.getUsersDatabase();

        if(usersEntitiesList.isEmpty()) {
            throw new GetUsersDatabaseException("There are no users in database");
        }

        List<UsersModel> usersModelList = new ArrayList<>();

        for (UsersEntity usersEntity : usersEntitiesList) {
            UsersModel usersModel = new UsersModel(usersEntity);
            usersModelList.add(usersModel);
        }

        return usersModelList;
    }
}
