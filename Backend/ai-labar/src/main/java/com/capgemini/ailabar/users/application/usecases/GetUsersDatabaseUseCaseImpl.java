package com.capgemini.ailabar.users.application.usecases;

import com.capgemini.ailabar.users.domain.exceptions.GetUsersDatabaseException;
import com.capgemini.ailabar.users.domain.ports.in.GetUsersDatabaseUseCase;
import com.capgemini.ailabar.users.domain.ports.out.UsersRepositoryPort;
import com.capgemini.ailabar.users.infraestructure.entities.UsersEntity;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class GetUsersDatabaseUseCaseImpl implements GetUsersDatabaseUseCase {
    private final UsersRepositoryPort usersRepositoryPort;

    public GetUsersDatabaseUseCaseImpl(UsersRepositoryPort usersRepositoryPort) {
        this.usersRepositoryPort = usersRepositoryPort;
    }

    @Override
    public List<UsersEntity> getUsersDatabase() {
        List<UsersEntity> usersEntitiesList = usersRepositoryPort.getUsersDatabase();

        if(usersEntitiesList.isEmpty()) {
            throw new GetUsersDatabaseException("There are no users in database");
        }

        return usersEntitiesList;
    }
}
