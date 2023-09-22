package com.capgemini.ailabar.users.application.usecases;

import com.capgemini.ailabar.users.domain.exceptions.LoginException;
import com.capgemini.ailabar.users.domain.exceptions.LogoutException;
import com.capgemini.ailabar.users.domain.models.UsersModel;
import com.capgemini.ailabar.users.domain.ports.in.LogoutUseCase;
import com.capgemini.ailabar.users.domain.ports.out.UsersRepositoryPort;
import com.capgemini.ailabar.users.infraestructure.entities.UsersEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional
public class LogoutUseCaseImpl implements LogoutUseCase {
    private final UsersRepositoryPort usersRepositoryPort;

    public LogoutUseCaseImpl(UsersRepositoryPort usersRepositoryPort) {
        this.usersRepositoryPort = usersRepositoryPort;
    }

    @Override
    public void logout(UsersModel usersModel) {
        if (usersModel.getUser().isBlank() || usersModel.getToken().isBlank()) {
            throw new LogoutException("User and token are required to logout");
        }

        if(Boolean.FALSE.equals(usersRepositoryPort.checkAuthorization(usersModel.getUser(), usersModel.getToken()))) {
            throw new LogoutException("Unauthorized user");
        }

        UsersEntity usersEntity = usersRepositoryPort.getUserByName(usersModel.getUser());

        if(usersEntity == null) {
            throw new LoginException("User not found");
        }

        usersEntity.setOnline(0);

        usersRepositoryPort.editUser(usersEntity);
    }
}
