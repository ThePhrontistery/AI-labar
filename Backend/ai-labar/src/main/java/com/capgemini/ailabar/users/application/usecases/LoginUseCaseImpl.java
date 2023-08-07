package com.capgemini.ailabar.users.application.usecases;

import com.capgemini.ailabar.users.domain.exceptions.LoginException;
import com.capgemini.ailabar.users.domain.models.UsersModel;
import com.capgemini.ailabar.users.domain.ports.in.LoginUseCase;
import com.capgemini.ailabar.users.domain.ports.out.UsersRepositoryPort;
import com.capgemini.ailabar.users.infraestructure.entities.UsersEntity;
import org.apache.commons.codec.digest.DigestUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(readOnly = true)
public class LoginUseCaseImpl implements LoginUseCase {
    private final UsersRepositoryPort usersRepositoryPort;

    public LoginUseCaseImpl(UsersRepositoryPort usersRepositoryPort) {
        this.usersRepositoryPort = usersRepositoryPort;
    }

    @Override
    public String login(UsersModel usersModel) {
        if (usersModel.getUser().isBlank() || usersModel.getPassword().isBlank()) {
            throw new LoginException("User and password are required to login");
        }

        if (Boolean.FALSE.equals(usersRepositoryPort.login(usersModel.getUser(), DigestUtils.sha256Hex(usersModel.getPassword())))) {
            throw new LoginException("Login failed");
        }

        UsersEntity usersEntity = usersRepositoryPort.getUserByName(usersModel.getUser());

        if(usersEntity == null) {
            throw new LoginException("User not found");
        }

        return usersEntity.getToken();
    }
}
