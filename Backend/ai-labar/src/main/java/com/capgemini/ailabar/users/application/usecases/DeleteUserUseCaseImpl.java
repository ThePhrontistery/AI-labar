package com.capgemini.ailabar.users.application.usecases;

import com.capgemini.ailabar.users.domain.exceptions.DeleteUserException;
import com.capgemini.ailabar.users.domain.ports.in.DeleteUserUseCase;
import com.capgemini.ailabar.users.domain.models.UsersModel;
import com.capgemini.ailabar.users.domain.ports.out.UsersRepositoryPort;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;

@Service
@Transactional
public class DeleteUserUseCaseImpl implements DeleteUserUseCase {
    private final UsersRepositoryPort usersRepositoryPort;

    public DeleteUserUseCaseImpl(UsersRepositoryPort usersRepositoryPort) {
        this.usersRepositoryPort = usersRepositoryPort;
    }

    @Override
    public void deleteUser(UsersModel usersModel) {
        if(usersModel.getUser().isBlank() || usersModel.getToken().isBlank()) {
            throw new DeleteUserException("User name and token are required to delete a user");
        }

        if(Boolean.FALSE.equals(usersRepositoryPort.checkAuthorization(usersModel.getUser(), usersModel.getToken()))) {
            throw new DeleteUserException("Unauthorized user");
        }

        usersRepositoryPort.deleteUser(usersModel.getUser());
    }
}
