package com.capgemini.ailabar.users.application.usecases;

import com.capgemini.ailabar.users.domain.exceptions.GetAllUsersException;
import com.capgemini.ailabar.users.domain.ports.in.GetAllUsersUseCase;
import com.capgemini.ailabar.users.domain.models.UsersModel;
import com.capgemini.ailabar.users.domain.ports.out.UsersRepositoryPort;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class GetAllUsersUseCaseImpl implements GetAllUsersUseCase {
    private final UsersRepositoryPort usersRepositoryPort;

    public GetAllUsersUseCaseImpl(UsersRepositoryPort usersRepositoryPort) {
        this.usersRepositoryPort = usersRepositoryPort;
    }

    @Override
    public List<String> getAllUsers(UsersModel usersModel) {
        if(usersModel.getUser().isBlank() || usersModel.getToken().isBlank()) {
            throw new GetAllUsersException("User name and token are required to delete a user");
        }

        if(Boolean.FALSE.equals(usersRepositoryPort.checkAuthorization(usersModel.getUser(), usersModel.getToken()))) {
            throw new GetAllUsersException("Unauthorized user");
        }

        return usersRepositoryPort.getAllUsers();
    }
}
