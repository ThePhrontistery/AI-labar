package com.capgemini.ailabar.users.application.usecases;

import com.capgemini.ailabar.users.domain.exceptions.GetUsersByMatchException;
import com.capgemini.ailabar.users.domain.ports.in.GetUsersByMatchUseCase;
import com.capgemini.ailabar.users.domain.models.UsersModel;
import com.capgemini.ailabar.users.domain.ports.out.UsersRepositoryPort;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class GetUsersByMatchUseCaseImpl implements GetUsersByMatchUseCase {
    private final UsersRepositoryPort usersRepositoryPort;

    public GetUsersByMatchUseCaseImpl(UsersRepositoryPort usersRepositoryPort) {
        this.usersRepositoryPort = usersRepositoryPort;
    }

    @Override
    public List<String> getUsersByMatch(UsersModel usersModel) {
        if(usersModel.getUser().isBlank() || usersModel.getToken().isBlank()) {
            throw new GetUsersByMatchException("User and token are required");
        }

        if(Boolean.FALSE.equals(usersRepositoryPort.checkAuthorization(usersModel.getUser(), usersModel.getToken()))) {
            throw new GetUsersByMatchException("Unauthorized user");
        }

        List<String> usersMatchesList = usersRepositoryPort.getUsersByMatch(usersModel.getMatcher());
        if(usersMatchesList == null) {
            throw new GetUsersByMatchException("Not matches");
        }

        return usersMatchesList;
    }
}
