package com.capgemini.ailabar.users.application.usecases;

import com.capgemini.ailabar.commons.utils.DateTime;
import com.capgemini.ailabar.users.domain.exceptions.DeleteUserException;
import com.capgemini.ailabar.users.domain.ports.in.DeleteUserUseCase;
import com.capgemini.ailabar.users.domain.models.UsersModel;
import com.capgemini.ailabar.users.domain.ports.out.UsersRepositoryPort;
import com.capgemini.ailabar.users.infraestructure.entities.UsersEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

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
            throw new DeleteUserException("User name and token are required to delete your account");
        }

        if(Boolean.FALSE.equals(usersRepositoryPort.checkAuthorization(usersModel.getUser(), usersModel.getToken()))) {
            throw new DeleteUserException("Unauthorized user");
        }

        UsersEntity usersEntity = usersRepositoryPort.getUserByName(usersModel.getUser());
        String deactivatedName = usersEntity.getUser() + " **Deactivated** " + usersEntity.getId();
        usersEntity.setUser(deactivatedName);
        usersEntity.setDeactivationDate(DateTime.actualDateAndTime());
        usersRepositoryPort.editUser(usersEntity);

        usersRepositoryPort.disbleGroupsByUserAdmin(usersEntity.getUser(), deactivatedName);

        usersRepositoryPort.deleteMembersByUserId(usersEntity.getId());
    }
}
