package com.capgemini.ailabar.users.application.usecases;

import com.capgemini.ailabar.users.domain.exceptions.EditUserException;
import com.capgemini.ailabar.users.domain.ports.in.EditUserUseCase;
import com.capgemini.ailabar.users.domain.models.UsersModel;
import com.capgemini.ailabar.users.domain.ports.out.UsersRepositoryPort;
import com.capgemini.ailabar.users.infraestructure.entities.UsersEntity;
import org.apache.commons.codec.digest.DigestUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional
public class EditUserUseCaseImpl implements EditUserUseCase {
    private final UsersRepositoryPort usersRepositoryPort;

    public EditUserUseCaseImpl(UsersRepositoryPort usersRepositoryPort) {
        this.usersRepositoryPort = usersRepositoryPort;
    }

    @Override
    public void editUser(UsersModel usersModel) {
        if(usersModel.getUser().isBlank() || usersModel.getPassword().isBlank()
                || usersModel.getToken().isBlank()) {
            throw new EditUserException("All data is required to edit a user");
        }

        if(Boolean.FALSE.equals(usersRepositoryPort.checkAuthorization(usersModel.getUser(), usersModel.getToken()))) {
            throw new EditUserException("Unauthorized user");
        }

        if((usersModel.getNewUser() == null || usersModel.getNewUser().isBlank())
                && (usersModel.getNewPassword() == null || usersModel.getNewPassword().isBlank())) {
            throw new EditUserException("There are no values to update");
        }

        if(Boolean.TRUE.equals(usersRepositoryPort.checkUser(usersModel.getNewUser().strip()))) {
            throw new EditUserException("The new username already exists");
        }

        UsersEntity userEntity = usersRepositoryPort.getUserByName(usersModel.getUser());

        if(usersModel.getNewUser() != null && !usersModel.getNewUser().isBlank()) {
            userEntity.setUser(usersModel.getNewUser().strip());
        }

        if(usersModel.getNewPassword() != null && !usersModel.getNewPassword().isBlank()) {
            String hashedPassword = DigestUtils.sha256Hex(usersModel.getNewPassword());
            userEntity.setPassword(hashedPassword);
        }

        if(usersModel.getGender() != null && !usersModel.getGender().isBlank()) {
            userEntity.setGender(usersModel.getGender());
        }

        if(usersModel.getPhoto() != null && !usersModel.getPhoto().isBlank()) {
            userEntity.setPhoto(usersModel.getPhoto());
        }

        String token = DigestUtils.sha256Hex(userEntity.getUser()+userEntity.getPassword()+userEntity.getId());
        userEntity.setToken(token);

        usersRepositoryPort.editUser(userEntity);
    }
}
