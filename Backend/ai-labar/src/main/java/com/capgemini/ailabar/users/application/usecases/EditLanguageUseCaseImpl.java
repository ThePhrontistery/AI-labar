package com.capgemini.ailabar.users.application.usecases;

import com.capgemini.ailabar.commons.utils.DateTime;
import com.capgemini.ailabar.users.domain.exceptions.EditLanguageException;
import com.capgemini.ailabar.users.domain.models.UsersModel;
import com.capgemini.ailabar.users.domain.ports.in.EditLanguageUseCase;
import com.capgemini.ailabar.users.domain.ports.out.UsersRepositoryPort;
import com.capgemini.ailabar.users.infraestructure.entities.UsersEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional
public class EditLanguageUseCaseImpl implements EditLanguageUseCase {
    private final UsersRepositoryPort usersRepositoryPort;

    public EditLanguageUseCaseImpl(UsersRepositoryPort usersRepositoryPort) {
        this.usersRepositoryPort = usersRepositoryPort;
    }

    @Override
    public void editLanguage(UsersModel usersModel) {
        if(usersModel.getUser().isBlank() || usersModel.getToken().isBlank() || usersModel.getLanguage().isBlank()) {
            throw new EditLanguageException("User name, language and token are required");
        }

        if(Boolean.FALSE.equals(usersRepositoryPort.checkAuthorization(usersModel.getUser(), usersModel.getToken()))) {
            throw new EditLanguageException("Unauthorized user");
        }

        UsersEntity userEntity = usersRepositoryPort.getUserByName(usersModel.getUser());

        userEntity.setLanguage(usersModel.getLanguage());
        userEntity.setLastModificationDate(DateTime.actualDateAndTime());

        usersRepositoryPort.editUser(userEntity);
    }
}
