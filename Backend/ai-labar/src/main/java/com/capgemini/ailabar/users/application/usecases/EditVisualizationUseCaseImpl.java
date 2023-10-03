package com.capgemini.ailabar.users.application.usecases;

import com.capgemini.ailabar.commons.utils.DateTime;
import com.capgemini.ailabar.users.domain.exceptions.EditVisualizationException;
import com.capgemini.ailabar.users.domain.models.UsersModel;
import com.capgemini.ailabar.users.domain.ports.in.EditVisualizationUseCase;
import com.capgemini.ailabar.users.domain.ports.out.UsersRepositoryPort;
import com.capgemini.ailabar.users.infraestructure.entities.UsersEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional
public class EditVisualizationUseCaseImpl implements EditVisualizationUseCase {
    private final UsersRepositoryPort usersRepositoryPort;

    public EditVisualizationUseCaseImpl(UsersRepositoryPort usersRepositoryPort) {
        this.usersRepositoryPort = usersRepositoryPort;
    }

    @Override
    public void editVisualization(UsersModel usersModel) {
        if(usersModel.getUser().isBlank() || usersModel.getToken().isBlank() || usersModel.getVisualization().isBlank()) {
            throw new EditVisualizationException("User name, visualization and token are required");
        }

        if(Boolean.FALSE.equals(usersRepositoryPort.checkAuthorization(usersModel.getUser(), usersModel.getToken()))) {
            throw new EditVisualizationException("Unauthorized user");
        }

        UsersEntity userEntity = usersRepositoryPort.getUserByName(usersModel.getUser());

        userEntity.setVisualization(usersModel.getVisualization());
        userEntity.setLastModificationDate(DateTime.actualDateAndTime());

        usersRepositoryPort.editUser(userEntity);
    }
}
