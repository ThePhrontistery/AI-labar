package com.capgemini.ailabar.users.application.usecases;

import com.capgemini.ailabar.users.domain.exceptions.CreateUserException;
import com.capgemini.ailabar.users.domain.models.UsersModel;
import com.capgemini.ailabar.users.domain.ports.in.CreateUserUseCase;
import com.capgemini.ailabar.users.domain.ports.out.UsersRepositoryPort;
import com.capgemini.ailabar.users.infraestructure.entities.UsersEntity;
import org.apache.commons.codec.digest.DigestUtils;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;

@Service
@Transactional
public class CreateUserUseCaseImpl implements CreateUserUseCase {
    private final UsersRepositoryPort usersRepositoryPort;

    public CreateUserUseCaseImpl(UsersRepositoryPort usersRepositoryPort) {
        this.usersRepositoryPort = usersRepositoryPort;
    }

    @Override
    public void createUser(UsersModel usersModel) {
        if (usersModel.getUser().isBlank() || usersModel.getPassword().isBlank()
                || usersModel.getEmail().isBlank()) {
            throw new CreateUserException("All data is required to create a new user");
        }

        if (Boolean.TRUE.equals(usersRepositoryPort.checkUser(usersModel.getUser()))) {
            throw new CreateUserException("The user already existsThe user already exists");
        }

        if (Boolean.TRUE.equals(usersRepositoryPort.checkEmail(usersModel.getEmail()))) {
            throw new CreateUserException("The email already exists");
        }

        if ((usersModel.getGender() != null && !usersModel.getUser().isBlank())
                && (!usersModel.getGender().equals("H") && !usersModel.getGender().equals("M"))) {
            throw new CreateUserException("The gender must be equal to 'H' or 'M'");
        }

        String hashedPassword = DigestUtils.sha256Hex(usersModel.getPassword());

        UsersEntity usersEntity = new UsersEntity(usersModel);
        usersEntity.setPassword(hashedPassword);
        usersEntity.setToken("");
        usersRepositoryPort.createUser(usersEntity);

        usersEntity = usersRepositoryPort.getUserByName(usersModel.getUser());

        if(usersEntity == null) {
            throw new CreateUserException("User not found");
        }

        String token = DigestUtils.sha256Hex(usersModel.getUser()+hashedPassword+usersEntity.getId());
        usersEntity.setToken(token);

        usersRepositoryPort.createUser(usersEntity);
    }
}
