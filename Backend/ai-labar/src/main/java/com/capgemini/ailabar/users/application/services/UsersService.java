package com.capgemini.ailabar.users.application.services;

import com.capgemini.ailabar.users.domain.exceptions.*;
import com.capgemini.ailabar.users.domain.models.UsersModel;
import com.capgemini.ailabar.users.domain.ports.in.*;
import com.capgemini.ailabar.users.infraestructure.entities.UsersEntity;
import org.springframework.stereotype.Service;
import java.util.List;

@Service
public class UsersService implements LoginUseCase, CreateUserUseCase, EditUserUseCase, DeleteUserUseCase,
        GetUsersByMatchUseCase, EditVisualizationUseCase, GetAllUsersUseCase, GetUsersDatabaseUseCase,EditLanguageUseCase {
    private final LoginUseCase loginUseCase;
    private final CreateUserUseCase createUserUseCase;
    private final EditUserUseCase editUserUseCase;
    private final DeleteUserUseCase deleteUserUseCase;
    private final GetUsersByMatchUseCase getUsersByMatch;
    private final EditVisualizationUseCase editVisualizationUseCase;
    private final EditLanguageUseCase editLanguageUseCase;
    private final GetAllUsersUseCase getAllUsersUseCase;
    private final GetUsersDatabaseUseCase getUsersDatabaseUseCase;

    public UsersService(LoginUseCase loginUseCase, CreateUserUseCase createUserUseCase,
                        EditUserUseCase editUserUseCase, DeleteUserUseCase deleteUserUseCase,
                        GetUsersByMatchUseCase getUsersByMatch, EditVisualizationUseCase editVisualizationUseCase,
                        GetAllUsersUseCase getAllUsersUseCase, GetUsersDatabaseUseCase getUsersDatabaseUseCase ,
                        EditLanguageUseCase editLanguageUseCase) {
        this.loginUseCase = loginUseCase;
        this.createUserUseCase = createUserUseCase;
        this.editUserUseCase = editUserUseCase;
        this.deleteUserUseCase = deleteUserUseCase;
        this.getUsersByMatch = getUsersByMatch;
        this.editVisualizationUseCase = editVisualizationUseCase;
        this.editLanguageUseCase = editLanguageUseCase;
        this.getAllUsersUseCase = getAllUsersUseCase;
        this.getUsersDatabaseUseCase = getUsersDatabaseUseCase;
    }
    @Override
    public List<String> login(UsersModel usersModel) {
        try {
            return loginUseCase.login(usersModel);
        } catch (LoginException loginException) {
            throw loginException;
        }
    }

    @Override
    public void createUser(UsersModel usersModel) {
        try {
            createUserUseCase.createUser(usersModel);
        } catch (CreateUserException createUserException) {
            throw createUserException;
        }
    }

    @Override
    public void editUser(UsersModel usersModel) {
        try {
            editUserUseCase.editUser(usersModel);
        } catch (EditUserException editUserException) {
            throw editUserException;
        }
    }

    @Override
    public void editVisualization(UsersModel usersModel) {
        try {
            editVisualizationUseCase.editVisualization(usersModel);
        } catch (EditVisualizationException editVisualizationException) {
            throw editVisualizationException;
        }
    }

    @Override
    public void editLanguage(UsersModel usersModel) {
        try {
            editLanguageUseCase.editLanguage(usersModel);
        } catch (EditLanguageException editLanguageException) {
            throw editLanguageException;
        }
    }

    @Override
    public void deleteUser(UsersModel usersModel) {
        try {
            deleteUserUseCase.deleteUser(usersModel);
        } catch (DeleteUserException deleteUserException) {
            throw deleteUserException;
        }
    }

    @Override
    public List<String> getUsersByMatch(UsersModel usersModel) {
        try {
            return this.getUsersByMatch.getUsersByMatch(usersModel);
        } catch (GetUsersByMatchException getUsersException) {
            throw getUsersException;
        }
    }

    @Override
    public List<String> getAllUsers(UsersModel usersModel) {
        try {
            return getAllUsersUseCase.getAllUsers(usersModel);
        } catch (GetAllUsersException getAllUsersException) {
            throw getAllUsersException;
        }
    }

    @Override
    public List<UsersEntity> getUsersDatabase() {
        try {
            return this.getUsersDatabaseUseCase.getUsersDatabase();
        } catch (GetUsersDatabaseException getUsersDatabaseException) {
            throw getUsersDatabaseException;
        }
    }
}
