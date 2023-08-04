package com.capgemini.ailabar.users.application.services;

import com.capgemini.ailabar.users.domain.exceptions.*;
import com.capgemini.ailabar.users.domain.models.UsersModel;
import com.capgemini.ailabar.users.domain.ports.in.*;
import com.capgemini.ailabar.users.infraestructure.entities.UsersEntity;
import org.springframework.stereotype.Service;
import javax.transaction.Transactional;
import java.util.List;

@Service
public class UsersService {
    private final CreateUserUseCase createUserUseCase;
    private final EditUserUseCase editUserUseCase;
    private final DeleteUserUseCase deleteUserUseCase;
    private final GetUsersByMatchUseCase getUsersByMatch;
    private final GetAllUsersUseCase getAllUsersUseCase;
    private final GetUsersDatabaseUseCase getUsersDatabaseUseCase;

    public UsersService(CreateUserUseCase createUserUseCase, EditUserUseCase editUserUseCase,
                        DeleteUserUseCase deleteUserUseCase, GetUsersByMatchUseCase getUsersByMatch,
                        GetAllUsersUseCase getAllUsersUseCase, GetUsersDatabaseUseCase getUsersDatabaseUseCase) {
        this.createUserUseCase = createUserUseCase;
        this.editUserUseCase = editUserUseCase;
        this.deleteUserUseCase = deleteUserUseCase;
        this.getUsersByMatch = getUsersByMatch;
        this.getAllUsersUseCase = getAllUsersUseCase;
        this.getUsersDatabaseUseCase = getUsersDatabaseUseCase;
    }

    public void createUser(UsersModel usersModel) {
        try {
            createUserUseCase.createUser(usersModel);
        } catch (CreateUserException createUserException) {
            throw createUserException;
        }
    }

    public void editUser(UsersModel usersModel) {
        try {
            editUserUseCase.editUser(usersModel);
        } catch (EditUserException editUserException) {
            throw editUserException;
        }
    }

    public void deleteUser(UsersModel usersModel) {
        try {
            deleteUserUseCase.deleteUser(usersModel);
        } catch (DeleteUserException deleteUserException) {
            throw deleteUserException;
        }
    }

    public List<String> getUsersByMatch(UsersModel usersModel) {
        try {
            return this.getUsersByMatch.getUsersByMatch(usersModel);
        } catch (GetUsersByMatchException getUsersException) {
            throw getUsersException;
        }
    }

    public List<String> getAllUsers(UsersModel usersModel) {
        try {
            return getAllUsersUseCase.getAllUsers(usersModel);
        } catch (GetAllUsersException getAllUsersException) {
            throw getAllUsersException;
        }
    }

    public List<UsersEntity> getUsersDatabase() {
        try {
            return this.getUsersDatabaseUseCase.getUsersDatabase();
        } catch (GetUsersDatabaseException getUsersDatabaseException) {
            throw getUsersDatabaseException;
        }
    }

//    public Boolean checkUser(String user){
//        return usersRepositoryPort.existsByUser(user);
//    }
//
//    public List<String> userMatches(String matcher){
//        return usersRepositoryPort.findUsersByUsernameContaining(matcher);
//    }
//
//    public Boolean checkToken(String user, String token){
//        return usersRepositoryPort.existsByUserAndToken(user, token);
//    }
//
//    public List<String> getMails(List<String> userList) {
//        return usersRepositoryPort.getEmailsByUserList(userList);
//    }
//
//    public List<String> getAllUsers() {
//        return usersRepositoryPort.findAllUsers();
//    }
//
//    /* Inicio de métodos sólo para realizar pruebas */
//    public void saveUser(UsersEntity userEntity) {
//        usersRepositoryPort.save(userEntity);
//    }
//
//    public void deleteUser(String user) {
//        usersRepositoryPort.deleteByUser(user);
//    }
//
//    public List<UsersEntity> getAllUsersData() {
//        return usersRepositoryPort.findAll();
//    }
//    /* Fin métodos sólo para realizar pruebas */
//
//    public Boolean existsByEmail(String email){
//        return usersRepositoryPort.existsByEmail(email);
//    }
//
//    public UsersEntity findByUser(String user) {
//        return usersRepositoryPort.findByUser(user);
//    }
//
//    public List<OptionsData> getUsersPhotos(List<OptionsData> optionsDataList) {
//        for (OptionsData optionData : optionsDataList) {
//            String userPhoto = usersRepositoryPort.getUserPhotoByOption(optionData.getOption());
//            if (userPhoto != null && !userPhoto.isEmpty()) {
//                optionData.setImage(userPhoto);
//            }
//        }
//        return optionsDataList;
//    }
}
