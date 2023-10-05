package com.capgemini.ailabar.users.infraestructure.adapters.out;

import com.capgemini.ailabar.users.domain.ports.out.UsersRepositoryPort;
import com.capgemini.ailabar.users.infraestructure.entities.UsersEntity;
import com.capgemini.ailabar.users.infraestructure.repositories.UsersRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public class UsersRepositoryAdapter implements UsersRepositoryPort {
    private final UsersRepository usersRepository;

    public UsersRepositoryAdapter(UsersRepository usersRepository) {
        this.usersRepository = usersRepository;
    }

    @Override
    public boolean checkAuthorization(String user, String token) {
        return usersRepository.checkAuthorization(user, token);
    }

    @Override
    public boolean checkEmail(String email) {
        return usersRepository.checkEmail(email);
    }

    @Override
    public boolean checkUser(String user) {
        return usersRepository.checkUser(user);
    }

    @Override
    public void createUser(UsersEntity usersEntity) {
        usersRepository.save(usersEntity);
    }

    @Override
    public void editUser(UsersEntity usersEntity) {
        usersRepository.save(usersEntity);
    }

    @Override
    public List<String> getAllUsers() {
        return usersRepository.getAllUsers();
    }

    @Override
    public UsersEntity getUserByEmail(String email) {
        return usersRepository.getUserByEmail(email);
    }

    @Override
    public UsersEntity getUserByName(String user) {
        return usersRepository.getUserByName(user);
    }

    @Override
    public List<String> getUsersByMatch(String matcher) {
        return usersRepository.getUsersByNameMatch(matcher.toUpperCase());
    }

    @Override
    public List<UsersEntity> getUsersDatabase() {
        return usersRepository.findAll();
    }

    @Override
    public void disbleGroupsByUserAdmin(String admin, String deactivatedName) {
        usersRepository.disbleGroupsByUserAdmin(admin, deactivatedName);
    }

    @Override
    public void deleteMembersByUserId(Integer userId) {
        usersRepository.deleteMembersByUserId(userId);
    }

    @Override
    public void updateUserNameAndToken(Integer userId, String user, String newToken) {
        usersRepository.updateUserNameAndToken(userId, user, newToken);
    }

    @Override
    public boolean login(String user, String password) {
        return usersRepository.checkLogin(user, password);
    }
}
