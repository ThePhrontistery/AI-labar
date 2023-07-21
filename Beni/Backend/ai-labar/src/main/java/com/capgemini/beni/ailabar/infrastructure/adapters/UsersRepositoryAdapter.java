package com.capgemini.beni.ailabar.infrastructure.adapters;

import com.capgemini.beni.ailabar.domain.repository.UsersRepositoryInterface;
import com.capgemini.beni.ailabar.infrastructure.entity.UsersEntity;
import com.capgemini.beni.ailabar.infrastructure.repository.UsersRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public class UsersRepositoryAdapter implements UsersRepositoryInterface {
    private final UsersRepository usersRepository;

    @Autowired
    public UsersRepositoryAdapter(UsersRepository usersRepository) {
        this.usersRepository = usersRepository;
    }

    @Override
    public Boolean existsByUser(String user) {
        return usersRepository.existsByUser(user);
    }

    @Override
    public Boolean existsByUserAndToken(String user, String token) {
        return usersRepository.existsByUserAndToken(user, token);
    }

    @Override
    public List<String> findUsersByUsernameContaining(String matcher) {
        return usersRepository.findUsersByUsernameContaining(matcher);
    }

    @Override
    public Boolean existsByEmail(String email) {
        return usersRepository.existsByEmail(email);
    }

    @Override
    public UsersEntity findByUser(String user) {
        return usersRepository.findByUser(user);
    }

    @Override
    public void deleteByUser(String user) {
        usersRepository.deleteByUser(user);
    }

    @Override
    public List<String> findAllUsers() {
        return usersRepository.findAllUsers();
    }

    @Override
    public List<String> getEmailsByUserList(List<String> userList) {
        return usersRepository.getEmailsByUserList(userList);
    }

    @Override
    public <S extends UsersEntity> S save(S entity) {
        return usersRepository.save(entity);
    }

    @Override
    public List<UsersEntity> findAll() {
        return usersRepository.findAll();
    }
}
