package com.capgemini.beni.ailabar.service;

import com.capgemini.beni.ailabar.entity.UsersEntity;
import com.capgemini.beni.ailabar.repository.UsersRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import javax.transaction.Transactional;
import java.util.List;

@Service
@Transactional
public class UsersService {
    private final UsersRepository usersRepository;

    @Autowired
    public UsersService(UsersRepository usersRepository) {
        this.usersRepository = usersRepository;
    }

    public Boolean checkUser(String user){
        return usersRepository.existsByUser(user);
    }

    public List<String> userMatches(String matcher){
        return usersRepository.findUsersByUsernameContaining(matcher);
    }

    public Boolean checkToken(String user, String token){
        return usersRepository.existsByUserAndToken(user, token);
    }

    public List<String> getMails(List<String> userList) {
        return usersRepository.getEmailsByUserList(userList);
    }

    /* Inicio de métodos sólo para realizar pruebas */
    public void saveUser(UsersEntity userEntity) {
        usersRepository.save(userEntity);
    }

    public void deleteUser(String user) {
        usersRepository.deleteByUser(user);
    }

    public List<UsersEntity> getAllUsersData() {
        return usersRepository.findAll();
    }
    /* Fin métodos sólo para realizar pruebas */

    public Boolean existsByEmail(String email){
        return usersRepository.existsByEmail(email);
    }

    public UsersEntity findByUser(String user) {
        return usersRepository.findByUser(user);
    }
}
