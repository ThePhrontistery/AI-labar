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

    public void createUser(UsersEntity userEntity) {
        usersRepository.save(userEntity);
    }

    public Boolean existsByUser(String user){
        return usersRepository.existsByUser(user);
    }

    public Boolean existsByEmail(String email){
        return usersRepository.existsByEmail(email);
    }

    public UsersEntity findByUser(String user) {
        return usersRepository.findByUser(user);
    }

    public void deleteUser(String user) {
        usersRepository.deleteByUser(user);
    }

    public List<UsersEntity> getAllUsersData() {
        return usersRepository.findAll();
    }

    /* Este método es sólo de prueba, no va en esta clase, si no en MailService */
    public List<String> getEmailsByUserList(List<String> userList) {
        return usersRepository.getEmailsByUserList(userList);
    }
    /* ¡¡BORRAR!! */
}
