package com.capgemini.beni.ailabar.service;

import com.capgemini.beni.ailabar.repository.UsersRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

@Service
@Transactional
public class MailService {
    private final UsersRepository usersRepository;

    @Autowired
    public MailService(UsersRepository usersRepository) {
        this.usersRepository = usersRepository;
    }

    public void sendEmail(String members) {
        if(members.isBlank()) {
            return;
        }

        String[] usersArray = members.split("[,;]");

        List<String> userList = new ArrayList<>();
        Arrays.stream(usersArray).forEach(user -> userList.add(user.strip()));

        //List<String> emailList = usersRepository.getEmailsByUserList(userList);
        String emailsString = String.join(";", usersRepository.getEmailsByUserList(userList));

        /* Aquí se realizaría el proceso de envío de email */
    }
}
