package com.capgemini.beni.ailabar.service;

import com.capgemini.beni.ailabar.dto.TopicsDto;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mail.SimpleMailMessage;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.stereotype.Service;
import javax.transaction.Transactional;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

@Service
@Transactional
public class MailService {
    private final UsersService usersService;
    private final JavaMailSender javaMailSender;

    @Autowired
    public MailService(UsersService usersService, JavaMailSender javaMailSender) {
        this.usersService = usersService;
        this.javaMailSender = javaMailSender;
    }

    public void sendEmail(TopicsDto topicDto) {
        if(topicDto.getMembers().isBlank()) {
            throw new NullPointerException("The users to whom the email needs to be sent are required");
        }

        if(Boolean.FALSE.equals(usersService.checkUser(topicDto.getAuthor()))) {
            throw new NullPointerException("The user does not exist");
        }

        String[] usersArray = topicDto.getMembers().split("[,;]");

        List<String> userList = new ArrayList<>();
        Arrays.stream(usersArray).forEach(user -> userList.add(user.strip()));

        List<String> emailList = usersService.getMails(userList);

        if(emailList.isEmpty()) {
            throw new NullPointerException("Members not found in the database");
        }

        emailList.forEach(email -> {
            SimpleMailMessage message = new SimpleMailMessage();
            message.setTo(email);
            message.setSubject("Se ha creado el Topic " + topicDto.getTitle() + " en el que puedes participar");
            message.setText("Me gustaría que formes parte de la votación. \nUn saludo.");

            javaMailSender.send(message);
        });
    }
}
