package com.capgemini.ailabar.commons.utils;

import com.capgemini.ailabar.topics.domain.models.TopicsModel;
import com.capgemini.ailabar.users.application.services.UsersService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mail.SimpleMailMessage;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.stereotype.Service;
import javax.transaction.Transactional;
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

    public void sendEmail(TopicsModel topicDto) {
//        if(topicDto.getMembers().isEmpty()) {
//            throw new NullPointerException("The users to whom the email needs to be sent are required");
//        }
//
//        if(Boolean.FALSE.equals(usersService.checkUser(topicDto.getUser()))) {
//            throw new NullPointerException("The user does not exist");
//        }
//
//
//        List<String> userList = topicDto.getMembers();
//        List<String> emailList = usersService.getMails(userList);
//
//
//        if(emailList.isEmpty()) {
//            throw new NullPointerException("Members not found in the database");
//        }
//
//        emailList.forEach(email -> {
//            SimpleMailMessage message = new SimpleMailMessage();
//            message.setTo(email);
//            message.setSubject("Se ha creado el Topic " + topicDto.getTitle() + " en el que puedes participar");
//            message.setText("Me gustaría que formes parte de la votación. \nUn saludo.");
//
//            javaMailSender.send(message);
//        });
    }
}
