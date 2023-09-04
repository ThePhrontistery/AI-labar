package com.capgemini.ailabar.commons.utils;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mail.SimpleMailMessage;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.stereotype.Service;
import javax.transaction.Transactional;
import java.util.List;

@Service
@Transactional
public class MailService {
    private final JavaMailSender javaMailSender;

    @Autowired
    public MailService(JavaMailSender javaMailSender) {
        this.javaMailSender = javaMailSender;
    }

    public void sendEmail(String topicTitle,  List<String> emailList) {
        emailList.forEach(email -> {
            SimpleMailMessage message = new SimpleMailMessage();
            message.setTo(email);
            message.setSubject("Se ha creado el Topic " + topicTitle + " en el que puedes participar");
            message.setText("Me gustaría que formes parte de la votación. \nUn saludo.");

            javaMailSender.send(message);
        });
    }
}
