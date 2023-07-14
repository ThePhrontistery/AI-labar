package com.capgemini.beni.ailabar.service;

import com.capgemini.beni.ailabar.dto.TopicsDto;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.mail.SimpleMailMessage;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(SpringExtension.class)
@SpringBootTest
class MailServiceTest {
    @Mock
    private UsersService usersService;

    @Mock
    private JavaMailSender javaMailSender;

    @InjectMocks
    private MailService mailService;

    @BeforeEach
    void setUp() {
        Mockito.reset(usersService, javaMailSender);
    }

    @Test
    void testSendEmail() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setMembers(Arrays.asList("user1", "user2"));
        topicDto.setAuthor("author");
        topicDto.setTitle("Topic Title");

        List<String> emailList = Arrays.asList("user1@example.com", "user2@example.com");
        when(usersService.getMails(topicDto.getMembers())).thenReturn(emailList);

        ArgumentCaptor<SimpleMailMessage> messageCaptor = ArgumentCaptor.forClass(SimpleMailMessage.class);

//        MailService mailService = new MailService(javaMailSender, usersService);

        mailService.sendEmail(topicDto);

        verify(usersService, times(1)).getMails(topicDto.getMembers());

        verify(javaMailSender, times(2)).send(messageCaptor.capture());

        List<SimpleMailMessage> sentMessages = messageCaptor.getAllValues();

        assertEquals(2, sentMessages.size());

        for (SimpleMailMessage sentMessage : sentMessages) {
            assertEquals("Se ha creado el Topic Topic Title en el que puedes participar", sentMessage.getSubject());
            assertEquals("Me gustaría que formes parte de la votación. \nUn saludo.", sentMessage.getText());
        }
    }
}

