package com.capgemini.ailabar.application.service;

import com.capgemini.ailabar.domain.model.TopicsModel;
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
import java.util.Collections;
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
    void testSendEmail_EmptyMembers() {
        TopicsModel topicDto = new TopicsModel();
        topicDto.setMembers(Collections.emptyList());

        assertThrows(NullPointerException.class, () -> mailService.sendEmail(topicDto));
    }

    @Test
    void testSendEmail_UserNotFound() {
        TopicsModel topicDto = new TopicsModel();
        topicDto.setMembers(Collections.singletonList("user@example.com"));
        topicDto.setUser("nonExistentUser");

        when(usersService.checkUser(topicDto.getUser())).thenReturn(false);

        assertThrows(NullPointerException.class, () -> mailService.sendEmail(topicDto));

        verify(usersService, times(1)).checkUser(topicDto.getUser());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testSendEmail_EmailsNotFound() {
        TopicsModel topicDto = new TopicsModel();
        topicDto.setMembers(Collections.singletonList("user@example.com"));
        topicDto.setUser("existingUser");

        when(usersService.checkUser(topicDto.getUser())).thenReturn(true);
        when(usersService.getMails(topicDto.getMembers())).thenReturn(Collections.emptyList());

        assertThrows(NullPointerException.class, () -> mailService.sendEmail(topicDto));

        verify(usersService, times(1)).checkUser(topicDto.getUser());
        verify(usersService, times(1)).getMails(topicDto.getMembers());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testSendEmail_Successful() {
        TopicsModel topicDto = new TopicsModel();
        topicDto.setMembers(Arrays.asList("user1", "user2"));
        topicDto.setUser("user");
        topicDto.setTitle("Topic Title");

        List<String> emailList = Arrays.asList("user1@example.com", "user2@example.com");

        when(usersService.getMails(topicDto.getMembers())).thenReturn(emailList);
        when(usersService.checkUser(topicDto.getUser())).thenReturn(true);

        ArgumentCaptor<SimpleMailMessage> messageCaptor = ArgumentCaptor.forClass(SimpleMailMessage.class);

        MailService mailService = new MailService(usersService, javaMailSender);

        mailService.sendEmail(topicDto);

        verify(usersService, times(1)).getMails(topicDto.getMembers());
        verify(usersService, times(1)).checkUser(topicDto.getUser());
        verify(javaMailSender, times(2)).send(messageCaptor.capture());

        List<SimpleMailMessage> sentMessages = messageCaptor.getAllValues();

        assertEquals(2, sentMessages.size());

        for (SimpleMailMessage sentMessage : sentMessages) {
            assertEquals("Se ha creado el Topic Topic Title en el que puedes participar", sentMessage.getSubject());
            assertEquals("Me gustaría que formes parte de la votación. \nUn saludo.", sentMessage.getText());
        }
    }
}

