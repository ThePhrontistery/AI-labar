package com.capgemini.beni.ailabar.service;

import com.capgemini.beni.ailabar.dto.TopicsDto;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
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
        topicDto.setAuthor("john");
        topicDto.setMembers("jane, alex");

        List<String> userList = Arrays.asList("jane", "alex");
        List<String> emailList = Arrays.asList("jane@example.com", "alex@example.com");

        when(usersService.checkUser(topicDto.getAuthor())).thenReturn(true);
        when(usersService.getMails(userList)).thenReturn(emailList);

        mailService.sendEmail(topicDto);

        verify(usersService, times(1)).checkUser(topicDto.getAuthor());
        verify(usersService, times(1)).getMails(userList);
        verify(javaMailSender, times(emailList.size())).send(any(SimpleMailMessage.class));
        verifyNoMoreInteractions(usersService, javaMailSender);
    }

    @Test
    void testSendEmail_BlankMembers() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setAuthor("john");
        topicDto.setMembers("");

        NullPointerException exception = assertThrows(NullPointerException.class, () -> mailService.sendEmail(topicDto));
        assertEquals("The users to whom the email needs to be sent are required", exception.getMessage());
        verifyNoInteractions(usersService, javaMailSender);
    }

    @Test
    void testSendEmail_UserNotExist() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setAuthor("john");
        topicDto.setMembers("jane, alex");

        when(usersService.checkUser(topicDto.getAuthor())).thenReturn(false);

        NullPointerException exception = assertThrows(NullPointerException.class, () -> mailService.sendEmail(topicDto));
        assertEquals("The user does not exist", exception.getMessage());
        verify(usersService, times(1)).checkUser(topicDto.getAuthor());
        verifyNoInteractions(javaMailSender);
    }

    @Test
    void testSendEmail_MembersNotFound() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setAuthor("john");
        topicDto.setMembers("jane, alex");

        List<String> userList = Arrays.asList("jane", "alex");

        when(usersService.checkUser(topicDto.getAuthor())).thenReturn(true);
        when(usersService.getMails(userList)).thenReturn(Arrays.asList());

        NullPointerException exception = assertThrows(NullPointerException.class, () -> mailService.sendEmail(topicDto));
        assertEquals("Members not found in the database", exception.getMessage());
        verify(usersService, times(1)).checkUser(topicDto.getAuthor());
        verify(usersService, times(1)).getMails(userList);
        verifyNoInteractions(javaMailSender);
    }
}

