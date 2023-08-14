package com.capgemini.ailabar.commons.utils;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.mail.SimpleMailMessage;
import org.springframework.mail.javamail.JavaMailSender;

import java.util.Arrays;
import java.util.List;

import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class MailServiceTest {

    @Mock
    private JavaMailSender javaMailSender;

    @InjectMocks
    private MailService mailService;

    @Test
    void testSendEmail() {
        String topicTitle = "Test Topic";
        List<String> emailList = Arrays.asList("user1@example.com", "user2@example.com");

        mailService.sendEmail(topicTitle, emailList);

        verify(javaMailSender, times(2)).send(any(SimpleMailMessage.class));
    }
}
