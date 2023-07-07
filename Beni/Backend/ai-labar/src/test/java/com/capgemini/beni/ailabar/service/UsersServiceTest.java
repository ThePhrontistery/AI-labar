package com.capgemini.beni.ailabar.service;

import com.capgemini.beni.ailabar.entity.UsersEntity;
import com.capgemini.beni.ailabar.repository.UsersRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(SpringExtension.class)
@SpringBootTest
class UsersServiceTest {
    @Mock
    private UsersRepository usersRepository;

    @InjectMocks
    private UsersService usersService;

    @BeforeEach
    void setUp() {
        Mockito.reset(usersRepository);
    }

    @Test
    void testCheckUser() {
        String user = "john";
        boolean expectedResult = true;

        when(usersRepository.existsByUser(user)).thenReturn(expectedResult);

        boolean result = usersService.checkUser(user);

        assertEquals(expectedResult, result);
        verify(usersRepository, times(1)).existsByUser(user);
        verifyNoMoreInteractions(usersRepository);
    }

    @Test
    void testCheckToken() {
        String user = "exampleUser";
        String token = "exampleToken";
        boolean expected = true;

        when(usersRepository.existsByUserAndToken(user, token)).thenReturn(expected);

        boolean actual = usersService.checkToken(user, token);

        assertEquals(expected, actual);
        verify(usersRepository, times(1)).existsByUserAndToken(user, token);
        verifyNoMoreInteractions(usersRepository);
    }

    @Test
    void testGetMails() {
        List<String> userList = Arrays.asList("john", "jane");
        List<String> expectedEmails = Arrays.asList("john@example.com", "jane@example.com");

        when(usersRepository.getEmailsByUserList(userList)).thenReturn(expectedEmails);

        List<String> result = usersService.getMails(userList);

        assertEquals(expectedEmails, result);
        verify(usersRepository, times(1)).getEmailsByUserList(userList);
        verifyNoMoreInteractions(usersRepository);
    }

    @Test
    void testSaveUser() {
        UsersEntity userEntity = new UsersEntity();

        usersService.saveUser(userEntity);

        // Assert
        verify(usersRepository, times(1)).save(userEntity);
        verifyNoMoreInteractions(usersRepository);
    }

    @Test
    void testDeleteUser() {
        String user = "john";

        usersService.deleteUser(user);

        verify(usersRepository, times(1)).deleteByUser(user);
        verifyNoMoreInteractions(usersRepository);
    }

    @Test
    void testGetAllUsersData() {
        List<UsersEntity> expectedUsers = Arrays.asList(new UsersEntity(), new UsersEntity());

        when(usersRepository.findAll()).thenReturn(expectedUsers);

        List<UsersEntity> result = usersService.getAllUsersData();

        assertEquals(expectedUsers, result);
        verify(usersRepository, times(1)).findAll();
        verifyNoMoreInteractions(usersRepository);
    }

    @Test
    void testExistsByEmail() {
        String email = "john@example.com";
        boolean expectedResult = true;

        when(usersRepository.existsByEmail(email)).thenReturn(expectedResult);

        boolean result = usersService.existsByEmail(email);

        assertEquals(expectedResult, result);
        verify(usersRepository, times(1)).existsByEmail(email);
        verifyNoMoreInteractions(usersRepository);
    }

    @Test
    void testFindByUser() {
        String user = "john";
        UsersEntity expectedUser = new UsersEntity();

        when(usersRepository.findByUser(user)).thenReturn(expectedUser);

        UsersEntity result = usersService.findByUser(user);

        assertEquals(expectedUser, result);
        verify(usersRepository, times(1)).findByUser(user);
        verifyNoMoreInteractions(usersRepository);
    }
}

