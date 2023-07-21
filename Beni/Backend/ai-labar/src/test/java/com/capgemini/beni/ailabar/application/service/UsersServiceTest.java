package com.capgemini.beni.ailabar.application.service;

import com.capgemini.beni.ailabar.domain.repository.UsersRepositoryInterface;
import com.capgemini.beni.ailabar.infrastructure.entity.UsersEntity;
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
    private UsersRepositoryInterface usersRepository;

    @InjectMocks
    private UsersService usersService;

    @BeforeEach
    void setUp() {
        Mockito.reset(usersRepository);
    }

    @Test
    void testCheckUser() {
        String user = "john";

        when(usersRepository.existsByUser(user)).thenReturn(true);

        boolean result = usersService.checkUser(user);

        assertTrue(result);
        verify(usersRepository, times(1)).existsByUser(user);
        verifyNoMoreInteractions(usersRepository);
    }

    @Test
    void testUserMatches() {
        String matcher = "jo";
        List<String> expectedUsers = Arrays.asList("john", "joe");

        when(usersRepository.findUsersByUsernameContaining(matcher)).thenReturn(expectedUsers);

        List<String> result = usersService.userMatches(matcher);

        assertEquals(expectedUsers, result);
        verify(usersRepository, times(1)).findUsersByUsernameContaining(matcher);
        verifyNoMoreInteractions(usersRepository);
    }

    @Test
    void testCheckToken() {
        String user = "john";
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
        List<String> userList = Arrays.asList("john", "emma");
        List<String> expectedEmails = Arrays.asList("john@example.com", "emma@example.com");

        when(usersRepository.getEmailsByUserList(userList)).thenReturn(expectedEmails);

        List<String> result = usersService.getMails(userList);

        assertEquals(expectedEmails, result);
        verify(usersRepository, times(1)).getEmailsByUserList(userList);
        verifyNoMoreInteractions(usersRepository);
    }

    @Test
    void testExistsByEmail() {
        String email = "john@example.com";

        when(usersRepository.existsByEmail(email)).thenReturn(true);

        boolean result = usersService.existsByEmail(email);

        assertTrue(result);
        verify(usersRepository, times(1)).existsByEmail(email);
        verifyNoMoreInteractions(usersRepository);
    }

    @Test
    void testFindByUser() {
        String user = "john";
        UsersEntity expectedEntity = new UsersEntity();

        when(usersRepository.findByUser(user)).thenReturn(expectedEntity);

        UsersEntity result = usersService.findByUser(user);

        assertEquals(expectedEntity, result);
        verify(usersRepository, times(1)).findByUser(user);
        verifyNoMoreInteractions(usersRepository);
    }

    /* Los siguientes casos de uso son específicos para pruebas internas */

    @Test
    void testSaveUser() {
        UsersEntity userEntity = new UsersEntity();

        usersService.saveUser(userEntity);

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
    void testGetAllUsers() {
        List<String> expectedUsersList = Arrays.asList("user1", "user2");

        when(usersRepository.findAllUsers()).thenReturn(expectedUsersList);

        List<String> actualUsersList = usersService.getAllUsers();

        assertEquals(expectedUsersList, actualUsersList);

        verify(usersRepository, times(1)).findAllUsers();
        verifyNoMoreInteractions(usersRepository);
    }


    @Test
    void testGetAllUsersData() {
        UsersEntity user1 = new UsersEntity();
        UsersEntity user2 = new UsersEntity();
        List<UsersEntity> expectedUsers = Arrays.asList(user1, user2);

        when(usersRepository.findAll()).thenReturn(expectedUsers);

        List<UsersEntity> result = usersService.getAllUsersData();

        assertEquals(expectedUsers, result);
        verify(usersRepository, times(1)).findAll();
        verifyNoMoreInteractions(usersRepository);
    }
}
