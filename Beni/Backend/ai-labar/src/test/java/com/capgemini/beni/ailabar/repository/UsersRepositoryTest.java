package com.capgemini.beni.ailabar.repository;

import com.capgemini.beni.ailabar.entity.UsersEntity;
import com.capgemini.beni.ailabar.service.UsersService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
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
class UsersRepositoryTest {
    @Mock
    private UsersRepository usersRepository;

    @BeforeEach
    void setUp() {
        Mockito.reset(usersRepository);
    }

    @Test
    void testExistsByUser() {
        String user = "john";

        when(usersRepository.existsByUser(user)).thenReturn(true);

        boolean result = usersRepository.existsByUser(user);

        assertTrue(result);
        verify(usersRepository, times(1)).existsByUser(user);
        verifyNoMoreInteractions(usersRepository);
    }

    @Test
    void testExistsByUserAndToken() {
        String user = "exampleUser";
        String token = "exampleToken";
        boolean expected = true;

        when(usersRepository.existsByUserAndToken(user, token)).thenReturn(expected);

        boolean actual = usersRepository.existsByUserAndToken(user, token);

        assertEquals(expected, actual);
        verify(usersRepository, times(1)).existsByUserAndToken(user, token);
        verifyNoMoreInteractions(usersRepository);
    }

    @Test
    void testExistsByEmail() {
        String email = "john@example.com";

        when(usersRepository.existsByEmail(email)).thenReturn(true);

        boolean result = usersRepository.existsByEmail(email);

        assertTrue(result);
        verify(usersRepository, times(1)).existsByEmail(email);
        verifyNoMoreInteractions(usersRepository);
    }

    @Test
    void testFindByUser() {
        String user = "john";
        UsersEntity expectedEntity = new UsersEntity();

        when(usersRepository.findByUser(user)).thenReturn(expectedEntity);

        UsersEntity result = usersRepository.findByUser(user);

        assertEquals(expectedEntity, result);
        verify(usersRepository, times(1)).findByUser(user);
        verifyNoMoreInteractions(usersRepository);
    }

    @Test
    void testDeleteByUser() {
        String user = "john";

        usersRepository.deleteByUser(user);

        verify(usersRepository, times(1)).deleteByUser(user);
        verifyNoMoreInteractions(usersRepository);
    }

    @Test
    void testGetEmailsByUserList() {
        List<String> userList = Arrays.asList("john", "emma");
        List<String> expectedEmails = Arrays.asList("john@example.com", "emma@example.com");

        when(usersRepository.getEmailsByUserList(userList)).thenReturn(expectedEmails);

        List<String> result = usersRepository.getEmailsByUserList(userList);

        assertEquals(expectedEmails, result);
        verify(usersRepository, times(1)).getEmailsByUserList(userList);
        verifyNoMoreInteractions(usersRepository);
    }

}

