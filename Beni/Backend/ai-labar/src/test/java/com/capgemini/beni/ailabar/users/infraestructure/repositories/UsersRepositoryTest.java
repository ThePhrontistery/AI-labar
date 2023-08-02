package com.capgemini.beni.ailabar.users.infraestructure.repositories;

import com.capgemini.beni.ailabar.users.infraestructure.entities.UsersEntity;
import com.capgemini.beni.ailabar.users.infraestructure.repositories.UsersRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.ArrayList;
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
    void testFindAllUsers_Success() {
        List<String> expectedUsersList = Arrays.asList("user1", "user2", "user3");

        when(usersRepository.findAllUsers()).thenReturn(expectedUsersList);

        List<String> actualUsersList = usersRepository.findAllUsers();

        assertEquals(expectedUsersList, actualUsersList);

        verify(usersRepository, times(1)).findAllUsers();
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

    @Test
    void testSaveUser() {
        UsersEntity userEntity = new UsersEntity();
        userEntity.setUser("john_doe");
        userEntity.setEmail("john@example.com");

        when(usersRepository.save(userEntity)).thenReturn(userEntity);

        UsersEntity savedUser = usersRepository.save(userEntity);

        assertNotNull(savedUser);
        assertEquals("john_doe", savedUser.getUser());
        assertEquals("john@example.com", savedUser.getEmail());

        verify(usersRepository, times(1)).save(userEntity);
    }

    @Test
    void testGetAllUsers() {
        List<UsersEntity> userList = new ArrayList<>();

        UsersEntity user1 = new UsersEntity();
        user1.setUser("User1");

        UsersEntity user2 = new UsersEntity();
        user2.setUser("User2");

        userList.add(user1);
        userList.add(user2);

        when(usersRepository.findAll()).thenReturn(userList);

        List<UsersEntity> allUsers = usersRepository.findAll();

        assertNotNull(allUsers);
        assertEquals(2, allUsers.size());

        assertEquals(userList, allUsers);

        verify(usersRepository, times(1)).findAll();
    }
}
