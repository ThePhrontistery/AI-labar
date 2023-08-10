package com.capgemini.ailabar.users.domain.ports.out;

import com.capgemini.ailabar.users.infraestructure.entities.UsersEntity;
import com.capgemini.ailabar.users.infraestructure.repositories.UsersRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.*;

@ExtendWith(SpringExtension.class)
@SpringBootTest
class UsersRepositoryPortTest {
    @Mock
    private UsersRepository usersRepository;

    @BeforeEach
    void setUp() {
        Mockito.reset(usersRepository);
    }

//    @Test
//    void testExistsByUser() {
//        String user = "exampleUser";
//        when(usersRepository.existsByUser(user)).thenReturn(true);
//
//        boolean result = usersRepository.existsByUser(user);
//
//        assertTrue(result);
//        verify(usersRepository, times(1)).existsByUser(user);
//    }
//
//    @Test
//    void testExistsByUserAndToken() {
//        String user = "exampleUser";
//        String token = "validToken";
//        when(usersRepository.existsByUserAndToken(user, token)).thenReturn(true);
//
//        boolean result = usersRepository.existsByUserAndToken(user, token);
//
//        assertTrue(result);
//        verify(usersRepository, times(1)).existsByUserAndToken(user, token);
//    }
//
//    @Test
//    void testFindUsersByUsernameContaining() {
//        String matcher = "exampleMatcher";
//        List<String> userList = Arrays.asList("user1", "user2");
//        when(usersRepository.findUsersByUsernameContaining(matcher)).thenReturn(userList);
//
//        List<String> result = usersRepository.findUsersByUsernameContaining(matcher);
//
//        assertEquals(userList, result);
//        verify(usersRepository, times(1)).findUsersByUsernameContaining(matcher);
//    }
//
//    @Test
//    void testExistsByEmail() {
//        String email = "example@example.com";
//        when(usersRepository.existsByEmail(email)).thenReturn(true);
//
//        boolean result = usersRepository.existsByEmail(email);
//
//        assertTrue(result);
//        verify(usersRepository, times(1)).existsByEmail(email);
//    }
//
//    @Test
//    void testFindByUser() {
//        String user = "exampleUser";
//        UsersEntity userEntity = new UsersEntity();
//        userEntity.setId(1);
//        userEntity.setUser(user);
//        when(usersRepository.findByUser(user)).thenReturn(userEntity);
//
//        UsersEntity result = usersRepository.findByUser(user);
//
//        assertEquals(userEntity, result);
//        verify(usersRepository, times(1)).findByUser(user);
//    }
//
//    @Test
//    void testDeleteByUser() {
//        String user = "exampleUser";
//
//        usersRepository.deleteByUser(user);
//
//        verify(usersRepository, times(1)).deleteByUser(user);
//    }
//
//    @Test
//    void testFindAllUsers() {
//        List<String> userList = Arrays.asList("user1", "user2");
//        when(usersRepository.findAllUsers()).thenReturn(userList);
//
//        List<String> result = usersRepository.findAllUsers();
//
//        assertEquals(userList, result);
//        verify(usersRepository, times(1)).findAllUsers();
//    }
//
//    @Test
//    void testGetEmailsByUserList() {
//        List<String> userList = Arrays.asList("user1", "user2");
//        List<String> emailList = Arrays.asList("email1@example.com", "email2@example.com");
//        when(usersRepository.getEmailsByUserList(userList)).thenReturn(emailList);
//
//        List<String> result = usersRepository.getEmailsByUserList(userList);
//
//        assertEquals(emailList, result);
//        verify(usersRepository, times(1)).getEmailsByUserList(userList);
//    }
//
//    @Test
//    void testSave() {
//        UsersEntity userEntity = new UsersEntity();
//        userEntity.setId(1);
//        userEntity.setUser("exampleUser");
//        when(usersRepository.save(userEntity)).thenReturn(userEntity);
//
//        UsersEntity result = usersRepository.save(userEntity);
//
//        assertEquals(userEntity, result);
//        verify(usersRepository, times(1)).save(userEntity);
//    }
//
//    @Test
//    void testFindAll() {
//        List<UsersEntity> userList = Arrays.asList(new UsersEntity(), new UsersEntity());
//        when(usersRepository.findAll()).thenReturn(userList);
//
//        List<UsersEntity> result = usersRepository.findAll();
//
//        assertEquals(userList, result);
//        verify(usersRepository, times(1)).findAll();
//    }
}
