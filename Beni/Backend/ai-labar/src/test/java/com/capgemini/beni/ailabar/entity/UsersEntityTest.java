package com.capgemini.beni.ailabar.entity;

import com.capgemini.beni.ailabar.dto.UsersDto;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.context.SpringBootTest;

@ExtendWith(MockitoExtension.class)
@SpringBootTest
class UsersEntityTest {
    @Test
    void testId() {
        Integer id = 1;
        UsersEntity usersEntity = new UsersEntity();
        usersEntity.setId(id);
        Assertions.assertEquals(id, usersEntity.getId());
    }

    @Test
    void testUser() {
        String user = "testuser";
        UsersEntity usersEntity = new UsersEntity();
        usersEntity.setUser(user);
        Assertions.assertEquals(user, usersEntity.getUser());
    }

    @Test
    void testPassword() {
        String password = "testpassword";
        UsersEntity usersEntity = new UsersEntity();
        usersEntity.setPassword(password);
        Assertions.assertEquals(password, usersEntity.getPassword());
    }

    @Test
    void testEmail() {
        String email = "test@example.com";
        UsersEntity usersEntity = new UsersEntity();
        usersEntity.setEmail(email);
        Assertions.assertEquals(email, usersEntity.getEmail());
    }

    @Test
    void testConstructor() {
        UsersDto usersDto = Mockito.mock(UsersDto.class);
        String user = "testuser";
        String password = "testpassword";
        String email = "test@example.com";
        Mockito.when(usersDto.getUser()).thenReturn(user);
        Mockito.when(usersDto.getPassword()).thenReturn(password);
        Mockito.when(usersDto.getEmail()).thenReturn(email);

        UsersEntity usersEntity = new UsersEntity(usersDto);

        Assertions.assertEquals(user, usersEntity.getUser());
        Assertions.assertEquals(password, usersEntity.getPassword());
        Assertions.assertEquals(email, usersEntity.getEmail());
    }
}
