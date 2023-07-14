package com.capgemini.beni.ailabar.entity;

import com.capgemini.beni.ailabar.dto.UsersDto;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import static org.junit.jupiter.api.Assertions.assertEquals;

@ExtendWith(SpringExtension.class)
@SpringBootTest
class UsersEntityTest {
    @Test
    void testIdProperty() {
        UsersEntity entity = new UsersEntity();
        entity.setId(1);
        assertEquals(1, entity.getId());
    }

    @Test
    void testUserProperty() {
        UsersEntity entity = new UsersEntity();
        entity.setUser("user1");
        assertEquals("user1", entity.getUser());
    }

    @Test
    void testPasswordProperty() {
        UsersEntity entity = new UsersEntity();
        entity.setPassword("password");
        assertEquals("password", entity.getPassword());
    }

    @Test
    void testEmailProperty() {
        UsersEntity entity = new UsersEntity();
        entity.setEmail("user1@example.com");
        assertEquals("user1@example.com", entity.getEmail());
    }

    @Test
    void testTokenProperty() {
        UsersEntity entity = new UsersEntity();
        entity.setToken("token123");
        assertEquals("token123", entity.getToken());
    }

    @Test
    void testConstructorWithUsersDto() {
        UsersDto dto = new UsersDto();
        dto.setUser("user1");
        dto.setPassword("password");
        dto.setEmail("user1@example.com");

        UsersEntity entity = new UsersEntity(dto);

        assertEquals("user1", entity.getUser());
        assertEquals("password", entity.getPassword());
        assertEquals("user1@example.com", entity.getEmail());
    }
}
