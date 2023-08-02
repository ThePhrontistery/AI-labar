package com.capgemini.beni.ailabar.users.infraestructure.entities;

import com.capgemini.beni.ailabar.users.domain.models.UsersModel;
import com.capgemini.beni.ailabar.users.infraestructure.entities.UsersEntity;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
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
    void testGenderProperty() {
        UsersEntity model = new UsersEntity();
        model.setGender("H");
        assertEquals("H", model.getGender());
    }

    @Test
    void testPhotoProperty() {
        UsersEntity model = new UsersEntity();
        model.setPhoto("photo");
        assertEquals("photo", model.getPhoto());
    }

    @Test
    void testTokenProperty() {
        UsersEntity entity = new UsersEntity();
        entity.setToken("token123");
        assertEquals("token123", entity.getToken());
    }

    @Test
    void testConstructorWithUsersModel() {
        UsersModel model = new UsersModel();
        model.setUser("user1");
        model.setPassword("password");
        model.setEmail("user1@example.com");

        UsersEntity entity = new UsersEntity(model);

        assertEquals("user1", entity.getUser());
        assertEquals("password", entity.getPassword());
        assertEquals("user1@example.com", entity.getEmail());
    }
}
