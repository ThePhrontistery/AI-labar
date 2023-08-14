package com.capgemini.ailabar.users.infraestructure.entities;

import com.capgemini.ailabar.users.domain.models.UsersModel;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(MockitoExtension.class)
class UsersEntityTest {

    @Mock
    private UsersModel mockUsersModel;

    @Test
    void testUsersEntityCreation() {
        when(mockUsersModel.getUser()).thenReturn("TestUser");
        when(mockUsersModel.getPassword()).thenReturn("TestPassword");
        when(mockUsersModel.getEmail()).thenReturn("test@example.com");
        when(mockUsersModel.getGender()).thenReturn("Male");
        when(mockUsersModel.getPhoto()).thenReturn("profile.jpg");

        UsersEntity usersEntity = new UsersEntity(mockUsersModel);

        assertEquals("TestUser", usersEntity.getUser());
        assertEquals("TestPassword", usersEntity.getPassword());
        assertEquals("test@example.com", usersEntity.getEmail());
        assertEquals("Male", usersEntity.getGender());
        assertEquals("profile.jpg", usersEntity.getPhoto());
    }
}
