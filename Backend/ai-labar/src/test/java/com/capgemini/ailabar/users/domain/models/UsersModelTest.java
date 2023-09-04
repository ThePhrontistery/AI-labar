package com.capgemini.ailabar.users.domain.models;

import com.capgemini.ailabar.users.infraestructure.entities.UsersEntity;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.extension.ExtendWith;

@ExtendWith(MockitoExtension.class)
class UsersModelTest {
    @Mock
    private UsersEntity mockUsersEntity;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    void testConstructorFromEntity() {
        when(mockUsersEntity.getId()).thenReturn(1);
        when(mockUsersEntity.getUser()).thenReturn("TestUser");
        when(mockUsersEntity.getPassword()).thenReturn("TestPassword");
        when(mockUsersEntity.getEmail()).thenReturn("test@example.com");
        when(mockUsersEntity.getGender()).thenReturn("Male");
        when(mockUsersEntity.getPhoto()).thenReturn("profile.jpg");

        UsersModel usersModel = new UsersModel();
        usersModel.setId(mockUsersEntity.getId());
        usersModel.setUser(mockUsersEntity.getUser());
        usersModel.setPassword(mockUsersEntity.getPassword());
        usersModel.setEmail(mockUsersEntity.getEmail());
        usersModel.setGender(mockUsersEntity.getGender());
        usersModel.setPhoto(mockUsersEntity.getPhoto());

        assertEquals(1, usersModel.getId());
        assertEquals("TestUser", usersModel.getUser());
        assertEquals("TestPassword", usersModel.getPassword());
        assertEquals("test@example.com", usersModel.getEmail());
        assertEquals("Male", usersModel.getGender());
        assertEquals("profile.jpg", usersModel.getPhoto());

        verify(mockUsersEntity, times(1)).getId();
        verify(mockUsersEntity, times(1)).getUser();
        verify(mockUsersEntity, times(1)).getPassword();
        verify(mockUsersEntity, times(1)).getEmail();
        verify(mockUsersEntity, times(1)).getGender();
        verify(mockUsersEntity, times(1)).getPhoto();
    }
}
