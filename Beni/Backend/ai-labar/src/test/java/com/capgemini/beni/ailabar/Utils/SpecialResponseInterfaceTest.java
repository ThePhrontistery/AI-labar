package com.capgemini.beni.ailabar.Utils;

import com.capgemini.beni.ailabar.utils.SpecialResponse;
import com.capgemini.beni.ailabar.utils.SpecialResponseInterface;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import static org.junit.jupiter.api.Assertions.assertEquals;

@ExtendWith(SpringExtension.class)
@SpringBootTest
class SpecialResponseInterfaceTest {
    @Mock
    private Object entity;

    private final SpecialResponseInterface specialResponseInterface = new SpecialResponseInterface() {};

    @BeforeEach
    void setUp() {
        Mockito.reset(entity);
    }

    @Test
    void testSpecialResponse() {
        // Arrange
        String message = "Success";

        // Act
        SpecialResponse specialResponse = specialResponseInterface.specialResponse(entity, message);

        // Assert
        assertEquals(entity, specialResponse.getEntity());
        assertEquals(message, specialResponse.getMessage());
    }
}
