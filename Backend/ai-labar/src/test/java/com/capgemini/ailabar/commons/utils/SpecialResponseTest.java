package com.capgemini.ailabar.commons.utils;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.assertEquals;

@ExtendWith(MockitoExtension.class)
class SpecialResponseTest {

    @Test
    void testSpecialResponseGetters() {
        Object entity = new Object();
        String message = "Test Message";
        SpecialResponse specialResponse = new SpecialResponse(entity, message);

        assertEquals(entity, specialResponse.getEntity());
        assertEquals(message, specialResponse.getMessage());
    }
}

