package com.capgemini.ailabar.commons.adapters.out;

import com.capgemini.ailabar.commons.utils.SpecialResponse;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class SpecialResponseInterfaceTest {

    @Mock
    private JSONObject messageMock;

    @Test
    void testSpecialResponse() {
        when(messageMock.getString("message")).thenReturn("Test Message");

        SpecialResponseInterface specialResponseInterface = new SpecialResponseInterface() {};

        Object entity = new Object();
        SpecialResponse specialResponse = specialResponseInterface.specialResponse(entity, messageMock);

        assertEquals(entity, specialResponse.getEntity());
        assertEquals("Test Message", specialResponse.getMessage());
    }
}
