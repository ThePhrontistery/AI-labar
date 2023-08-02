package com.capgemini.beni.ailabar.commons.adapters.out;

import com.capgemini.beni.ailabar.commons.utils.SpecialResponse;
import org.json.JSONObject;
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
        JSONObject responseJson = new JSONObject();
        responseJson.put("message", "Success");

        SpecialResponse specialResponse = specialResponseInterface.specialResponse(entity, responseJson);

        assertEquals(entity, specialResponse.getEntity());
        assertEquals(responseJson.getString("message"), specialResponse.getMessage());
    }
}
