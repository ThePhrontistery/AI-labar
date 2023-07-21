package com.capgemini.beni.ailabar.infrastructure.Utils;

import com.capgemini.beni.ailabar.infrastructure.utils.SpecialResponse;
import org.json.JSONObject;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

@ExtendWith(SpringExtension.class)
@SpringBootTest
class SpecialResponseTest {
    @Test
    void testGetEntity() {
        Object entity = Mockito.mock(Object.class);
        JSONObject responseJson = new JSONObject();
        responseJson.put("message", "Test Message");
        SpecialResponse specialResponse = new SpecialResponse(entity, responseJson.getString("message"));
        Assertions.assertSame(entity, specialResponse.getEntity());
    }

    @Test
    void testGetMessage() {
        JSONObject responseJson = new JSONObject();
        responseJson.put("message", "Test Message");
        SpecialResponse specialResponse = new SpecialResponse(new Object(), responseJson.getString("message"));
        Assertions.assertEquals("Test Message", specialResponse.getMessage());
    }
}
