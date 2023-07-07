package com.capgemini.beni.ailabar.Utils;

import com.capgemini.beni.ailabar.utils.SpecialResponse;
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
        SpecialResponse specialResponse = new SpecialResponse(entity, "Test Message");
        Assertions.assertSame(entity, specialResponse.getEntity());
    }

    @Test
    void testGetMessage() {
        SpecialResponse specialResponse = new SpecialResponse(new Object(), "Test Message");
        Assertions.assertEquals("Test Message", specialResponse.getMessage());
    }
}

