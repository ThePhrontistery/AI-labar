package com.capgemini.ailabar.commons.utils;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

@ExtendWith(SpringExtension.class)
@SpringBootTest
class ConstantsTest {
    @Test
    void testTopicType() {
        Assertions.assertEquals(Constants.TopicType.TEXT_SINGLE, Constants.TopicType.valueOf("TEXT_SINGLE"));
        Assertions.assertEquals(Constants.TopicType.TEXT_MULTIPLE, Constants.TopicType.valueOf("TEXT_MULTIPLE"));
        Assertions.assertEquals(Constants.TopicType.IMAGE_SINGLE, Constants.TopicType.valueOf("IMAGE_SINGLE"));
        Assertions.assertEquals(Constants.TopicType.IMAGE_MULTIPLE, Constants.TopicType.valueOf("IMAGE_MULTIPLE"));
        Assertions.assertEquals(Constants.TopicType.AS, Constants.TopicType.valueOf("AS"));
        Assertions.assertEquals(Constants.TopicType.RATING, Constants.TopicType.valueOf("RATING"));
    }
}
