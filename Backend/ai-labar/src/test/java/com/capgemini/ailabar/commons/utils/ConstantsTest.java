package com.capgemini.ailabar.commons.utils;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(MockitoExtension.class)
class ConstantsTest {

    @Test
    void testTopicTypeConstants() {
        assertEquals(6, Constants.TopicType.values().length);

        assertEquals("TEXT_SINGLE", Constants.TopicType.TEXT_SINGLE.name());
        assertEquals("TEXT_MULTIPLE", Constants.TopicType.TEXT_MULTIPLE.name());
        assertEquals("IMAGE_SINGLE", Constants.TopicType.IMAGE_SINGLE.name());
        assertEquals("IMAGE_MULTIPLE", Constants.TopicType.IMAGE_MULTIPLE.name());
        assertEquals("AS", Constants.TopicType.AS.name());
        assertEquals("RATING", Constants.TopicType.RATING.name());
    }

    @Test
    void testTopicTypeValues() {
        Constants.TopicType[] values = Constants.TopicType.values();

        assertNotNull(values);
        assertEquals(6, values.length);
        assertTrue(containsTopicType(values, Constants.TopicType.TEXT_SINGLE));
        assertTrue(containsTopicType(values, Constants.TopicType.TEXT_MULTIPLE));
        assertTrue(containsTopicType(values, Constants.TopicType.IMAGE_SINGLE));
        assertTrue(containsTopicType(values, Constants.TopicType.IMAGE_MULTIPLE));
        assertTrue(containsTopicType(values, Constants.TopicType.AS));
        assertTrue(containsTopicType(values, Constants.TopicType.RATING));
    }

    private boolean containsTopicType(Constants.TopicType[] values, Constants.TopicType topicType) {
        for (Constants.TopicType value : values) {
            if (value == topicType) {
                return true;
            }
        }
        return false;
    }
}
