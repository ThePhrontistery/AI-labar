package com.capgemini.beni.ailabar.commons.utils;

import com.capgemini.beni.ailabar.commons.utils.OptionsData;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(SpringExtension.class)
@SpringBootTest
class OptionsDataTest {
    @Test
    void testOptionsDataConstructor() {
        String option = "Option 1";
        Integer votes = 5;

        OptionsData optionsData = new OptionsData(option, votes);

        assertEquals(option, optionsData.getOption());
        assertEquals(votes, optionsData.getVotes());
    }

    @Test
    void testOptionsDataEquality() {
        String option1 = "Option 1";
        Integer votes1 = 5;

        String option2 = "Option 1";
        Integer votes2 = 5;

        OptionsData optionsData1 = new OptionsData(option1, votes1);
        OptionsData optionsData2 = new OptionsData(option2, votes2);

        assertEquals(optionsData1, optionsData2);
    }

    @Test
    void testOptionsDataInequality() {
        String option1 = "Option 1";
        Integer votes1 = 5;

        String option2 = "Option 2";
        Integer votes2 = 10;

        OptionsData optionsData1 = new OptionsData(option1, votes1);
        OptionsData optionsData2 = new OptionsData(option2, votes2);

        assertNotEquals(optionsData1, optionsData2);
    }
}
