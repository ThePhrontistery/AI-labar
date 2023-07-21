package com.capgemini.beni.ailabar.domain.model;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;

@ExtendWith(SpringExtension.class)
@SpringBootTest
class TopicsModelTest {
    @Test
    void testIdProperty() {
        TopicsModel dto = new TopicsModel();
        dto.setId(1);
        assertEquals(1, dto.getId());
    }

    @Test
    void testTitleProperty() {
        TopicsModel dto = new TopicsModel();
        dto.setTitle("Topic Title");
        assertEquals("Topic Title", dto.getTitle());
    }

    @Test
    void testTypeProperty() {
        TopicsModel dto = new TopicsModel();
        dto.setType("Type");
        assertEquals("Type", dto.getType());
    }

    @Test
    void testQuestionProperty() {
        TopicsModel dto = new TopicsModel();
        dto.setQuestion("Question");
        assertEquals("Question", dto.getQuestion());
    }

    @Test
    void testOptionsProperty() {
        TopicsModel dto = new TopicsModel();
        List<String> options = Arrays.asList("Option 1", "Option 2");
        dto.setOptions(options);
        assertEquals(options, dto.getOptions());
    }

    @Test
    void testOptionsMapProperty() {
        TopicsModel dto = new TopicsModel();
        Map<String, Integer> optionsMap = new HashMap<>();
        optionsMap.put("Option 1", 1);
        optionsMap.put("Option 2", 2);
        dto.setOptionsMap(optionsMap);
        assertEquals(optionsMap, dto.getOptionsMap());
    }

    @Test
    void testVotedByProperty() {
        TopicsModel dto = new TopicsModel();
        dto.setVotedBy("User1");
        assertEquals("User1", dto.getVotedBy());
    }

    @Test
    void testAuthorProperty() {
        TopicsModel dto = new TopicsModel();
        dto.setAuthor("Author");
        assertEquals("Author", dto.getAuthor());
    }

    @Test
    void testMembersProperty() {
        TopicsModel dto = new TopicsModel();
        List<String> members = Arrays.asList("User1", "User2");
        dto.setMembers(members);
        assertEquals(members, dto.getMembers());
    }

    @Test
    void testCloseDateProperty() {
        TopicsModel dto = new TopicsModel();
        dto.setCloseDate("2023-07-01");
        assertEquals("2023-07-01", dto.getCloseDate());
    }

    @Test
    void testVisitsProperty() {
        TopicsModel dto = new TopicsModel();
        dto.setVisits(10);
        assertEquals(10, dto.getVisits());
    }

    @Test
    void testStatusProperty() {
        TopicsModel dto = new TopicsModel();
        dto.setStatus("Open");
        assertEquals("Open", dto.getStatus());
    }

    @Test
    void testUserProperty() {
        TopicsModel dto = new TopicsModel();
        dto.setUser("User");
        assertEquals("User", dto.getUser());
    }

    @Test
    void testTokenProperty() {
        TopicsModel dto = new TopicsModel();
        dto.setToken("Token");
        assertEquals("Token", dto.getToken());
    }

    @Test
    void testVotationProperty() {
        TopicsModel dto = new TopicsModel();
        List<String> votation = Arrays.asList("Option 1", "Option 2");
        dto.setVotation(votation);
        assertEquals(votation, dto.getVotation());
    }
}
