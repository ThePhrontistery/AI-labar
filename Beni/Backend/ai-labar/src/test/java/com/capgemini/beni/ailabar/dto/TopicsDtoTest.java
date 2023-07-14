package com.capgemini.beni.ailabar.dto;

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
class TopicsDtoTest {
    @Test
    void testIdProperty() {
        TopicsDto dto = new TopicsDto();
        dto.setId(1);
        assertEquals(1, dto.getId());
    }

    @Test
    void testTitleProperty() {
        TopicsDto dto = new TopicsDto();
        dto.setTitle("Topic Title");
        assertEquals("Topic Title", dto.getTitle());
    }

    @Test
    void testTypeProperty() {
        TopicsDto dto = new TopicsDto();
        dto.setType("Type");
        assertEquals("Type", dto.getType());
    }

    @Test
    void testQuestionProperty() {
        TopicsDto dto = new TopicsDto();
        dto.setQuestion("Question");
        assertEquals("Question", dto.getQuestion());
    }

    @Test
    void testOptionsProperty() {
        TopicsDto dto = new TopicsDto();
        List<String> options = Arrays.asList("Option 1", "Option 2");
        dto.setOptions(options);
        assertEquals(options, dto.getOptions());
    }

    @Test
    void testOptionsMapProperty() {
        TopicsDto dto = new TopicsDto();
        Map<String, Integer> optionsMap = new HashMap<>();
        optionsMap.put("Option 1", 1);
        optionsMap.put("Option 2", 2);
        dto.setOptionsMap(optionsMap);
        assertEquals(optionsMap, dto.getOptionsMap());
    }

    @Test
    void testVotedByProperty() {
        TopicsDto dto = new TopicsDto();
        dto.setVotedBy("User1");
        assertEquals("User1", dto.getVotedBy());
    }

    @Test
    void testAuthorProperty() {
        TopicsDto dto = new TopicsDto();
        dto.setAuthor("Author");
        assertEquals("Author", dto.getAuthor());
    }

    @Test
    void testMembersProperty() {
        TopicsDto dto = new TopicsDto();
        List<String> members = Arrays.asList("User1", "User2");
        dto.setMembers(members);
        assertEquals(members, dto.getMembers());
    }

    @Test
    void testCloseDateProperty() {
        TopicsDto dto = new TopicsDto();
        dto.setCloseDate("2023-07-01");
        assertEquals("2023-07-01", dto.getCloseDate());
    }

    @Test
    void testVisitsProperty() {
        TopicsDto dto = new TopicsDto();
        dto.setVisits(10);
        assertEquals(10, dto.getVisits());
    }

    @Test
    void testStatusProperty() {
        TopicsDto dto = new TopicsDto();
        dto.setStatus("Open");
        assertEquals("Open", dto.getStatus());
    }

    @Test
    void testUserProperty() {
        TopicsDto dto = new TopicsDto();
        dto.setUser("User");
        assertEquals("User", dto.getUser());
    }

    @Test
    void testTokenProperty() {
        TopicsDto dto = new TopicsDto();
        dto.setToken("Token");
        assertEquals("Token", dto.getToken());
    }

    @Test
    void testVotationProperty() {
        TopicsDto dto = new TopicsDto();
        List<String> votation = Arrays.asList("Option 1", "Option 2");
        dto.setVotation(votation);
        assertEquals(votation, dto.getVotation());
    }
}
