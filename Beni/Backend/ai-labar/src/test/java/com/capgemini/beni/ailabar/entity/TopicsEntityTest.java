package com.capgemini.beni.ailabar.entity;

import com.capgemini.beni.ailabar.dto.TopicsDto;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import static org.junit.jupiter.api.Assertions.assertEquals;

@ExtendWith(SpringExtension.class)
@SpringBootTest
class TopicsEntityTest {
    @Test
    void testIdProperty() {
        TopicsEntity entity = new TopicsEntity();
        entity.setId(1);
        assertEquals(1, entity.getId());
    }

    @Test
    void testTitleProperty() {
        TopicsEntity entity = new TopicsEntity();
        entity.setTitle("Topic 1");
        assertEquals("Topic 1", entity.getTitle());
    }

    @Test
    void testTypeProperty() {
        TopicsEntity entity = new TopicsEntity();
        entity.setType("Type 1");
        assertEquals("Type 1", entity.getType());
    }

    @Test
    void testQuestionProperty() {
        TopicsEntity entity = new TopicsEntity();
        entity.setQuestion("Question 1");
        assertEquals("Question 1", entity.getQuestion());
    }

    @Test
    void testOptionsProperty() {
        TopicsEntity entity = new TopicsEntity();
        entity.setOptions("[option1, option2]");
        assertEquals("[option1, option2]", entity.getOptions());
    }

    @Test
    void testVotedByProperty() {
        TopicsEntity entity = new TopicsEntity();
        entity.setVotedBy("user1");
        assertEquals("user1", entity.getVotedBy());
    }

    @Test
    void testAuthorProperty() {
        TopicsEntity entity = new TopicsEntity();
        entity.setAuthor("author1");
        assertEquals("author1", entity.getAuthor());
    }

    @Test
    void testMembersProperty() {
        TopicsEntity entity = new TopicsEntity();
        entity.setMembers("[member1, member2]");
        assertEquals("[member1, member2]", entity.getMembers());
    }

    @Test
    void testCloseDateProperty() {
        TopicsEntity entity = new TopicsEntity();
        entity.setCloseDate("2023-07-01");
        assertEquals("2023-07-01", entity.getCloseDate());
    }

    @Test
    void testVisitsProperty() {
        TopicsEntity entity = new TopicsEntity();
        entity.setVisits(5);
        assertEquals(5, entity.getVisits());
    }

    @Test
    void testStatusProperty() {
        TopicsEntity entity = new TopicsEntity();
        entity.setStatus("open");
        assertEquals("open", entity.getStatus());
    }

    @Test
    void testConstructorWithTopicsDto() {
        TopicsDto dto = new TopicsDto();
        dto.setTitle("Topic 1");
        dto.setType("Type 1");
        dto.setQuestion("Question 1");
        dto.getOptions().add("option1");
        dto.getOptions().add("option2");
        dto.setAuthor("author1");
        dto.getMembers().add("member1");
        dto.getMembers().add("member2");
        dto.setCloseDate("2023-07-01");
        dto.setVisits(5);
        dto.setStatus("open");

        TopicsEntity entity = new TopicsEntity(dto);

        assertEquals("Topic 1", entity.getTitle());
        assertEquals("Type 1", entity.getType());
        assertEquals("Question 1", entity.getQuestion());
        assertEquals("[option1, option2]", entity.getOptions());
        assertEquals("author1", entity.getAuthor());
        assertEquals("[member1, member2]", entity.getMembers());
        assertEquals("2023-07-01", entity.getCloseDate());
        assertEquals(5, entity.getVisits());
        assertEquals("open", entity.getStatus());
    }
}
