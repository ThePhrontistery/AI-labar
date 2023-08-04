package com.capgemini.ailabar.topics.infraestructure.entities;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import static org.junit.jupiter.api.Assertions.assertEquals;

@ExtendWith(SpringExtension.class)
@SpringBootTest
class TopicsEntityTest {

    @InjectMocks
    private TopicsEntity topicsEntity;

    @Test
    void testTitleProperty() {
        String title = "Sample Title";
        topicsEntity.setTitle(title);
        assertEquals(title, topicsEntity.getTitle());
    }

    @Test
    void testTypeProperty() {
        String type = "Type 1";
        topicsEntity.setType(type);
        assertEquals(type, topicsEntity.getType());
    }

    @Test
    void testQuestionProperty() {
        String question = "Sample Question";
        topicsEntity.setQuestion(question);
        assertEquals(question, topicsEntity.getQuestion());
    }

    @Test
    void testOptionsProperty() {
        String options = "[\"Option 1\", \"Option 2\"]";
        topicsEntity.setOptions(options);
        assertEquals(options, topicsEntity.getOptions());
    }

    @Test
    void testVotedByProperty() {
        String votedBy = "User1";
        topicsEntity.setVotedBy(votedBy);
        assertEquals(votedBy, topicsEntity.getVotedBy());
    }

    @Test
    void testAuthorProperty() {
        String author = "Author1";
        topicsEntity.setAuthor(author);
        assertEquals(author, topicsEntity.getAuthor());
    }

    @Test
    void testMembersProperty() {
        String members = "[\"User1\", \"User2\"]";
        topicsEntity.setMembers(members);
        assertEquals(members, topicsEntity.getMembers());
    }

    @Test
    void testCloseDateProperty() {
        String closeDate = "2023-07-01";
        topicsEntity.setCloseDate(closeDate);
        assertEquals(closeDate, topicsEntity.getCloseDate());
    }

    @Test
    void testVisitsProperty() {
        int visits = 10;
        topicsEntity.setVisits(visits);
        assertEquals(visits, topicsEntity.getVisits());
    }

    @Test
    void testStatusProperty() {
        String status = "Open";
        topicsEntity.setStatus(status);
        assertEquals(status, topicsEntity.getStatus());
    }
}
