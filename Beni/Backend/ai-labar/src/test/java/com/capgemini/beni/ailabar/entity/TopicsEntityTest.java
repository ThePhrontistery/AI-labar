package com.capgemini.beni.ailabar.entity;

import com.capgemini.beni.ailabar.dto.TopicsDto;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

@ExtendWith(SpringExtension.class)
@SpringBootTest
class TopicsEntityTest {
    @Test
    void testId() {
        Integer id = 1;
        TopicsEntity topicsEntity = new TopicsEntity();
        topicsEntity.setId(id);
        Assertions.assertEquals(id, topicsEntity.getId());
    }

    @Test
    void testTitle() {
        String title = "Test Title";
        TopicsEntity topicsEntity = new TopicsEntity();
        topicsEntity.setTitle(title);
        Assertions.assertEquals(title, topicsEntity.getTitle());
    }

    @Test
    void testType() {
        String type = "Test Type";
        TopicsEntity topicsEntity = new TopicsEntity();
        topicsEntity.setType(type);
        Assertions.assertEquals(type, topicsEntity.getType());
    }

    @Test
    void testQuestion() {
        String question = "Test Question";
        TopicsEntity topicsEntity = new TopicsEntity();
        topicsEntity.setQuestion(question);
        Assertions.assertEquals(question, topicsEntity.getQuestion());
    }

    @Test
    void testOptions() {
        String options = "Test Options";
        TopicsEntity topicsEntity = new TopicsEntity();
        topicsEntity.setOptions(options);
        Assertions.assertEquals(options, topicsEntity.getOptions());
    }

    @Test
    void testVotedBy() {
        String votedBy = "Test Voted By";
        TopicsEntity topicsEntity = new TopicsEntity();
        topicsEntity.setVotedBy(votedBy);
        Assertions.assertEquals(votedBy, topicsEntity.getVotedBy());
    }

    @Test
    void testAuthor() {
        String author = "Test Author";
        TopicsEntity topicsEntity = new TopicsEntity();
        topicsEntity.setAuthor(author);
        Assertions.assertEquals(author, topicsEntity.getAuthor());
    }

    @Test
    void testMembers() {
        String members = "Test Members";
        TopicsEntity topicsEntity = new TopicsEntity();
        topicsEntity.setMembers(members);
        Assertions.assertEquals(members, topicsEntity.getMembers());
    }

    @Test
    void testCloseDate() {
        String closeDate = "Test Close Date";
        TopicsEntity topicsEntity = new TopicsEntity();
        topicsEntity.setCloseDate(closeDate);
        Assertions.assertEquals(closeDate, topicsEntity.getCloseDate());
    }

    @Test
    void testVisits() {
        Integer visits = 10;
        TopicsEntity topicsEntity = new TopicsEntity();
        topicsEntity.setVisits(visits);
        Assertions.assertEquals(visits, topicsEntity.getVisits());
    }

    @Test
    void testStatus() {
        String status = "Test Status";
        TopicsEntity topicsEntity = new TopicsEntity();
        topicsEntity.setStatus(status);
        Assertions.assertEquals(status, topicsEntity.getStatus());
    }

    @Test
    void testConstructor() {
        TopicsDto topicDto = Mockito.mock(TopicsDto.class);
        String title = "Test Title";
        String type = "Test Type";
        String question = "Test Question";
        String options = "Test Options";
        String author = "Test Author";
        String members = "Test Members";
        String closeDate = "Test Close Date";
        Integer visits = 10;
        String status = "Test Status";

        Mockito.when(topicDto.getTitle()).thenReturn(title);
        Mockito.when(topicDto.getType()).thenReturn(type);
        Mockito.when(topicDto.getQuestion()).thenReturn(question);
        Mockito.when(topicDto.getOptions()).thenReturn(options);
        Mockito.when(topicDto.getAuthor()).thenReturn(author);
        Mockito.when(topicDto.getMembers()).thenReturn(members);
        Mockito.when(topicDto.getCloseDate()).thenReturn(closeDate);
        Mockito.when(topicDto.getVisits()).thenReturn(visits);
        Mockito.when(topicDto.getStatus()).thenReturn(status);

        TopicsEntity topicsEntity = new TopicsEntity(topicDto);

        Assertions.assertEquals(title, topicsEntity.getTitle());
        Assertions.assertEquals(type, topicsEntity.getType());
        Assertions.assertEquals(question, topicsEntity.getQuestion());
        Assertions.assertEquals(options, topicsEntity.getOptions());
        Assertions.assertEquals(author, topicsEntity.getAuthor());
        Assertions.assertEquals(members, topicsEntity.getMembers());
        Assertions.assertEquals(closeDate, topicsEntity.getCloseDate());
        Assertions.assertEquals(visits, topicsEntity.getVisits());
        Assertions.assertEquals(status, topicsEntity.getStatus());
    }
}
