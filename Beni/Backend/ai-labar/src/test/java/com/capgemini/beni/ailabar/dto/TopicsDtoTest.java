package com.capgemini.beni.ailabar.dto;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

@ExtendWith(SpringExtension.class)
@SpringBootTest
class TopicsDtoTest {
    @Test
    void testId() {
        TopicsDto topicsDto = new TopicsDto();

        Integer expectedId = 1;
        topicsDto.setId(expectedId);

        Integer actualId = topicsDto.getId();

        assertEquals(expectedId, actualId);
    }

    @Test
    void testTitle() {
        TopicsDto topicsDto = new TopicsDto();

        String expectedTitle = "Test Topic";
        topicsDto.setTitle(expectedTitle);

        String actualTitle = topicsDto.getTitle();

        assertEquals(expectedTitle, actualTitle);
    }

    @Test
    void testType() {
        TopicsDto topicsDto = new TopicsDto();

        String expectedType = "Question";
        topicsDto.setType(expectedType);

        String actualType = topicsDto.getType();

        assertEquals(expectedType, actualType);
    }

    @Test
    void testQuestion() {
        TopicsDto topicsDto = new TopicsDto();

        String expectedQuestion = "What is your favorite color?";
        topicsDto.setQuestion(expectedQuestion);

        String actualQuestion = topicsDto.getQuestion();

        assertEquals(expectedQuestion, actualQuestion);
    }

    @Test
    void testOptions() {
        TopicsDto topicsDto = new TopicsDto();

        String expectedOptions = "Red, Blue, Green";
        topicsDto.setOptions(expectedOptions);

        String actualOptions = topicsDto.getOptions();

        assertEquals(expectedOptions, actualOptions);
    }

    @Test
    void testVotedBy() {
        TopicsDto topicsDto = new TopicsDto();

        String expectedVotedBy = "John, Jane, Bob";
        topicsDto.setVotedBy(expectedVotedBy);

        String actualVotedBy = topicsDto.getVotedBy();

        assertEquals(expectedVotedBy, actualVotedBy);
    }

    @Test
    void testAuthor() {
        TopicsDto topicsDto = new TopicsDto();

        String expectedAuthor = "AuthorUser";
        topicsDto.setAuthor(expectedAuthor);

        String actualAuthor = topicsDto.getAuthor();

        assertEquals(expectedAuthor, actualAuthor);
    }

    @Test
    void testMembers() {
        TopicsDto topicsDto = new TopicsDto();

        String expectedMembers = "John, Jane, Bob";
        topicsDto.setMembers(expectedMembers);

        String actualMembers = topicsDto.getMembers();

        assertEquals(expectedMembers, actualMembers);
    }

    @Test
    void testCloseDate() {
        TopicsDto topicsDto = new TopicsDto();

        String expectedCloseDate = "2023-07-31";
        topicsDto.setCloseDate(expectedCloseDate);

        String actualCloseDate = topicsDto.getCloseDate();

        assertEquals(expectedCloseDate, actualCloseDate);
    }

    @Test
    void testVisits() {
        TopicsDto topicsDto = new TopicsDto();

        Integer expectedVisits = 10;
        topicsDto.setVisits(expectedVisits);

        Integer actualVisits = topicsDto.getVisits();

        assertEquals(expectedVisits, actualVisits);
    }

    @Test
    void testStatus() {
        TopicsDto topicsDto = new TopicsDto();

        String expectedStatus = "Open";
        topicsDto.setStatus(expectedStatus);

        String actualStatus = topicsDto.getStatus();

        assertEquals(expectedStatus, actualStatus);
    }

    @Test
    void testUser() {
        TopicsDto topicsDto = new TopicsDto();

        String expectedUser = "User1";
        topicsDto.setUser(expectedUser);

        String actualUser = topicsDto.getUser();

        assertEquals(expectedUser, actualUser);
    }

    @Test
    void testToken() {
        TopicsDto topicsDto = new TopicsDto();

        String expectedToken = "token";
        topicsDto.setToken(expectedToken);

        String actualToken = topicsDto.getToken();

        assertEquals(expectedToken, actualToken);
    }

    @Test
    void testVotation() {
        TopicsDto topicsDto = new TopicsDto();

        List<String> expectedVotation = Arrays.asList("Option1", "Option2", "Option3");
        topicsDto.setVotation(expectedVotation);

        List<String> actualVotation = topicsDto.getVotation();

        assertEquals(expectedVotation, actualVotation);
    }
}

