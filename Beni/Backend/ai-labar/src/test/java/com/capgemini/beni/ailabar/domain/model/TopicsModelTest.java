package com.capgemini.beni.ailabar.domain.model;

import com.capgemini.beni.ailabar.infrastructure.utils.OptionsData;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

@ExtendWith(SpringExtension.class)
@SpringBootTest
class TopicsModelTest {

    private TopicsModel topicsModel;

    @BeforeEach
    void setUp() {
        topicsModel = new TopicsModel();
    }
    @Test
    void testIdProperty() {
        topicsModel.setId(1);
        assertEquals(1, topicsModel.getId());
    }

    @Test
    void testTitleProperty() {
        topicsModel.setTitle("Topic Title");
        assertEquals("Topic Title", topicsModel.getTitle());
    }

    @Test
    void testTypeProperty() {
        topicsModel.setType("Type");
        assertEquals("Type", topicsModel.getType());
    }

    @Test
    void testQuestionProperty() {
        topicsModel.setQuestion("Question");
        assertEquals("Question", topicsModel.getQuestion());
    }

    @Test
    void testOptionsProperty() {
        OptionsData option1 = new OptionsData("Option 1", 0);
        OptionsData option2 = new OptionsData("Option 2", 0);
        List<OptionsData> options = Arrays.asList(option1, option2);
        topicsModel.setOptions(options);
        assertEquals(options, topicsModel.getOptions());
    }

    @Test
    void testOptionsDataListProperty() {
        OptionsData option1 = new OptionsData("Option 1", 0);
        OptionsData option2 = new OptionsData("Option 2", 0);
        List<OptionsData> options = Arrays.asList(option1, option2);
        topicsModel.setOptionsDataList(options);
        assertEquals(options, topicsModel.getOptionsDataList());
    }

    @Test
    void testVotedByProperty() {
        topicsModel.setVotedBy("User1");
        assertEquals("User1", topicsModel.getVotedBy());
    }

    @Test
    void testAuthorProperty() {
        topicsModel.setAuthor("Author");
        assertEquals("Author", topicsModel.getAuthor());
    }

    @Test
    void testMembersProperty() {
        List<String> members = Arrays.asList("User1", "User2");
        topicsModel.setMembers(members);
        assertEquals(members, topicsModel.getMembers());
    }

    @Test
    void testCloseDateProperty() {
        topicsModel.setCloseDate("2023-07-01");
        assertEquals("2023-07-01", topicsModel.getCloseDate());
    }

    @Test
    void testVisitsProperty() {
        topicsModel.setVisits(10);
        assertEquals(10, topicsModel.getVisits());
    }

    @Test
    void testStatusProperty() {
        topicsModel.setStatus("Open");
        assertEquals("Open", topicsModel.getStatus());
    }

    @Test
    void testUserProperty() {
        topicsModel.setUser("User");
        assertEquals("User", topicsModel.getUser());
    }

    @Test
    void testTokenProperty() {
        topicsModel.setToken("Token");
        assertEquals("Token", topicsModel.getToken());
    }

    @Test
    void testCanVoteProperty() {
        topicsModel.setCanVote(true);
        assertEquals(true, topicsModel.getCanVote());
    }

    @Test
    void testVotationProperty() {
        List<String> votation = Arrays.asList("Option 1", "Option 2");
        topicsModel.setVotation(votation);
        assertEquals(votation, topicsModel.getVotation());
    }
}
