package com.capgemini.ailabar.topics.infraestructure.controllers;

import com.capgemini.ailabar.commons.utils.Constants;
import com.capgemini.ailabar.commons.utils.SpecialResponse;
import com.capgemini.ailabar.options.domain.models.OptionsModel;
import com.capgemini.ailabar.topics.application.services.TopicsService;
import com.capgemini.ailabar.topics.domain.exceptions.*;
import com.capgemini.ailabar.topics.domain.models.TopicsModel;
import com.capgemini.ailabar.users.domain.models.UsersModel;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class TopicsControllerTest {

    @InjectMocks
    private TopicsController topicsController;

    @Mock
    private TopicsService topicsService;

    @Test
    void testLoadTopicsSuccess() {
        UsersModel usersModel = new UsersModel();
        usersModel.setUser("username");
        usersModel.setToken("token");
        usersModel.setElements(10);

        Map<String, Object> responseMap = new HashMap<>();
        responseMap.put("pagination", Collections.singletonList(new HashMap<String, Integer>() {{
            put("page", 1);
            put("elements", 1);
            put("total", 1);
        }}));
        responseMap.put("entity", Collections.singletonList(new TopicsModel()));

        when(topicsService.loadTopics(usersModel)).thenReturn(responseMap);

        String expectedMessage = "OK";

        ResponseEntity<SpecialResponse> actualResponse = topicsController.loadTopics(usersModel);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        SpecialResponse specialResponse = actualResponse.getBody();
        assertNotNull(specialResponse);
        assertEquals(expectedMessage, specialResponse.getMessage());
        assertEquals(responseMap, specialResponse.getEntity());

        verify(topicsService, times(1)).loadTopics(usersModel);
    }

    @Test
    void testLoadTopicsWithFiltersSuccess() {
        UsersModel usersModel = new UsersModel();
        usersModel.setUser("username");
        usersModel.setToken("token");
        usersModel.setElements(10);

        List<String> filters = new ArrayList<>();
        filters.add("mines");
        filters.add("status");
        filters.add("votePending");

        usersModel.setFilters(filters);

        Map<String, Object> responseMap = new HashMap<>();
        responseMap.put("pagination", Collections.singletonList(new HashMap<String, Integer>() {{
            put("page", 1);
            put("elements", 1);
            put("total", 1);
        }}));
        responseMap.put("entity", Collections.singletonList(new TopicsModel()));

        when(topicsService.loadTopics(usersModel)).thenReturn(responseMap);

        String expectedMessage = "OK";

        ResponseEntity<SpecialResponse> actualResponse = topicsController.loadTopics(usersModel);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        SpecialResponse specialResponse = actualResponse.getBody();
        assertNotNull(specialResponse);
        assertEquals(expectedMessage, specialResponse.getMessage());
        assertEquals(responseMap, specialResponse.getEntity());

        verify(topicsService, times(1)).loadTopics(usersModel);
    }

    @Test
    void testCreateTopicSuccess() {
        TopicsModel topicsModel = new TopicsModel();
        topicsModel.setUser("User");
        topicsModel.setToken("Token");
        topicsModel.setTitle("Title");
        topicsModel.setType("AS");
        topicsModel.setQuestion("Question");

        List<OptionsModel> options = new ArrayList<>();
        options.add(new OptionsModel("User1", 0));
        options.add(new OptionsModel("User2", 0));
        topicsModel.setOptions(options);

        topicsModel.setGroupName("Grupo1");

        doNothing().when(topicsService).createTopic(topicsModel);

        String expectedMessage = "Topic created successfully";

        ResponseEntity<SpecialResponse> actualResponse = topicsController.createTopic(topicsModel);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        SpecialResponse specialResponse = actualResponse.getBody();
        assertNotNull(specialResponse);
        assertEquals(expectedMessage, specialResponse.getMessage());
        assertNull(specialResponse.getEntity());

        verify(topicsService, times(1)).createTopic(topicsModel);
    }

    @Test
    void testEditTopicSuccess() {
        TopicsModel topicsModel = new TopicsModel();
        topicsModel.setId(1);
        topicsModel.setUser("User");
        topicsModel.setToken("Token");
        topicsModel.setTitle("Title");
        topicsModel.setType("AS");
        topicsModel.setQuestion("Question");

        List<OptionsModel> options = new ArrayList<>();
        options.add(new OptionsModel("User1", 0));
        options.add(new OptionsModel("User2", 0));
        topicsModel.setOptions(options);

        topicsModel.setGroupName("Grupo1");

        doNothing().when(topicsService).editTopic(topicsModel);

        String expectedMessage = "Topic edited successfully";

        ResponseEntity<SpecialResponse> actualResponse = topicsController.editTopic(topicsModel);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        SpecialResponse specialResponse = actualResponse.getBody();
        assertNotNull(specialResponse);
        assertEquals(expectedMessage, specialResponse.getMessage());
        assertNull(specialResponse.getEntity());

        verify(topicsService).editTopic(topicsModel);
    }

    @Test
    void testCloseTopicSuccess() {
        TopicsModel topicsModel = new TopicsModel();
        topicsModel.setId(1);
        topicsModel.setUser("User");
        topicsModel.setToken("Token");

        doNothing().when(topicsService).closeTopic(topicsModel);

        String expectedMessage = "The topic has been closed";

        ResponseEntity<SpecialResponse> actualResponse = topicsController.closeTopic(topicsModel);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        SpecialResponse specialResponse = actualResponse.getBody();
        assertNotNull(specialResponse);
        assertEquals(expectedMessage, specialResponse.getMessage());
        assertNull(specialResponse.getEntity());

        verify(topicsService, times(1)).closeTopic(topicsModel);
    }

    @Test
    void testReOpenTopicSuccess() {
        TopicsModel topicsModel = new TopicsModel();
        topicsModel.setId(1);
        topicsModel.setUser("User");
        topicsModel.setToken("Token");

        doNothing().when(topicsService).reOpenTopic(topicsModel);

        String expectedMessage = "Topic reopened";

        ResponseEntity<SpecialResponse> actualResponse = topicsController.reOpenTopic(topicsModel);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        SpecialResponse specialResponse = actualResponse.getBody();
        assertNotNull(specialResponse);
        assertEquals(expectedMessage, specialResponse.getMessage());
        assertNull(specialResponse.getEntity());

        verify(topicsService, times(1)).reOpenTopic(topicsModel);
    }

    @Test
    void testDeleteTopicSuccess() {
        TopicsModel topicsModel = new TopicsModel();
        topicsModel.setId(1);
        topicsModel.setUser("User");
        topicsModel.setToken("Token");

        doNothing().when(topicsService).deleteTopic(topicsModel);

        String expectedMessage = "The topic has been deleted";

        ResponseEntity<SpecialResponse> actualResponse = topicsController.deleteTopic(topicsModel);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        SpecialResponse specialResponse = actualResponse.getBody();
        assertNotNull(specialResponse);
        assertEquals(expectedMessage, specialResponse.getMessage());
        assertNull(specialResponse.getEntity());

        verify(topicsService, times(1)).deleteTopic(topicsModel);
    }

    @Test
    void testGetTopicsDatabaseSuccess() {
        List<TopicsModel> topicsList = new ArrayList<>();

        TopicsModel topicModel = new TopicsModel();
        topicModel.setId(1);
        topicModel.setUser("User");
        topicModel.setToken("Token");
        topicModel.setTitle("Title");
        topicModel.setType("AS");
        topicModel.setQuestion("Question");

        List<OptionsModel> options = new ArrayList<>();
        options.add(new OptionsModel("User1", 0));
        options.add(new OptionsModel("User2", 0));
        topicModel.setOptions(options);

        topicModel.setGroupName("Grupo1");

        topicsList.add(topicModel);

        when(topicsService.getTopicsDatabase()).thenReturn(topicsList);

        String expectedMessage = "OK";

        ResponseEntity<SpecialResponse> actualResponse = topicsController.getTopicsDatabase();

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        SpecialResponse specialResponse = actualResponse.getBody();
        assertNotNull(specialResponse);
        assertEquals(expectedMessage, specialResponse.getMessage());
        assertEquals(topicsList, specialResponse.getEntity());

        verify(topicsService, times(1)).getTopicsDatabase();
    }

    @Test
    void testVoteSuccess() {
        TopicsModel topicsModel = new TopicsModel();
        topicsModel.setUser("User");
        topicsModel.setToken("Token");
        topicsModel.setId(1);
        topicsModel.setVotation(Arrays.asList("Option1", "Option2"));

        doNothing().when(topicsService).vote(topicsModel);

        String expectedMessage = "Votation updated successfully";

        ResponseEntity<SpecialResponse> actualResponse = topicsController.vote(topicsModel);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        SpecialResponse specialResponse = actualResponse.getBody();
        assertNotNull(specialResponse);
        assertEquals(expectedMessage, specialResponse.getMessage());
        assertNull(specialResponse.getEntity());

        verify(topicsService, times(1)).vote(topicsModel);
    }

    @Test
    void testVotingResultsSuccess() {
        TopicsModel topicsModel = new TopicsModel();
        topicsModel.setUser("User");
        topicsModel.setToken("Token");
        topicsModel.setId(1);

        List<OptionsModel> optionsModelList = new ArrayList<>();
        optionsModelList.add(new OptionsModel("Option1", 5));
        optionsModelList.add(new OptionsModel("Option2", 10));

        Map<String, List<OptionsModel>> resultMap = new HashMap<>();
        resultMap.put(Constants.TopicType.TEXT_SINGLE.toString(), optionsModelList);

        when(topicsService.votingResults(topicsModel)).thenReturn(resultMap);

        String expectedMessage = Constants.TopicType.TEXT_SINGLE.toString();

        ResponseEntity<SpecialResponse> actualResponse = topicsController.votingResults(topicsModel);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        SpecialResponse specialResponse = actualResponse.getBody();
        assertNotNull(specialResponse);
        assertEquals(expectedMessage, specialResponse.getMessage());
        assertNotNull(specialResponse.getEntity());


        verify(topicsService, times(1)).votingResults(topicsModel);
    }

    @Test
    void testHandlerLoadTopicException() {
        LoadTopicException exception = new LoadTopicException("Load topic error message");

        ResponseEntity<SpecialResponse> response = topicsController.handlerLoadTopicException(exception);

        assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
        JSONObject expectedJson = new JSONObject();
        expectedJson.put("message", "Load topic error message");
        assertEquals(expectedJson.getString("message"), Objects.requireNonNull(response.getBody()).getMessage());
    }

    @Test
    void testHandlerCreateTopicException() {
        CreateTopicException exception = new CreateTopicException("Create topic error message");

        ResponseEntity<SpecialResponse> response = topicsController.handlerCreateTopicException(exception);

        assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
        JSONObject expectedJson = new JSONObject();
        expectedJson.put("message", "Create topic error message");
        assertEquals(expectedJson.getString("message"), Objects.requireNonNull(response.getBody()).getMessage());
    }

    @Test
    void testHandlerEditTopicException() {
        EditTopicException exception = new EditTopicException("Edit topic error message");

        ResponseEntity<SpecialResponse> response = topicsController.handlerEditTopicException(exception);

        assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
        JSONObject expectedJson = new JSONObject();
        expectedJson.put("message", "Edit topic error message");
        assertEquals(expectedJson.getString("message"), Objects.requireNonNull(response.getBody()).getMessage());
    }

    @Test
    void testHandlerCloseTopicException() {
        CloseTopicException exception = new CloseTopicException("Close topic error message");

        ResponseEntity<SpecialResponse> response = topicsController.handlerCloseTopicException(exception);

        assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
        JSONObject expectedJson = new JSONObject();
        expectedJson.put("message", "Close topic error message");
        assertEquals(expectedJson.getString("message"), Objects.requireNonNull(response.getBody()).getMessage());
    }

    @Test
    void testHandlerReOpenTopicException() {
        ReOpenTopicException exception = new ReOpenTopicException("Reopen topic error message");

        ResponseEntity<SpecialResponse> response = topicsController.handlerReOpenTopicException(exception);

        assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
        JSONObject expectedJson = new JSONObject();
        expectedJson.put("message", "Reopen topic error message");
        assertEquals(expectedJson.getString("message"), Objects.requireNonNull(response.getBody()).getMessage());
    }

    @Test
    void testHandlerVoteTopicException() {
        VoteTopicException exception = new VoteTopicException("Vote topic error message");

        ResponseEntity<SpecialResponse> response = topicsController.handlerVoteTopicException(exception);

        assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
        JSONObject expectedJson = new JSONObject();
        expectedJson.put("message", "Vote topic error message");
        assertEquals(expectedJson.getString("message"), Objects.requireNonNull(response.getBody()).getMessage());
    }

    @Test
    void testHandlerVotingResultsTopicException() {
        VotingResultsTopicException exception = new VotingResultsTopicException("Voting results topic error message");

        ResponseEntity<SpecialResponse> response = topicsController.handlerVotingResultsTopicException(exception);

        assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
        JSONObject expectedJson = new JSONObject();
        expectedJson.put("message", "Voting results topic error message");
        assertEquals(expectedJson.getString("message"), Objects.requireNonNull(response.getBody()).getMessage());
    }

    @Test
    void testHandlerMailServiceException() {
        MailServiceException exception = new MailServiceException("Mail service error message");

        ResponseEntity<SpecialResponse> response = topicsController.handlerMailServiceException(exception);

        assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
        JSONObject expectedJson = new JSONObject();
        expectedJson.put("message", "Mail service error message");
        assertEquals(expectedJson.getString("message"), Objects.requireNonNull(response.getBody()).getMessage());
    }
}
