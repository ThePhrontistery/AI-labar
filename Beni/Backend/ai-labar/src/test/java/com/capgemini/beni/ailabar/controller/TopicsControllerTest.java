package com.capgemini.beni.ailabar.controller;

import com.capgemini.beni.ailabar.dto.TopicsDto;
import com.capgemini.beni.ailabar.dto.UsersDto;
import com.capgemini.beni.ailabar.entity.TopicsEntity;
import com.capgemini.beni.ailabar.entity.UsersEntity;
import com.capgemini.beni.ailabar.service.MailService;
import com.capgemini.beni.ailabar.service.TopicsService;
import com.capgemini.beni.ailabar.service.UsersService;
import com.capgemini.beni.ailabar.utils.Constants;
import com.capgemini.beni.ailabar.utils.SpecialResponse;
import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
import org.apache.commons.codec.digest.DigestUtils;
import org.json.JSONObject;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(SpringExtension.class)
@SpringBootTest
class TopicsControllerTest {
    @Mock
    private TopicsService topicsService;

    @Mock
    private UsersService usersService;

    @Mock
    private MailService mailService;

    @InjectMocks
    private TopicsController topicsController;

    @BeforeEach
    public void setUp() {
        Mockito.reset(topicsService, usersService, mailService);
    }

    @Test
    void testLogin_UserAndPasswordMissing_ReturnsBadRequest() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("");
        userDto.setPassword("");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "User and password are required to login");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.BAD_GATEWAY);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.login(userDto);

        assertEquals(HttpStatus.BAD_GATEWAY, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verifyNoInteractions(topicsService);
    }

    @Test
    void testLogin_LoginFailed_ReturnsUnauthorized() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("exampleUser");
        userDto.setPassword("examplePassword");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "Login failed");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.UNAUTHORIZED);

        when(topicsService.login(userDto.getUser(), DigestUtils.sha256Hex(userDto.getPassword()))).thenReturn(false);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.login(userDto);

        assertEquals(HttpStatus.UNAUTHORIZED, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(topicsService, times(1)).login(userDto.getUser(), DigestUtils.sha256Hex(userDto.getPassword()));
        verifyNoMoreInteractions(topicsService);
    }

    @Test
    void testLogin_UserNotFound_ReturnsNotFound() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("exampleUser");
        userDto.setPassword("examplePassword");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "User not found");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.NOT_FOUND);

        when(topicsService.login(userDto.getUser(), DigestUtils.sha256Hex(userDto.getPassword()))).thenReturn(true);
        when(usersService.findByUser(userDto.getUser())).thenReturn(null);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.login(userDto);

        assertEquals(HttpStatus.NOT_FOUND, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(topicsService, times(1)).login(userDto.getUser(), DigestUtils.sha256Hex(userDto.getPassword()));
        verify(usersService, times(1)).findByUser(userDto.getUser());
        verifyNoMoreInteractions(topicsService, usersService);
    }

    @Test
    void testLogin_LoginSuccessful_ReturnsToken() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("exampleUser");
        userDto.setPassword("examplePassword");

        UsersEntity userEntity = new UsersEntity();
        userEntity.setToken("exampleToken");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "Login successful");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(userEntity.getToken(), expectedResponseJson), HttpStatus.OK);

        when(topicsService.login(userDto.getUser(), DigestUtils.sha256Hex(userDto.getPassword()))).thenReturn(true);
        when(usersService.findByUser(userDto.getUser())).thenReturn(userEntity);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.login(userDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(topicsService, times(1)).login(userDto.getUser(), DigestUtils.sha256Hex(userDto.getPassword()));
        verify(usersService, times(1)).findByUser(userDto.getUser());
        verifyNoMoreInteractions(topicsService, usersService);
    }

    @Test
    void testLoadTopics_UnauthorizedUser_ReturnsNotFound() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("exampleUser");
        userDto.setToken("invalidToken");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "Unauthorized user");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.NOT_FOUND);

        when(usersService.checkToken(userDto.getUser(), userDto.getToken())).thenReturn(false);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.loadTopics(userDto);

        assertEquals(HttpStatus.NOT_FOUND, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(userDto.getUser(), userDto.getToken());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testLoadTopics_NoTopicsFound_ReturnsOk() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("exampleUser");
        userDto.setToken("validToken");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "There are no topics related to the user");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(userDto.getUser(), userDto.getToken())).thenReturn(true);
        when(topicsService.loadTopics(userDto.getUser())).thenReturn(Collections.emptyList());

        ResponseEntity<SpecialResponse> actualResponse = topicsController.loadTopics(userDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(userDto.getUser(), userDto.getToken());
        verify(topicsService, times(1)).loadTopics(userDto.getUser());
        verifyNoMoreInteractions(usersService, topicsService);
    }

    @Test
    void testLoadTopics_TopicsFound_ReturnsOk() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("exampleUser");
        userDto.setToken("validToken");

        TopicsEntity topic1 = new TopicsEntity();
        topic1.setId(1);
        topic1.setTitle("Topic 1");
        topic1.setType(String.valueOf(Constants.TopicType.TEXT_SINGLE));
        topic1.setQuestion("Question 1");
        topic1.setOptions("{\"Lunes, Martes\":0,\"Miércoles\":0,\"Jueves, Viernes\":0}");
        topic1.setAuthor("Author 1");
        topic1.setMembers("[\"Member 1\",\"Member 2\"]");
        topic1.setVisits(5);
        topic1.setStatus(Constants.STATUS_OPENED);

        TopicsEntity topic2 = new TopicsEntity();
        topic2.setId(2);
        topic2.setTitle("Topic 2");
        topic2.setType(String.valueOf(Constants.TopicType.TEXT_SINGLE));
        topic2.setQuestion("Question 2");
        topic2.setOptions("{\"Lunes, Martes\":0,\"Miércoles\":0,\"Jueves, Viernes\":0}");
        topic2.setAuthor("Author 2");
        topic2.setMembers("[\"Member 3\"]");
        topic2.setVisits(3);
        topic2.setStatus(Constants.STATUS_OPENED);

        List<TopicsEntity> topicsList = Arrays.asList(topic1, topic2);

        List<TopicsDto> expectedTopicsDtoList = new ArrayList<>();

        TopicsDto topicsDto1 = new TopicsDto();
        topicsDto1.setId(topic1.getId());
        topicsDto1.setTitle(topic1.getTitle());
        topicsDto1.setType(topic1.getType());
        topicsDto1.setQuestion(topic1.getQuestion());
        topicsDto1.setOptions(Collections.singletonList(topic1.getOptions()));
        topicsDto1.setAuthor(topic1.getAuthor());
        topicsDto1.setMembers(Collections.singletonList(topic1.getMembers()));
        topicsDto1.setVisits(topic1.getVisits());
        topicsDto1.setStatus(topic1.getStatus());

        TopicsDto topicsDto2 = new TopicsDto();
        topicsDto2.setId(topic2.getId());
        topicsDto2.setTitle(topic2.getTitle());
        topicsDto2.setType(topic2.getType());
        topicsDto2.setQuestion(topic2.getQuestion());
        topicsDto2.setOptions(Collections.singletonList(topic2.getOptions()));
        topicsDto2.setAuthor(topic2.getAuthor());
        topicsDto2.setMembers(Collections.singletonList(topic2.getMembers()));
        topicsDto2.setVisits(topic2.getVisits());
        topicsDto2.setStatus(topic2.getStatus());

        expectedTopicsDtoList.add(topicsDto1);
        expectedTopicsDtoList.add(topicsDto2);

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "OK");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(expectedTopicsDtoList, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(userDto.getUser(), userDto.getToken())).thenReturn(true);
        when(topicsService.loadTopics(userDto.getUser())).thenReturn(topicsList);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.loadTopics(userDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(userDto.getUser(), userDto.getToken());
        verify(topicsService, times(1)).loadTopics(userDto.getUser());
        verifyNoMoreInteractions(usersService, topicsService);
    }

    @Test
    void testOpenTopic_UnauthorizedUser_ReturnsNotFound() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setUser("exampleUser");
        topicDto.setToken("invalidToken");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "Unauthorized user");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.NOT_FOUND);

        when(usersService.checkToken(topicDto.getUser(), topicDto.getToken())).thenReturn(false);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.openTopic(topicDto);

        assertEquals(HttpStatus.NOT_FOUND, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(topicDto.getUser(), topicDto.getToken());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testOpenTopic_TopicNotFound_ReturnsOk() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setUser("exampleUser");
        topicDto.setToken("validToken");
        topicDto.setId(1);

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "There is no topic with that id");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(topicDto.getUser(), topicDto.getToken())).thenReturn(true);
        when(topicsService.getTopicForEdit(topicDto.getId())).thenReturn(null);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.openTopic(topicDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(topicDto.getUser(), topicDto.getToken());
        verify(topicsService, times(1)).getTopicForEdit(topicDto.getId());
        verifyNoMoreInteractions(usersService, topicsService);
    }

    @Test
    void testOpenTopic_TopicClosed_ReturnsOk() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setUser("exampleUser");
        topicDto.setToken("validToken");
        topicDto.setId(1);

        TopicsEntity topicEntity = new TopicsEntity();
        topicEntity.setId(1);
        topicEntity.setTitle("Topic 1");
        topicEntity.setType("Type 1");
        topicEntity.setQuestion("Question 1");
        topicEntity.setOptions("[\"Option 1\",\"Option 2\",\"Option 3\"]");
        topicEntity.setAuthor("Author 1");
        topicEntity.setMembers("[\"Member 1\",\"Member 2\"]");
        topicEntity.setVisits(5);
        topicEntity.setStatus(Constants.STATUS_CLOSED);

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "The topic is closed");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(topicDto.getUser(), topicDto.getToken())).thenReturn(true);
        when(topicsService.getTopicForEdit(topicDto.getId())).thenReturn(topicEntity);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.openTopic(topicDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(topicDto.getUser(), topicDto.getToken());
        verify(topicsService, times(1)).getTopicForEdit(topicDto.getId());
        verifyNoMoreInteractions(usersService, topicsService);
    }

    @Test
    void testOpenTopic_UserNotAuthor_ReturnsOk() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setUser("exampleUser");
        topicDto.setToken("validToken");
        topicDto.setId(1);

        TopicsEntity topicEntity = new TopicsEntity();
        topicEntity.setId(1);
        topicEntity.setTitle("Topic 1");
        topicEntity.setType("Type 1");
        topicEntity.setQuestion("Question 1");
        topicEntity.setOptions("[\"Option 1\",\"Option 2\",\"Option 3\"]");
        topicEntity.setAuthor("Author 2");
        topicEntity.setMembers("[\"Member 1\",\"Member 2\"]");
        topicEntity.setVisits(5);
        topicEntity.setStatus(Constants.STATUS_OPENED);

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "The user is not the author of the topic");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(topicDto.getUser(), topicDto.getToken())).thenReturn(true);
        when(topicsService.getTopicForEdit(topicDto.getId())).thenReturn(topicEntity);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.openTopic(topicDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(topicDto.getUser(), topicDto.getToken());
        verify(topicsService, times(1)).getTopicForEdit(topicDto.getId());
        verifyNoMoreInteractions(usersService, topicsService);
    }

    @Test
    void testOpenTopic_ValidInput_ReturnsOk() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setUser("exampleUser");
        topicDto.setToken("validToken");
        topicDto.setId(1);

        TopicsEntity topicEntity = new TopicsEntity();
        topicEntity.setId(1);
        topicEntity.setTitle("Topic 1");
        topicEntity.setType("Type 1");
        topicEntity.setQuestion("Question 1");
        topicEntity.setOptions("{\"Option 1\": 1, \"Option 2\": 2}");
        topicEntity.setAuthor("exampleUser");
        topicEntity.setMembers("[\"Member 1\",\"Member 2\"]");
        topicEntity.setVisits(5);
        topicEntity.setStatus(Constants.STATUS_OPENED);

        TopicsDto expectedTopicsDto = new TopicsDto();
        expectedTopicsDto.setId(topicEntity.getId());
        expectedTopicsDto.setTitle(topicEntity.getTitle());
        expectedTopicsDto.setType(topicEntity.getType());
        expectedTopicsDto.setQuestion(topicEntity.getQuestion());
        expectedTopicsDto.setOptions(List.of("{\"Option 1\": 1, \"Option 2\": 2}"));
        expectedTopicsDto.setAuthor(topicEntity.getAuthor());
        expectedTopicsDto.setMembers(List.of("[\"User 1\", \"User 2\"]"));
        expectedTopicsDto.setVisits(topicEntity.getVisits());
        expectedTopicsDto.setStatus(topicEntity.getStatus());

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "OK");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(expectedTopicsDto, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(topicDto.getUser(), topicDto.getToken())).thenReturn(true);
        when(topicsService.getTopicForEdit(topicDto.getId())).thenReturn(topicEntity);
        doNothing().when(topicsService).saveTopic(any(TopicsEntity.class));

        ResponseEntity<SpecialResponse> actualResponse = topicsController.openTopic(topicDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(topicDto.getUser(), topicDto.getToken());
        verify(topicsService, times(1)).getTopicForEdit(topicDto.getId());
        verify(topicsService, times(1)).saveTopic(topicEntity);
        verifyNoMoreInteractions(usersService, topicsService);
    }

    @Test
    void testCreateTopic_AllDataMissing_ReturnsBadRequest() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setTitle("");
        topicDto.setType("");
        topicDto.setQuestion("");
        topicDto.setOptions(new ArrayList<>());
        topicDto.setUser("");
        topicDto.setMembers(new ArrayList<>());

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "All data is required to edit a topic");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.BAD_GATEWAY);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.createTopic(topicDto);

        assertEquals(HttpStatus.BAD_GATEWAY, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verifyNoMoreInteractions(usersService, topicsService);
    }

    @Test
    void testCreateTopic_UnauthorizedUser_ReturnsNotFound() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setTitle("Topic 1");
        topicDto.setType("Type 1");
        topicDto.setQuestion("Question 1");
        topicDto.setOptions(Arrays.asList("Option 1", "Option 2"));
        topicDto.setUser("exampleUser");
        topicDto.setMembers(Arrays.asList("Member 1", "Member 2"));

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "Unauthorized user");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.NOT_FOUND);

        when(usersService.checkToken(topicDto.getUser(), topicDto.getToken())).thenReturn(false);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.createTopic(topicDto);

        assertEquals(HttpStatus.NOT_FOUND, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(topicDto.getUser(), topicDto.getToken());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testCreateTopic_DuplicateTopic_ReturnsOk() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setTitle("Topic 1");
        topicDto.setType("Type 1");
        topicDto.setQuestion("Question 1");
        topicDto.setOptions(Arrays.asList("Option 1", "Option 2"));
        topicDto.setUser("exampleUser");
        topicDto.setMembers(Arrays.asList("Member 1", "Member 2"));

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "There is already a topic assigned to the author with that name");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(topicDto.getUser(), topicDto.getToken())).thenReturn(true);
        when(topicsService.existsByTitleAndAuthor(topicDto.getTitle().strip(), topicDto.getUser())).thenReturn(true);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.createTopic(topicDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(topicDto.getUser(), topicDto.getToken());
        verify(topicsService, times(1)).existsByTitleAndAuthor(topicDto.getTitle().strip(), topicDto.getUser());
        verifyNoMoreInteractions(usersService, topicsService);
    }

    @Test
    void testCreateTopic_InvalidTopicType_ReturnsBadRequest() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setTitle("Topic 1");
        topicDto.setType("InvalidType");
        topicDto.setQuestion("Question 1");
        topicDto.setOptions(Arrays.asList("Option 1", "Option 2"));
        topicDto.setUser("exampleUser");
        topicDto.setMembers(Arrays.asList("Member 1", "Member 2"));

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "The topic type is not valid");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.BAD_GATEWAY);

        when(usersService.checkToken(topicDto.getUser(), topicDto.getToken())).thenReturn(true);
        when(topicsService.existsByTitleAndAuthor(topicDto.getTitle().strip(), topicDto.getUser())).thenReturn(false);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.createTopic(topicDto);

        assertEquals(HttpStatus.BAD_GATEWAY, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(topicDto.getUser(), topicDto.getToken());
        verify(topicsService, times(1)).existsByTitleAndAuthor(topicDto.getTitle().strip(), topicDto.getUser());
        verifyNoMoreInteractions(usersService, topicsService);
    }

    @Test
    void testCreateTopic_InternalServerError_ReturnsInternalServerError() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setTitle("Title");
        topicDto.setType(String.valueOf(Constants.TopicType.TEXT_SINGLE));
        topicDto.setQuestion("Question 1");
        topicDto.setOptions(List.of("[\"Option 1\", \"Option 2\"]"));
        topicDto.setUser("exampleUser");
        topicDto.setMembers(List.of("[\"User 1\", \"User 2\"]"));

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "An error occurred --> java.lang.NullPointerException: The topic type is not valid");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.INTERNAL_SERVER_ERROR);

        when(usersService.checkToken(topicDto.getUser(), topicDto.getToken())).thenReturn(true);
        when(topicsService.existsByTitleAndAuthor(topicDto.getTitle().strip(), topicDto.getUser())).thenReturn(false);
        when(topicsService.initiateVoting(topicDto.getOptions())).thenReturn("[\"Option 1\", \"Option 2\"]");
        doThrow(new NullPointerException("The topic type is not valid")).when(mailService).sendEmail(topicDto);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.createTopic(topicDto);

        assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(topicDto.getUser(), topicDto.getToken());
        verify(topicsService, times(1)).existsByTitleAndAuthor(topicDto.getTitle().strip(), topicDto.getUser());
        verify(topicsService, times(1)).initiateVoting(topicDto.getOptions());
        verifyNoMoreInteractions(usersService, topicsService);
    }

    @Test
    void testCreateTopic_ValidInput_ReturnsOk() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setTitle("Topic 1");
        topicDto.setType(String.valueOf(Constants.TopicType.TEXT_SINGLE));
        topicDto.setQuestion("Question 1");
        topicDto.setOptions(List.of("{\"Option 1\": 1, \"Option 2\": 2}"));
        topicDto.setUser("exampleUser");
        topicDto.setMembers(List.of("[\"User 1\", \"User 2\"]"));

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "Topic created successfully");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(topicDto.getUser(), topicDto.getToken())).thenReturn(true);
        when(topicsService.existsByTitleAndAuthor(topicDto.getTitle().strip(), topicDto.getUser())).thenReturn(false);
        when(topicsService.initiateVoting(topicDto.getOptions())).thenReturn("[\"Option 1\", \"Option 2\"]");
        doNothing().when(topicsService).saveTopic(any(TopicsEntity.class));
        doNothing().when(mailService).sendEmail(topicDto);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.createTopic(topicDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(topicDto.getUser(), topicDto.getToken());
        verify(topicsService, times(1)).existsByTitleAndAuthor(topicDto.getTitle().strip(), topicDto.getUser());
        verify(topicsService, times(1)).initiateVoting(topicDto.getOptions());
        verify(topicsService, times(1)).saveTopic(any(TopicsEntity.class));
        verify(mailService, times(1)).sendEmail(topicDto);
        verifyNoMoreInteractions(usersService, topicsService, mailService);
    }

    @Test
    void testEditTopic_TopicNotFound_ReturnsTopicNotFound() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(1);
        topicDto.setTitle("Topic 1");
        topicDto.setType("Type 1");
        topicDto.setQuestion("Question 1");
        topicDto.setOptions(Arrays.asList("Option 1", "Option 2"));
        topicDto.setAuthor("Author 1");
        topicDto.setMembers(Arrays.asList("Member 1", "Member 2"));
        topicDto.setUser("exampleUser");
        topicDto.setToken("validToken");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "There is no topic with that id");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(topicDto.getUser(), topicDto.getToken())).thenReturn(true);
        when(topicsService.existsById(topicDto.getId())).thenReturn(false);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.editTopic(topicDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(topicDto.getUser(), topicDto.getToken());
        verify(topicsService, times(1)).existsById(topicDto.getId());
        verifyNoMoreInteractions(usersService, topicsService);
    }

    @Test
    void testEditTopic_AllDataRequired_ReturnsBadGateway() {
        TopicsDto topicDto = new TopicsDto();

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "All data is required to edit a topic");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.BAD_GATEWAY);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.editTopic(topicDto);

        assertEquals(HttpStatus.BAD_GATEWAY, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verifyNoMoreInteractions(usersService, topicsService, mailService);
    }

    @Test
    void testEditTopic_UnauthorizedUser_ReturnsNotFound() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(1);
        topicDto.setTitle("Topic 1");
        topicDto.setType(String.valueOf(Constants.TopicType.TEXT_SINGLE));
        topicDto.setQuestion("Question 1");
        topicDto.setOptions(List.of("{\"Option 1\": 1, \"Option 2\": 2}"));
        topicDto.setMembers(List.of("[\"User 1\", \"User 2\"]"));
        topicDto.setUser("Author 1");
        topicDto.setToken("validToken");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "Unauthorized user");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.NOT_FOUND);

        when(usersService.checkToken(topicDto.getUser(), topicDto.getToken())).thenReturn(false);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.editTopic(topicDto);

        assertEquals(HttpStatus.NOT_FOUND, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(topicDto.getUser(), topicDto.getToken());
        verifyNoMoreInteractions(usersService, topicsService, mailService);
    }

    @Test
    void testEditTopic_UnauthorizedAuthor_ReturnsOk() {
        TopicsEntity topicEntity = new TopicsEntity();
        topicEntity.setId(1);
        topicEntity.setTitle("Topic 2");
        topicEntity.setType(String.valueOf(Constants.TopicType.TEXT_SINGLE));
        topicEntity.setQuestion("Question 1");
        topicEntity.setOptions("{\"Option 1\": 1, \"Option 2\": 2}");
        topicEntity.setAuthor("Author 1");
        topicEntity.setMembers("[\"User 1\", \"User 2\"]");
        topicEntity.setCloseDate(null);
        topicEntity.setVisits(0);
        topicEntity.setStatus(Constants.STATUS_OPENED);

        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(1);
        topicDto.setTitle("Topic 1");
        topicDto.setType(String.valueOf(Constants.TopicType.TEXT_SINGLE));
        topicDto.setQuestion("Question 1");
        topicDto.setOptions(List.of("{\"Option 1\": 1, \"Option 2\": 2}"));
        topicDto.setMembers(List.of("[\"User 1\", \"User 2\"]"));
        topicDto.setUser("Author 2");
        topicDto.setToken("validToken");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "The user is not the author of the topic");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(topicDto.getUser(), topicDto.getToken())).thenReturn(true);
        when(topicsService.existsById(topicDto.getId())).thenReturn(true);
        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(topicEntity);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.editTopic(topicDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(topicDto.getUser(), topicDto.getToken());
        verify(topicsService, times(1)).existsById(topicDto.getId());
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verifyNoMoreInteractions(usersService, topicsService, mailService);
    }

    @Test
    void testEditTopic_ClosedTopic_ReturnsOk() {
        TopicsEntity topicEntity = new TopicsEntity();
        topicEntity.setId(1);
        topicEntity.setTitle("Topic 2");
        topicEntity.setType(String.valueOf(Constants.TopicType.TEXT_SINGLE));
        topicEntity.setQuestion("Question 1");
        topicEntity.setOptions("{\"Option 1\": 1, \"Option 2\": 2}");
        topicEntity.setAuthor("Author 1");
        topicEntity.setMembers("[\"User 1\", \"User 2\"]");
        topicEntity.setCloseDate(null);
        topicEntity.setVisits(0);
        topicEntity.setStatus(Constants.STATUS_CLOSED);

        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(1);
        topicDto.setTitle("Topic 1");
        topicDto.setType(String.valueOf(Constants.TopicType.TEXT_SINGLE));
        topicDto.setQuestion("Question 1");
        topicDto.setOptions(List.of("{\"Option 1\": 1, \"Option 2\": 2}"));
        topicDto.setMembers(List.of("[\"User 1\", \"User 2\"]"));
        topicDto.setUser("Author 1");
        topicDto.setToken("validToken");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "The topic is closed");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(topicDto.getUser(), topicDto.getToken())).thenReturn(true);
        when(topicsService.existsById(topicDto.getId())).thenReturn(true);
        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(topicEntity);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.editTopic(topicDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(topicDto.getUser(), topicDto.getToken());
        verify(topicsService, times(1)).existsById(topicDto.getId());
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verifyNoMoreInteractions(usersService, topicsService, mailService);
    }

    @Test
    void testEditTopic_DuplicateTopic_ReturnsOk() {
        TopicsEntity topicEntity = new TopicsEntity();
        topicEntity.setId(1);
        topicEntity.setTitle("Topic 1");
        topicEntity.setType(String.valueOf(Constants.TopicType.TEXT_SINGLE));
        topicEntity.setQuestion("Question 1");
        topicEntity.setOptions("{\"Option 1\": 1, \"Option 2\": 2}");
        topicEntity.setAuthor("Author 1");
        topicEntity.setMembers("[\"User 1\", \"User 2\"]");
        topicEntity.setCloseDate(null);
        topicEntity.setVisits(0);
        topicEntity.setStatus(Constants.STATUS_OPENED);

        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(1);
        topicDto.setTitle("Topic 1");
        topicDto.setType(String.valueOf(Constants.TopicType.TEXT_SINGLE));
        topicDto.setQuestion("Question 1");
        topicDto.setOptions(List.of("{\"Option 1\": 1, \"Option 2\": 2}"));
        topicDto.setMembers(List.of("[\"User 1\", \"User 2\"]"));
        topicDto.setUser("Author 1");
        topicDto.setToken("validToken");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "There is already a topic assigned to the author with that name");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(topicDto.getUser(), topicDto.getToken())).thenReturn(true);
        when(topicsService.existsById(topicDto.getId())).thenReturn(true);
        when(topicsService.existsByTitleAndAuthor(topicDto.getTitle().strip(), topicDto.getUser())).thenReturn(true);
        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(topicEntity);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.editTopic(topicDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(topicDto.getUser(), topicDto.getToken());
        verify(topicsService, times(1)).existsById(topicDto.getId());
        verify(topicsService, times(1)).existsByTitleAndAuthor(topicDto.getTitle().strip(), topicDto.getUser());
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verifyNoMoreInteractions(usersService, topicsService, mailService);
    }

    @Test
    void testEditTopic_InvalidTopicType_ReturnsBadGateway() {
        TopicsEntity topicEntity = new TopicsEntity();
        topicEntity.setId(1);
        topicEntity.setTitle("Topic 2");
        topicEntity.setType(String.valueOf(Constants.TopicType.TEXT_SINGLE));
        topicEntity.setQuestion("Question 1");
        topicEntity.setOptions("{\"Option 1\": 1, \"Option 2\": 2}");
        topicEntity.setAuthor("Author 1");
        topicEntity.setMembers("[\"User 1\", \"User 2\"]");
        topicEntity.setCloseDate(null);
        topicEntity.setVisits(0);
        topicEntity.setStatus(Constants.STATUS_OPENED);

        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(1);
        topicDto.setTitle("Topic 1");
        topicDto.setType("Type");
        topicDto.setQuestion("Question 1");
        topicDto.setOptions(List.of("{\"Option 1\": 1, \"Option 2\": 2}"));
        topicDto.setMembers(List.of("[\"User 1\", \"User 2\"]"));
        topicDto.setUser("Author 1");
        topicDto.setToken("validToken");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "The topic type is not valid");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(topicDto.getUser(), topicDto.getToken())).thenReturn(true);
        when(topicsService.existsById(topicDto.getId())).thenReturn(true);
        when(topicsService.existsByTitleAndAuthor(topicDto.getTitle().strip(), topicDto.getUser())).thenReturn(false);
        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(topicEntity);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.editTopic(topicDto);

        assertEquals(HttpStatus.BAD_GATEWAY, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(topicDto.getUser(), topicDto.getToken());
        verify(topicsService, times(1)).existsById(topicDto.getId());
        verify(topicsService, times(1)).existsByTitleAndAuthor(topicDto.getTitle().strip(), topicDto.getUser());
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verifyNoMoreInteractions(usersService, topicsService, mailService);
    }

    @Test
    void testEditTopic_AllDataProvided_TopicEditedSuccessfully() {
        TopicsEntity topicEntity = new TopicsEntity();
        topicEntity.setId(1);
        topicEntity.setTitle("Topic 2");
        topicEntity.setType(String.valueOf(Constants.TopicType.TEXT_SINGLE));
        topicEntity.setQuestion("Question 1");
        topicEntity.setOptions("{\"Option 1\": 1, \"Option 2\": 2}");
        topicEntity.setAuthor("Author 1");
        topicEntity.setMembers("[\"User 1\", \"User 2\"]");
        topicEntity.setCloseDate(null);
        topicEntity.setVisits(0);
        topicEntity.setStatus(Constants.STATUS_OPENED);

        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(1);
        topicDto.setTitle("Topic 1");
        topicDto.setType(String.valueOf(Constants.TopicType.TEXT_SINGLE));
        topicDto.setQuestion("Question 1");
        topicDto.setOptions(List.of("{\"Option 1\": 1, \"Option 2\": 2}"));
        topicDto.setMembers(List.of("[\"User 1\", \"User 2\"]"));
        topicDto.setUser("Author 1");
        topicDto.setToken("validToken");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "Topic edited successfully");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(topicDto.getUser(), topicDto.getToken())).thenReturn(true);
        when(topicsService.existsById(topicDto.getId())).thenReturn(true);
        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(topicEntity);
        when(topicsService.existsByTitleAndAuthor(topicDto.getTitle().strip(), topicDto.getUser())).thenReturn(false);
        when(topicsService.initiateVoting(topicDto.getOptions())).thenReturn("{\"Option 1\": 1, \"Option 2\": 2}");
        doNothing().when(topicsService).saveTopic(any(TopicsEntity.class));

        ResponseEntity<SpecialResponse> actualResponse = topicsController.editTopic(topicDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(topicDto.getUser(), topicDto.getToken());
        verify(topicsService, times(1)).existsById(topicDto.getId());
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verify(topicsService, times(1)).existsByTitleAndAuthor(topicDto.getTitle().strip(), topicDto.getUser());
        verify(topicsService, times(1)).initiateVoting(topicDto.getOptions());
        verify(topicsService, times(1)).saveTopic(topicEntity);
        verifyNoMoreInteractions(usersService, topicsService);
    }

    @Test
    void testCloseTopic_UnauthorizedUser() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(1);
        topicDto.setUser("exampleUser");
        topicDto.setToken("validToken");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "Unauthorized user");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.NOT_FOUND);

        when(usersService.checkToken(topicDto.getUser(), topicDto.getToken())).thenReturn(false);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.closeTopic(topicDto);

        assertEquals(HttpStatus.NOT_FOUND, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(topicDto.getUser(), topicDto.getToken());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testCloseTopic_TopicNotFound_ReturnsTopicNotFound() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(1);
        topicDto.setUser("exampleUser");
        topicDto.setToken("validToken");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "There is no topic with that id");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(topicDto.getUser(), topicDto.getToken())).thenReturn(true);
        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(null);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.closeTopic(topicDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(topicDto.getUser(), topicDto.getToken());
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verifyNoMoreInteractions(usersService, topicsService);
    }

    @Test
    void testCloseTopic_UserNotAuthor_ReturnsUserNotAuthor() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(1);
        topicDto.setUser("exampleUser");
        topicDto.setToken("validToken");

        TopicsEntity topicEntity = new TopicsEntity();
        topicEntity.setId(topicDto.getId());
        topicEntity.setAuthor("differentUser");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "The user is not the author of the topic");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(topicDto.getUser(), topicDto.getToken())).thenReturn(true);
        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(topicEntity);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.closeTopic(topicDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(topicDto.getUser(), topicDto.getToken());
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verifyNoMoreInteractions(usersService, topicsService);
    }

    @Test
    void testCloseTopic_TopicAlreadyClosed_ReturnsTopicClosed() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(1);
        topicDto.setUser("exampleUser");
        topicDto.setToken("validToken");

        TopicsEntity topicEntity = new TopicsEntity();
        topicEntity.setId(topicDto.getId());
        topicEntity.setAuthor(topicDto.getUser());
        topicEntity.setStatus(Constants.STATUS_CLOSED);

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "The topic is currently closed");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(topicDto.getUser(), topicDto.getToken())).thenReturn(true);
        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(topicEntity);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.closeTopic(topicDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(topicDto.getUser(), topicDto.getToken());
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verifyNoMoreInteractions(usersService, topicsService);
    }

    @Test
    void testCloseTopic_TopicClosedSuccessfully() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(1);
        topicDto.setUser("exampleUser");
        topicDto.setToken("validToken");

        TopicsEntity topicEntity = new TopicsEntity();
        topicEntity.setId(topicDto.getId());
        topicEntity.setAuthor(topicDto.getUser());
        topicEntity.setStatus(Constants.STATUS_OPENED);

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "The topic has been closed");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(topicDto.getUser(), topicDto.getToken())).thenReturn(true);
        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(topicEntity);
        doNothing().when(topicsService).saveTopic(any(TopicsEntity.class));

        ResponseEntity<SpecialResponse> actualResponse = topicsController.closeTopic(topicDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(topicDto.getUser(), topicDto.getToken());
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verify(topicsService, times(1)).saveTopic(topicEntity);
        verifyNoMoreInteractions(usersService, topicsService);
    }

    @Test
    void testReOpenTopic_UnauthorizedUser() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(1);
        topicDto.setUser("exampleUser");
        topicDto.setToken("validToken");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "Unauthorized user");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.NOT_FOUND);

        when(usersService.checkToken(topicDto.getUser(), topicDto.getToken())).thenReturn(false);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.reOpenTopic(topicDto);

        assertEquals(HttpStatus.NOT_FOUND, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(topicDto.getUser(), topicDto.getToken());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testReOpenTopic_TopicNotFound_ReturnsTopicNotFound() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(1);
        topicDto.setUser("exampleUser");
        topicDto.setToken("validToken");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "There is no topic with that id");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(topicDto.getUser(), topicDto.getToken())).thenReturn(true);
        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(null);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.reOpenTopic(topicDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(topicDto.getUser(), topicDto.getToken());
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verifyNoMoreInteractions(usersService, topicsService);
    }

    @Test
    void testReOpenTopic_UserNotAuthor_ReturnsUserNotAuthor() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(1);
        topicDto.setUser("exampleUser");
        topicDto.setToken("validToken");

        TopicsEntity topicEntity = new TopicsEntity();
        topicEntity.setId(topicDto.getId());
        topicEntity.setAuthor("differentUser");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "The user is not the author of the topic");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(topicDto.getUser(), topicDto.getToken())).thenReturn(true);
        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(topicEntity);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.reOpenTopic(topicDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(topicDto.getUser(), topicDto.getToken());
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verifyNoMoreInteractions(usersService, topicsService);
    }

    @Test
    void testReOpenTopic_TopicAlreadyClosed_ReturnsTopicClosed() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(1);
        topicDto.setUser("exampleUser");
        topicDto.setToken("validToken");

        TopicsEntity topicEntity = new TopicsEntity();
        topicEntity.setId(topicDto.getId());
        topicEntity.setAuthor(topicDto.getUser());
        topicEntity.setStatus(Constants.STATUS_OPENED);

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "The topic is currently open");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(topicDto.getUser(), topicDto.getToken())).thenReturn(true);
        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(topicEntity);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.reOpenTopic(topicDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(topicDto.getUser(), topicDto.getToken());
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verifyNoMoreInteractions(usersService, topicsService);
    }

    @Test
    void testReOpenTopic_TopicClosedSuccessfully() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(1);
        topicDto.setUser("exampleUser");
        topicDto.setToken("validToken");

        TopicsEntity topicEntity = new TopicsEntity();
        topicEntity.setId(topicDto.getId());
        topicEntity.setAuthor(topicDto.getUser());
        topicEntity.setStatus(Constants.STATUS_CLOSED);

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "Topic reopened");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(topicDto.getUser(), topicDto.getToken())).thenReturn(true);
        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(topicEntity);
        doNothing().when(topicsService).saveTopic(any(TopicsEntity.class));

        ResponseEntity<SpecialResponse> actualResponse = topicsController.reOpenTopic(topicDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(topicDto.getUser(), topicDto.getToken());
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verify(topicsService, times(1)).saveTopic(topicEntity);
        verifyNoMoreInteractions(usersService, topicsService);
    }

    @Test
    void testDeleteTopic_TopicNotFound_ReturnsTopicNotFound() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(1);
        topicDto.setUser("exampleUser");
        topicDto.setToken("validToken");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "There is no topic with that id");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(topicDto.getUser(), topicDto.getToken())).thenReturn(true);
        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(null);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.deleteTopic(topicDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(topicDto.getUser(), topicDto.getToken());
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verifyNoMoreInteractions(usersService, topicsService);
    }

    @Test
    void testDeleteTopic_UserNotAuthor_ReturnsUserNotAuthor() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(1);
        topicDto.setUser("exampleUser");
        topicDto.setToken("validToken");

        TopicsEntity topicEntity = new TopicsEntity();
        topicEntity.setId(topicDto.getId());
        topicEntity.setAuthor("differentUser");

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "The user is not the author of the topic");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(topicDto.getUser(), topicDto.getToken())).thenReturn(true);
        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(topicEntity);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.deleteTopic(topicDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(topicDto.getUser(), topicDto.getToken());
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verifyNoMoreInteractions(usersService, topicsService);
    }

    @Test
    void testDeleteTopic_TopicDeletedSuccessfully() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(1);
        topicDto.setUser("exampleUser");
        topicDto.setToken("validToken");

        TopicsEntity topicEntity = new TopicsEntity();
        topicEntity.setId(topicDto.getId());
        topicEntity.setAuthor(topicDto.getUser());

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "The topic has been deleted");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(topicDto.getUser(), topicDto.getToken())).thenReturn(true);
        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(topicEntity);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.deleteTopic(topicDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(topicDto.getUser(), topicDto.getToken());
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verify(topicsService, times(1)).deleteTopic(topicDto.getId());
        verifyNoMoreInteractions(usersService, topicsService);
    }

    @Test
    void testGetAllTopicsData_NoTopics_ReturnsNoTopicsInDatabase() {
        List<TopicsEntity> topicsList = new ArrayList<>();

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "There are no topics in database");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(topicsService.getAllTopicsData()).thenReturn(topicsList);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.getAllTopicsData();

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(topicsService, times(1)).getAllTopicsData();
        verifyNoMoreInteractions(topicsService);
    }

    @Test
    void testGetAllTopicsData_TopicsExist_ReturnsTopicsData() {
        List<TopicsEntity> topicsList = new ArrayList<>();
        TopicsEntity topicEntity1 = new TopicsEntity();
        topicEntity1.setId(1);
        topicEntity1.setTitle("Topic 1");
        topicEntity1.setType("Type 1");
        topicEntity1.setQuestion("Question 1");
        topicEntity1.setOptions("{\"Option 1\": 1, \"Option 2\": 2}");
        topicEntity1.setVotedBy("User 1");
        topicEntity1.setAuthor("Author 1");
        topicEntity1.setMembers("[\"Member 1\", \"Member 2\"]");
        topicEntity1.setVisits(10);
        topicEntity1.setStatus("Open");
        topicsList.add(topicEntity1);

        TopicsEntity topicEntity2 = new TopicsEntity();
        topicEntity2.setId(2);
        topicEntity2.setTitle("Topic 2");
        topicEntity2.setType("Type 2");
        topicEntity2.setQuestion("Question 2");
        topicEntity2.setOptions("{\"Option 3\": 3, \"Option 4\": 4}");
        topicEntity2.setVotedBy("User 2");
        topicEntity2.setAuthor("Author 2");
        topicEntity2.setMembers("[\"Member 3\", \"Member 4\"]");
        topicEntity2.setVisits(5);
        topicEntity2.setStatus("Closed");
        topicsList.add(topicEntity2);

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "OK");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(topicsList, expectedResponseJson), HttpStatus.OK);

        when(topicsService.getAllTopicsData()).thenReturn(topicsList);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.getAllTopicsData();

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());

        List<TopicsDto> actualTopicsDtoList = (List<TopicsDto>) Objects.requireNonNull(actualResponse.getBody()).getEntity();
        assertEquals(topicsList.size(), actualTopicsDtoList.size());
        for (int i = 0; i < topicsList.size(); i++) {
            TopicsEntity topicEntity = topicsList.get(i);
            TopicsDto topicsDto = actualTopicsDtoList.get(i);
            assertEquals(topicEntity.getId(), topicsDto.getId());
            assertEquals(topicEntity.getTitle(), topicsDto.getTitle());
            assertEquals(topicEntity.getType(), topicsDto.getType());
            assertEquals(topicEntity.getQuestion(), topicsDto.getQuestion());
            Map<String, Integer> expectedOptionsMap = new Gson().fromJson(topicEntity.getOptions(), new TypeToken<Map<String, Integer>>() {}.getType());
            assertEquals(expectedOptionsMap, topicsDto.getOptionsMap());
            assertEquals(topicEntity.getVotedBy(), topicsDto.getVotedBy());
            assertEquals(topicEntity.getAuthor(), topicsDto.getAuthor());
            assertEquals(new Gson().fromJson(topicEntity.getMembers(), new TypeToken<List<String>>() {}.getType()), topicsDto.getMembers());
            assertEquals(topicEntity.getVisits(), topicsDto.getVisits());
            assertEquals(topicEntity.getStatus(), topicsDto.getStatus());
        }
        verify(topicsService, times(1)).getAllTopicsData();
        verifyNoMoreInteractions(topicsService);
    }

    @Test
    void testVote_UserUnauthorized_ReturnsUnauthorizedUser() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setUser("User 1");
        topicDto.setToken("invalidToken");
        topicDto.setId(1);
        topicDto.setVotation(Arrays.asList("Option 1"));

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "Unauthorized user");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.NOT_FOUND);

        when(usersService.checkToken(topicDto.getUser(), topicDto.getToken())).thenReturn(false);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.vote(topicDto);

        assertEquals(HttpStatus.NOT_FOUND, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(topicDto.getUser(), topicDto.getToken());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testVote_TopicNotFound_ReturnsNoTopicWithThatId() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setUser("User 1");
        topicDto.setToken("validToken");
        topicDto.setId(1);
        topicDto.setVotation(Arrays.asList("Option 1"));

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "There is no topic with that id");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(topicDto.getUser(), topicDto.getToken())).thenReturn(true);
        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(null);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.vote(topicDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(topicDto.getUser(), topicDto.getToken());
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verifyNoMoreInteractions(usersService, topicsService);
    }

    @Test
    void testVote_EmptyVotation_ReturnsVotingCannotBeEmpty() {
        TopicsEntity topicEntity = new TopicsEntity();
        topicEntity.setId(1);
        topicEntity.setOptions("{\"Option 1\": 1, \"Option 2\": 2}");
        topicEntity.setVotedBy(null);
        topicEntity.setMembers("[\"Member 1\", \"Member 2\"]");
        topicEntity.setAuthor("Author 1");

        TopicsDto topicDto = new TopicsDto();
        topicDto.setUser("User 1");
        topicDto.setToken("validToken");
        topicDto.setId(1);
        topicDto.setVotation(Collections.emptyList());

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "The voting cannot be empty");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.BAD_GATEWAY);

        when(usersService.checkToken(topicDto.getUser(), topicDto.getToken())).thenReturn(true);
        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(topicEntity);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.vote(topicDto);

        assertEquals(HttpStatus.BAD_GATEWAY, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(topicDto.getUser(), topicDto.getToken());
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verifyNoMoreInteractions(usersService, topicsService);
    }

    @Test
    void testVote_UserNotAllowedToVote_ReturnsUserNotAllowedToVote() {
        TopicsEntity topicEntity = new TopicsEntity();
        topicEntity.setId(1);
        topicEntity.setOptions("{\"Option 1\": 1, \"Option 2\": 2}");
        topicEntity.setVotedBy(null);
        topicEntity.setMembers("[\"Member 1\", \"Member 2\"]");
        topicEntity.setAuthor("Author 1");

        TopicsDto topicDto = new TopicsDto();
        topicDto.setUser("User 3");
        topicDto.setToken("validToken");
        topicDto.setId(1);
        topicDto.setVotation(List.of("Option 1"));

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "The user is not allowed to vote on this topic");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(topicDto.getUser(), topicDto.getToken())).thenReturn(true);
        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(topicEntity);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.vote(topicDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(topicDto.getUser(), topicDto.getToken());
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verifyNoMoreInteractions(usersService, topicsService);
    }

    @Test
    void testVote_UserAlreadyVoted_ReturnsUserAlreadyVoted() {
        TopicsEntity topicEntity = new TopicsEntity();
        topicEntity.setId(1);
        topicEntity.setType(String.valueOf(Constants.TopicType.TEXT_MULTIPLE));
        topicEntity.setOptions("{\"Option 1\": 1, \"Option 2\": 2}");
        topicEntity.setVotedBy("User 1");
        topicEntity.setMembers("[\"User 1\", \"User 2\"]");
        topicEntity.setAuthor("Author 1");

        TopicsDto topicDto = new TopicsDto();
        topicDto.setUser("User 1");
        topicDto.setToken("validToken");
        topicDto.setId(1);
        topicDto.setVotation(List.of("Option 1"));

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "The user has already voted");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(topicDto.getUser(), topicDto.getToken())).thenReturn(true);
        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(topicEntity);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.vote(topicDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(topicDto.getUser(), topicDto.getToken());
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verifyNoMoreInteractions(usersService, topicsService);
    }

    @Test
    void testVote_InvalidTopicTypeForMultipleVotingOptions_ReturnsTopicTypeNotValid() {
        TopicsEntity topicEntity = new TopicsEntity();
        topicEntity.setId(1);
        topicEntity.setType(String.valueOf(Constants.TopicType.TEXT_SINGLE));
        topicEntity.setOptions("{\"Option 1\": 1, \"Option 2\": 2}");
        topicEntity.setVotedBy(null);
        topicEntity.setMembers("[\"User 1\", \"User 2\"]");
        topicEntity.setAuthor("Author 1");

        TopicsDto topicDto = new TopicsDto();
        topicDto.setUser("User 1");
        topicDto.setToken("validToken");
        topicDto.setId(1);
        topicDto.setVotation(Arrays.asList("Option 1", "Option 2"));

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "The topic type is not valid for multiple voting options");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.BAD_GATEWAY);

        when(usersService.checkToken(topicDto.getUser(), topicDto.getToken())).thenReturn(true);
        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(topicEntity);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.vote(topicDto);

        assertEquals(HttpStatus.BAD_GATEWAY, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(topicDto.getUser(), topicDto.getToken());
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verifyNoMoreInteractions(usersService, topicsService);
    }

    @Test
    void testVote_InvalidVotesList_ReturnsListVotesDoesNotMatchOptions() {
        TopicsEntity topicEntity = new TopicsEntity();
        topicEntity.setId(1);
        topicEntity.setType(String.valueOf(Constants.TopicType.TEXT_MULTIPLE));
        topicEntity.setOptions("{\"Option 1\": 1, \"Option 2\": 2}");
        topicEntity.setVotedBy(null);
        topicEntity.setMembers("[\"User 1\", \"User 2\"]");
        topicEntity.setAuthor("Author 1");

        TopicsDto topicDto = new TopicsDto();
        topicDto.setUser("User 1");
        topicDto.setToken("validToken");
        topicDto.setId(1);
        topicDto.setVotation(Arrays.asList("Option 3", "Option 4"));

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "The list of votes does not match the options of the topic");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.BAD_GATEWAY);

        when(usersService.checkToken(topicDto.getUser(), topicDto.getToken())).thenReturn(true);
        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(topicEntity);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.vote(topicDto);

        assertEquals(HttpStatus.BAD_GATEWAY, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(topicDto.getUser(), topicDto.getToken());
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verifyNoMoreInteractions(usersService, topicsService);
    }

    @Test
    void testVote_UserAuthorized_ValidVote_ReturnsVotationUpdatedSuccessfully() {
        TopicsEntity topicEntity = new TopicsEntity();
        topicEntity.setId(1);
        topicEntity.setType(String.valueOf(Constants.TopicType.TEXT_MULTIPLE));
        topicEntity.setOptions("{\"Option 1\": 1, \"Option 2\": 2}");
        topicEntity.setVotedBy(null);
        topicEntity.setMembers("[\"User 1\", \"User 2\"]");
        topicEntity.setAuthor("Author 1");

        TopicsDto topicDto = new TopicsDto();
        topicDto.setUser("User 1");
        topicDto.setToken("validToken");
        topicDto.setId(1);
        topicDto.setVotation(List.of("Option 1"));

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "Votation updated successfully");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(topicDto.getUser(), topicDto.getToken())).thenReturn(true);
        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(topicEntity);
        doNothing().when(topicsService).saveTopic(any(TopicsEntity.class));

        ResponseEntity<SpecialResponse> actualResponse = topicsController.vote(topicDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(topicDto.getUser(), topicDto.getToken());
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verify(topicsService, times(1)).saveTopic(topicEntity);
        verifyNoMoreInteractions(usersService, topicsService);
    }

    @Test
    void testVotingResults_UserUnauthorized_ReturnsUnauthorizedUser() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setUser("User 1");
        topicDto.setToken("invalidToken");
        topicDto.setId(1);

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "Unauthorized user");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.NOT_FOUND);

        when(usersService.checkToken(topicDto.getUser(), topicDto.getToken())).thenReturn(false);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.votingResults(topicDto);

        assertEquals(HttpStatus.NOT_FOUND, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(topicDto.getUser(), topicDto.getToken());
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testVotingResults_TopicNotFound_ReturnsNoTopicWithThatId() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setUser("User 1");
        topicDto.setToken("validToken");
        topicDto.setId(1);

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "There is no topic with that id");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(topicDto.getUser(), topicDto.getToken())).thenReturn(true);
        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(null);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.votingResults(topicDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(topicDto.getUser(), topicDto.getToken());
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verifyNoMoreInteractions(usersService, topicsService);
    }

    @Test
    void testVotingResults_UserNotAllowedToViewResults_ReturnsUserNotAllowedToViewResults() {
        TopicsEntity topicEntity = new TopicsEntity();
        topicEntity.setId(1);
        topicEntity.setStatus(Constants.STATUS_CLOSED);
        topicEntity.setType("Type 1");
        topicEntity.setOptions("{\"Option 1\": 1, \"Option 2\": 2}");
        topicEntity.setMembers("[\"Member 2\", \"Member 3\"]");
        topicEntity.setAuthor("Author 1");

        TopicsDto topicDto = new TopicsDto();
        topicDto.setUser("User 1");
        topicDto.setToken("validToken");
        topicDto.setId(1);

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "The user is not allowed to view the results on this topic");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(topicDto.getUser(), topicDto.getToken())).thenReturn(true);
        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(topicEntity);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.votingResults(topicDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(topicDto.getUser(), topicDto.getToken());
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verifyNoMoreInteractions(usersService, topicsService);
    }

    @Test
    void testVotingResults_TopicNotClosed_ReturnsTopicNotClosed() {
        TopicsEntity topicEntity = new TopicsEntity();
        topicEntity.setId(1);
        topicEntity.setStatus(Constants.STATUS_OPENED);
        topicEntity.setType("Type 1");
        topicEntity.setOptions("{\"Option 1\": 1, \"Option 2\": 2}");
        topicEntity.setMembers("[\"Member 1\", \"Member 2\"]");
        topicEntity.setAuthor("Author 1");

        TopicsDto topicDto = new TopicsDto();
        topicDto.setUser("User 1");
        topicDto.setToken("validToken");
        topicDto.setId(1);

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "The topic is not closed, so it is not possible to view the results");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(topicDto.getUser(), topicDto.getToken())).thenReturn(true);
        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(topicEntity);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.votingResults(topicDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        verify(usersService, times(1)).checkToken(topicDto.getUser(), topicDto.getToken());
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verifyNoMoreInteractions(usersService, topicsService);
    }

    @Test
    void testVotingResults_UserAuthorized_TopicExists_TopicClosed_ReturnsVotingResults() {
        TopicsEntity topicEntity = new TopicsEntity();
        topicEntity.setId(1);
        topicEntity.setStatus(Constants.STATUS_CLOSED);
        topicEntity.setType("Type 1");
        topicEntity.setOptions("{\"Option 1\": 1, \"Option 2\": 2}");
        topicEntity.setMembers("[\"User 1\", \"User 2\"]");
        topicEntity.setAuthor("Author 1");

        TopicsDto topicDto = new TopicsDto();
        topicDto.setUser("User 1");
        topicDto.setToken("validToken");
        topicDto.setId(1);

        Map<String, Integer> expectedOptionsMap = new HashMap<>();
        expectedOptionsMap.put("Option 1", 1);
        expectedOptionsMap.put("Option 2", 2);

        JSONObject expectedResponseJson = new JSONObject();
        expectedResponseJson.put("message", "Type 1");
        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(expectedOptionsMap, expectedResponseJson), HttpStatus.OK);

        when(usersService.checkToken(topicDto.getUser(), topicDto.getToken())).thenReturn(true);
        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(topicEntity);

        ResponseEntity<SpecialResponse> actualResponse = topicsController.votingResults(topicDto);

        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
        assertEquals(expectedResponse.getBody().getEntity(), actualResponse.getBody().getEntity());
        verify(usersService, times(1)).checkToken(topicDto.getUser(), topicDto.getToken());
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verifyNoMoreInteractions(usersService, topicsService);
    }

}






