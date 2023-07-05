package com.capgemini.beni.ailabar.controller;

import com.capgemini.beni.ailabar.dto.TopicsDto;
import com.capgemini.beni.ailabar.dto.UsersDto;
import com.capgemini.beni.ailabar.entity.TopicsEntity;
import com.capgemini.beni.ailabar.service.MailService;
import com.capgemini.beni.ailabar.service.TopicsService;
import com.capgemini.beni.ailabar.service.UsersService;
import com.capgemini.beni.ailabar.utils.Constants;
import com.capgemini.beni.ailabar.utils.SpecialResponse;
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
    void login_WithValidCredentials_ReturnsOkResponse() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("username");
        userDto.setPassword("password");
        when(topicsService.login(userDto.getUser(), DigestUtils.sha256Hex(userDto.getPassword()))).thenReturn(true);

        ResponseEntity<String> response = topicsController.login(userDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        JSONObject responseJson = new JSONObject(response.getBody());
        assertEquals("Login successful", responseJson.getString("message"));
        verify(topicsService).login(userDto.getUser(), DigestUtils.sha256Hex(userDto.getPassword()));
    }

    @Test
    void login_WithEmptyUserOrPassword_ReturnsBadRequestResponse() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("");
        userDto.setPassword("password");

        ResponseEntity<String> response = topicsController.login(userDto);

        assertEquals(HttpStatus.BAD_GATEWAY, response.getStatusCode());
        JSONObject responseJson = new JSONObject(response.getBody());
        assertEquals("User and password are required to login", responseJson.getString("message"));
        verifyNoInteractions(topicsService);
    }

    @Test
    void login_WithInvalidCredentials_ReturnsUnauthorizedResponse() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("username");
        userDto.setPassword("password");
        when(topicsService.login(userDto.getUser(), DigestUtils.sha256Hex(userDto.getPassword()))).thenReturn(false);

        ResponseEntity<String> response = topicsController.login(userDto);

        assertEquals(HttpStatus.UNAUTHORIZED, response.getStatusCode());
        JSONObject responseJson = new JSONObject(response.getBody());
        assertEquals("Login failed", responseJson.getString("message"));
        verify(topicsService).login(userDto.getUser(), DigestUtils.sha256Hex(userDto.getPassword()));
    }

    @Test
    void testLoadTopics() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("exampleUser");

        List<TopicsEntity> topicsList = new ArrayList<>();
        TopicsEntity topicsEntity = new TopicsEntity();
        topicsEntity.setId(3);
        topicsEntity.setTitle("exampleTitle");
        topicsEntity.setType(String.valueOf(Constants.TopicType.TEXT_SINGLE));
        topicsEntity.setQuestion("exampleQuestion");
        topicsEntity.setOptions("exampleOptions");
        topicsEntity.setAuthor("exampleUser");
        topicsEntity.setMembers("exampleMembers");
        topicsEntity.setVisits(0);
        topicsEntity.setStatus(Constants.STATUS_OPENED);
        topicsList.add(topicsEntity);

        when(topicsService.loadTopics(userDto.getUser())).thenReturn(topicsList);

        ResponseEntity<SpecialResponse> response = topicsController.loadTopics(userDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertNotNull(response.getBody());

        JSONObject responseJson = new JSONObject(response.getBody().getMessage());
        assertEquals("OK", responseJson.getString("message"));

        assertNotNull(topicsList);

        verify(topicsService, times(1)).loadTopics(userDto.getUser());
        verifyNoMoreInteractions(topicsService);
    }

    @Test
    void testLoadTopics_NoTopicsRelatedToUser() {
        UsersDto userDto = new UsersDto();
        userDto.setUser("exampleNoUser");

        List<TopicsEntity> topicsList = new ArrayList<>();
        TopicsEntity topicsEntity = new TopicsEntity();
        topicsEntity.setId(3);
        topicsEntity.setTitle("exampleTitle");
        topicsEntity.setType(String.valueOf(Constants.TopicType.TEXT_SINGLE));
        topicsEntity.setQuestion("exampleQuestion");
        topicsEntity.setOptions("exampleOptions");
        topicsEntity.setAuthor("exampleUser");
        topicsEntity.setMembers("exampleMembers");
        topicsEntity.setVisits(0);
        topicsEntity.setStatus(Constants.STATUS_OPENED);
        topicsList.add(topicsEntity);

        when(topicsService.loadTopics(userDto.getUser())).thenReturn(topicsList);

        ResponseEntity<SpecialResponse> response = topicsController.loadTopics(userDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertNotNull(response.getBody());

        JSONObject responseJson = new JSONObject(response.getBody().getMessage());
        assertEquals("There are no topics related to the user", responseJson.getString("message"));

        Object entity = response.getBody().getEntity();
        assertTrue(entity == null || entity instanceof List<?>);

        if (entity instanceof List<?>) {
            List<?> entityList = (List<?>) entity;
            assertTrue(entityList.isEmpty());
        }

        verify(topicsService, times(1)).loadTopics(userDto.getUser());
        verifyNoMoreInteractions(topicsService);
    }

    @Test
    void testOpenTopic_TopicNotFound() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(1);

        when(topicsService.openTopic(topicDto.getId())).thenReturn(null);

        ResponseEntity<SpecialResponse> response = topicsController.openTopic(topicDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertNotNull(response.getBody());

        JSONObject responseJson = new JSONObject(response.getBody().getMessage());
        assertEquals("There is no topic with that id", responseJson.getString("message"));
        assertNull(response.getBody().getEntity());

        verify(topicsService, times(1)).openTopic(topicDto.getId());
        verifyNoMoreInteractions(topicsService);
    }

    @Test
    void testOpenTopic_TopicOpenedSuccessfully() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(1);

        TopicsEntity topicEntity = new TopicsEntity();
        topicEntity.setId(1);
        topicEntity.setTitle("exampleTitle");
        topicEntity.setType(String.valueOf(Constants.TopicType.TEXT_SINGLE));
        topicEntity.setQuestion("exampleQuestion");
        topicEntity.setOptions("exampleOptions");
        topicEntity.setAuthor("exampleUser");
        topicEntity.setMembers("exampleMembers");
        topicEntity.setVisits(0);
        topicEntity.setStatus(Constants.STATUS_CLOSED);

        when(topicsService.openTopic(topicDto.getId())).thenReturn(topicEntity);
        doAnswer(invocation -> invocation.getArgument(0)).when(topicsService).saveTopic(topicEntity);

        ResponseEntity<SpecialResponse> response = topicsController.openTopic(topicDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertNotNull(response.getBody());

        JSONObject responseJson = new JSONObject(response.getBody().getMessage());
        assertEquals("OK", responseJson.getString("message"));

        TopicsEntity entity = (TopicsEntity) response.getBody().getEntity();
        assertNotNull(entity);
        assertEquals(topicEntity, entity);

        verify(topicsService, times(1)).openTopic(topicDto.getId());
        verify(topicsService, times(1)).saveTopic(topicEntity);
        verifyNoMoreInteractions(topicsService);
    }

    @Test
    void testCreateTopic_AllDataRequiredToEditTopic() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setTitle("");
        topicDto.setType("");
        topicDto.setQuestion("");
        topicDto.setOptions("");
        topicDto.setAuthor("");
        topicDto.setMembers("");

        ResponseEntity<String> response = topicsController.createTopic(topicDto);

        assertEquals(HttpStatus.BAD_GATEWAY, response.getStatusCode());
        JSONObject responseJson = new JSONObject(response.getBody());
        assertEquals("All data is required to edit a topic", responseJson.getString("message"));
        verifyNoInteractions(topicsService);
        verifyNoInteractions(usersService);
        verifyNoInteractions(mailService);
    }

    @Test
    void testCreateTopic_UserDoesNotExist() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setTitle("exampleTitle");
        topicDto.setType("exampleType");
        topicDto.setQuestion("exampleQuestion");
        topicDto.setOptions("exampleOptions");
        topicDto.setAuthor("exampleUser");
        topicDto.setMembers("exampleMembers");

        when(usersService.checkUser(topicDto.getAuthor())).thenReturn(false);

        ResponseEntity<String> response = topicsController.createTopic(topicDto);

        assertEquals(HttpStatus.BAD_GATEWAY, response.getStatusCode());
        JSONObject responseJson = new JSONObject(response.getBody());
        assertEquals("The user does not exist", responseJson.getString("message"));
        verify(usersService, times(1)).checkUser(topicDto.getAuthor());
        verifyNoMoreInteractions(usersService);
        verifyNoInteractions(topicsService);
        verifyNoInteractions(mailService);
    }

    @Test
    void testCreateTopic_TopicAlreadyAssignedToAuthor() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setTitle("exampleTitle");
        topicDto.setType("exampleType");
        topicDto.setQuestion("exampleQuestion");
        topicDto.setOptions("exampleOptions");
        topicDto.setAuthor("exampleUser");
        topicDto.setMembers("exampleMembers");

        when(usersService.checkUser(topicDto.getAuthor())).thenReturn(true);
        when(topicsService.existsByTitleAndAuthor(topicDto.getTitle().strip(), topicDto.getAuthor())).thenReturn(true);

        ResponseEntity<String> response = topicsController.createTopic(topicDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        JSONObject responseJson = new JSONObject(response.getBody());
        assertEquals("There is already a topic assigned to the author with that name", responseJson.getString("message"));
        verify(usersService, times(1)).checkUser(topicDto.getAuthor());
        verify(topicsService, times(1)).existsByTitleAndAuthor(topicDto.getTitle().strip(), topicDto.getAuthor());
        verifyNoMoreInteractions(usersService);
        verifyNoMoreInteractions(topicsService);
        verifyNoInteractions(mailService);
    }

    @Test
    void testCreateTopic_InvalidTopicType() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setTitle("exampleTitle");
        topicDto.setType("invalidType");
        topicDto.setQuestion("exampleQuestion");
        topicDto.setOptions("exampleOptions");
        topicDto.setAuthor("exampleUser");
        topicDto.setMembers("exampleMembers");

        when(usersService.checkUser(topicDto.getAuthor())).thenReturn(true);
        when(topicsService.existsByTitleAndAuthor(topicDto.getTitle().strip(), topicDto.getAuthor())).thenReturn(false);

        ResponseEntity<String> response = topicsController.createTopic(topicDto);

        assertEquals(HttpStatus.BAD_GATEWAY, response.getStatusCode());
        JSONObject responseJson = new JSONObject(response.getBody());
        assertEquals("The topic type is not valid", responseJson.getString("message"));
        verify(usersService, times(1)).checkUser(topicDto.getAuthor());
        verify(topicsService, times(1)).existsByTitleAndAuthor(topicDto.getTitle().strip(), topicDto.getAuthor());
        verifyNoMoreInteractions(usersService);
        verifyNoMoreInteractions(topicsService);
        verifyNoInteractions(mailService);
    }

    @Test
    void testCreateTopic_SuccessfulTopicCreation() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(25);
        topicDto.setTitle("exampleTitle");
        topicDto.setType(String.valueOf(Constants.TopicType.TEXT_SINGLE));
        topicDto.setQuestion("exampleQuestion");
        topicDto.setOptions("exampleOptions");
        topicDto.setAuthor("exampleUser");
        topicDto.setMembers("exampleMembers");
        topicDto.setVisits(0);
        topicDto.setStatus(Constants.STATUS_OPENED);

        TopicsEntity topicEntity = new TopicsEntity(topicDto);

        when(usersService.checkUser(topicDto.getAuthor())).thenReturn(true);
        when(topicsService.existsByTitleAndAuthor(topicDto.getTitle().strip(), topicDto.getAuthor())).thenReturn(false);
        when(topicsService.initiateVoting(topicDto.getOptions())).thenReturn("exampleOptions");
        doNothing().when(mailService).sendEmail(topicDto);
        doNothing().when(topicsService).saveTopic(Mockito.eq(topicEntity));

        ResponseEntity<String> response = topicsController.createTopic(topicDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        JSONObject responseJson = new JSONObject(response.getBody());
        assertEquals("Group saved successfully", responseJson.getString("message"));
        verify(usersService, times(1)).checkUser(topicDto.getAuthor());
        verify(topicsService, times(1)).existsByTitleAndAuthor(topicDto.getTitle().strip(), topicDto.getAuthor());
        verify(topicsService, times(1)).initiateVoting(topicDto.getOptions());
        verify(mailService, times(1)).sendEmail(topicDto);
        verify(topicsService, times(1)).saveTopic(Mockito.eq(topicEntity));
        verifyNoMoreInteractions(usersService);
        verifyNoMoreInteractions(topicsService);
        verifyNoMoreInteractions(mailService);
    }

    @Test
    void testCreateTopic_InternalServerError() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setTitle("exampleTitle");
        topicDto.setType(String.valueOf(Constants.TopicType.TEXT_SINGLE));
        topicDto.setQuestion("exampleQuestion");
        topicDto.setOptions("exampleOptions");
        topicDto.setAuthor("exampleUser");
        topicDto.setMembers("exampleMembers");

        when(usersService.checkUser(topicDto.getAuthor())).thenReturn(true);
        when(topicsService.existsByTitleAndAuthor(topicDto.getTitle().strip(), topicDto.getAuthor())).thenReturn(false);
        when(topicsService.initiateVoting(topicDto.getOptions())).thenThrow(new RuntimeException("Some error occurred"));

        ResponseEntity<String> response = topicsController.createTopic(topicDto);

        assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
        JSONObject responseJson = new JSONObject(response.getBody());
        assertEquals("An error occurred --> java.lang.RuntimeException: Some error occurred", responseJson.getString("message"));
        verify(usersService, times(1)).checkUser(topicDto.getAuthor());
        verify(topicsService, times(1)).existsByTitleAndAuthor(topicDto.getTitle().strip(), topicDto.getAuthor());
        verify(topicsService, times(1)).initiateVoting(topicDto.getOptions());
        verifyNoMoreInteractions(usersService);
        verifyNoMoreInteractions(topicsService);
        verifyNoInteractions(mailService);
    }

    @Test
    void testGetTopicForEdit_InvalidTopicId() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(25);

        when(topicsService.getTopicForEdit(topicDto.getId())).thenReturn(null);

        ResponseEntity<SpecialResponse> response = topicsController.getTopicForEdit(topicDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        JSONObject responseJson = new JSONObject(response.getBody().getMessage());
        assertEquals("There is no topic with that id", responseJson.getString("message"));
        verify(topicsService, times(1)).getTopicForEdit(topicDto.getId());
        verifyNoMoreInteractions(topicsService);
    }

    @Test
    void testGetTopicForEdit_ClosedTopic() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(25);
        topicDto.setUser("exampleUser");

        TopicsEntity topicEntity = new TopicsEntity();
        topicEntity.setStatus(Constants.STATUS_CLOSED);

        when(topicsService.getTopicForEdit(topicDto.getId())).thenReturn(topicEntity);

        ResponseEntity<SpecialResponse> response = topicsController.getTopicForEdit(topicDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        JSONObject responseJson = new JSONObject(response.getBody().getMessage());
        assertEquals("The topic is closed", responseJson.getString("message"));
        verify(topicsService, times(1)).getTopicForEdit(topicDto.getId());
        verifyNoMoreInteractions(topicsService);
    }

    @Test
    void testGetTopicForEdit_UnauthorizedUser() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(25);
        topicDto.setUser("exampleUser");

        TopicsEntity topicEntity = new TopicsEntity();
        topicEntity.setStatus(Constants.STATUS_OPENED);
        topicEntity.setAuthor("differentUser");

        when(topicsService.getTopicForEdit(topicDto.getId())).thenReturn(topicEntity);

        ResponseEntity<SpecialResponse> response = topicsController.getTopicForEdit(topicDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        JSONObject responseJson = new JSONObject(response.getBody().getMessage());
        assertEquals("The user is not the author of the topic", responseJson.getString("message"));
        verify(topicsService, times(1)).getTopicForEdit(topicDto.getId());
        verifyNoMoreInteractions(topicsService);
    }

    @Test
    void testGetTopicForEdit_Successful() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(25);
        topicDto.setUser("exampleUser");

        TopicsEntity topicEntity = new TopicsEntity();
        topicEntity.setStatus(Constants.STATUS_OPENED);
        topicEntity.setAuthor("exampleUser");
        topicEntity.setOptions("exampleOption");

        when(topicsService.getTopicForEdit(topicDto.getId())).thenReturn(topicEntity);

        ResponseEntity<SpecialResponse> response = topicsController.getTopicForEdit(topicDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        JSONObject responseJson = new JSONObject(response.getBody().getMessage());
        assertEquals("OK", responseJson.getString("message"));

        verify(topicsService, times(1)).getTopicForEdit(topicDto.getId());
        verifyNoMoreInteractions(topicsService);
    }

    @Test
    void testEditTopic_AllDataRequired() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(25);
        topicDto.setTitle("");
        topicDto.setType("");
        topicDto.setQuestion("");
        topicDto.setOptions("");
        topicDto.setAuthor("");
        topicDto.setMembers("");
        topicDto.setUser("");

        ResponseEntity<String> response = topicsController.editTopic(topicDto);

        assertEquals(HttpStatus.BAD_GATEWAY, response.getStatusCode());
        JSONObject responseJson = new JSONObject(response.getBody());
        assertEquals("All data is required to edit a topic", responseJson.getString("message"));
    }

    @Test
    void testEditTopic_NonExistentTopicId() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(25);
        topicDto.setTitle("exampleTitle");
        topicDto.setType("exampleType");
        topicDto.setQuestion("exampleQuestion");
        topicDto.setOptions("exampleOptions");
        topicDto.setAuthor("exampleAuthor");
        topicDto.setMembers("exampleMembers");
        topicDto.setUser("exampleUser");

        when(topicsService.existsById(topicDto.getId())).thenReturn(false);

        ResponseEntity<String> response = topicsController.editTopic(topicDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        JSONObject responseJson = new JSONObject(response.getBody());
        assertEquals("There is no topic with that id", responseJson.getString("message"));
        verify(topicsService, times(1)).existsById(topicDto.getId());
        verifyNoMoreInteractions(topicsService);
        verifyNoInteractions(usersService);
        verifyNoInteractions(mailService);
        verify(topicsService, times(0)).saveTopic(any(TopicsEntity.class));
    }


    @Test
    void testEditTopic_NonExistentUser() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(25);
        topicDto.setTitle("exampleTitle");
        topicDto.setType("exampleType");
        topicDto.setQuestion("exampleQuestion");
        topicDto.setOptions("exampleOptions");
        topicDto.setAuthor("exampleAuthor");
        topicDto.setMembers("exampleMembers");
        topicDto.setUser("nonExistentUser");

        when(topicsService.existsById(topicDto.getId())).thenReturn(true);
        when(usersService.checkUser(topicDto.getUser())).thenReturn(false);

        ResponseEntity<String> response = topicsController.editTopic(topicDto);

        assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, response.getStatusCode());
        JSONObject responseJson = new JSONObject(response.getBody());
        assertEquals("An error occurred --> java.lang.NullPointerException: The user does not exist", responseJson.getString("message"));
        verify(topicsService, times(1)).existsById(topicDto.getId());
        verify(usersService, times(1)).checkUser(topicDto.getUser());
        verifyNoMoreInteractions(topicsService);
        verifyNoMoreInteractions(usersService);
        verifyNoInteractions(mailService);
    }

    @Test
    void testEditTopic_UnauthorizedUser() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(25);
        topicDto.setTitle("exampleTitle");
        topicDto.setType("exampleType");
        topicDto.setQuestion("exampleQuestion");
        topicDto.setOptions("exampleOptions");
        topicDto.setAuthor("exampleAuthor");
        topicDto.setMembers("exampleMembers");
        topicDto.setUser("unauthorizedUser");

        when(topicsService.existsById(topicDto.getId())).thenReturn(true);
        when(usersService.checkUser(topicDto.getUser())).thenReturn(true);

        ResponseEntity<String> response = topicsController.editTopic(topicDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        JSONObject responseJson = new JSONObject(response.getBody());
        assertEquals("The user is not the author of the topic", responseJson.getString("message"));
        verify(topicsService, times(1)).existsById(topicDto.getId());
        verify(usersService, times(1)).checkUser(topicDto.getUser());
        verifyNoMoreInteractions(topicsService);
        verifyNoMoreInteractions(usersService);
        verifyNoInteractions(mailService);
    }

    @Test
    void testEditTopic_ClosedTopic() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(25);
        topicDto.setTitle("exampleTitle");
        topicDto.setType("exampleType");
        topicDto.setQuestion("exampleQuestion");
        topicDto.setOptions("exampleOptions");
        topicDto.setAuthor("exampleAuthor");
        topicDto.setMembers("exampleMembers");
        topicDto.setUser("exampleAuthor");

        TopicsEntity topicEntity = new TopicsEntity();
        topicEntity.setStatus(Constants.STATUS_CLOSED);
        topicEntity.setAuthor("exampleAuthor");

        when(topicsService.existsById(topicDto.getId())).thenReturn(true);
        when(usersService.checkUser(topicDto.getUser())).thenReturn(true);
        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(topicEntity);

        ResponseEntity<String> response = topicsController.editTopic(topicDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        JSONObject responseJson = new JSONObject(response.getBody());
        assertEquals("The topic is closed", responseJson.getString("message"));
        verify(topicsService, times(1)).existsById(topicDto.getId());
        verify(usersService, times(1)).checkUser(topicDto.getUser());
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verifyNoMoreInteractions(topicsService);
        verifyNoMoreInteractions(usersService);
        verifyNoInteractions(mailService);
    }

    @Test
    void testEditTopic_DuplicateTitle() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(25);
        topicDto.setTitle("exampleTitle");
        topicDto.setType("exampleType");
        topicDto.setQuestion("exampleQuestion");
        topicDto.setOptions("exampleOptions");
        topicDto.setAuthor("exampleAuthor");
        topicDto.setMembers("exampleMembers");
        topicDto.setUser("exampleAuthor");

        TopicsEntity topicEntity = new TopicsEntity();
        topicEntity.setStatus(Constants.STATUS_OPENED);
        topicEntity.setAuthor("exampleAuthor");

        when(topicsService.existsById(topicDto.getId())).thenReturn(true);
        when(usersService.checkUser(topicDto.getUser())).thenReturn(true);
        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(topicEntity);
        when(topicsService.existsByTitleAndAuthor(topicDto.getTitle().strip(), topicDto.getAuthor())).thenReturn(true);

        ResponseEntity<String> response = topicsController.editTopic(topicDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        JSONObject responseJson = new JSONObject(response.getBody());
        assertEquals("There is already a topic assigned to the author with that name", responseJson.getString("message"));
        verify(topicsService, times(1)).existsById(topicDto.getId());
        verify(usersService, times(1)).checkUser(topicDto.getUser());
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verify(topicsService, times(1)).existsByTitleAndAuthor(topicDto.getTitle().strip(), topicDto.getAuthor());
        verifyNoMoreInteractions(topicsService);
        verifyNoMoreInteractions(usersService);
        verifyNoInteractions(mailService);
    }

    @Test
    void testEditTopic_InvalidTopicType() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(25);
        topicDto.setTitle("exampleTitle");
        topicDto.setType("invalidType");
        topicDto.setQuestion("exampleQuestion");
        topicDto.setOptions("exampleOptions");
        topicDto.setAuthor("exampleAuthor");
        topicDto.setMembers("exampleMembers");
        topicDto.setUser("exampleAuthor");

        TopicsEntity topicEntity = new TopicsEntity();
        topicEntity.setStatus(Constants.STATUS_OPENED);
        topicEntity.setAuthor("exampleAuthor");

        when(topicsService.existsById(topicDto.getId())).thenReturn(true);
        when(usersService.checkUser(topicDto.getUser())).thenReturn(true);
        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(topicEntity);
        when(topicsService.existsByTitleAndAuthor(topicDto.getTitle().strip(), topicDto.getAuthor())).thenReturn(false);

        ResponseEntity<String> response = topicsController.editTopic(topicDto);

        assertEquals(HttpStatus.BAD_GATEWAY, response.getStatusCode());
        JSONObject responseJson = new JSONObject(response.getBody());
        assertEquals("The topic type is not valid", responseJson.getString("message"));
        verify(topicsService, times(1)).existsById(topicDto.getId());
        verify(usersService, times(1)).checkUser(topicDto.getUser());
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verify(topicsService, times(1)).existsByTitleAndAuthor(topicDto.getTitle().strip(), topicDto.getAuthor());
        verifyNoMoreInteractions(topicsService);
        verifyNoMoreInteractions(usersService);
        verifyNoInteractions(mailService);
    }

    @Test
    void testEditTopic_Successful() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(25);
        topicDto.setTitle("exampleTitleUpdated");
        topicDto.setType(String.valueOf(Constants.TopicType.TEXT_SINGLE));
        topicDto.setQuestion("exampleQuestion");
        topicDto.setOptions("exampleOptions");
        topicDto.setAuthor("exampleAuthor");
        topicDto.setMembers("exampleMembers");
        topicDto.setUser("exampleAuthor");

        TopicsEntity topicEntity = new TopicsEntity();
        topicEntity.setId(25);
        topicEntity.setTitle("exampleTitle");
        topicEntity.setType(String.valueOf(Constants.TopicType.TEXT_SINGLE));
        topicEntity.setQuestion("exampleQuestion");
        topicEntity.setOptions("exampleOptions");
        topicEntity.setAuthor("exampleAuthor");
        topicEntity.setMembers("exampleMembers");
        topicEntity.setVisits(0);
        topicEntity.setStatus(Constants.STATUS_OPENED);

        when(topicsService.existsById(topicDto.getId())).thenReturn(true);
        when(usersService.checkUser(topicDto.getUser())).thenReturn(true);
        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(topicEntity);
        when(topicsService.existsByTitleAndAuthor(topicDto.getTitle().strip(), topicDto.getAuthor())).thenReturn(false);

        ResponseEntity<String> response = topicsController.editTopic(topicDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        JSONObject responseJson = new JSONObject(response.getBody());
        assertEquals("Topic edited successfully", responseJson.getString("message"));
        verify(topicsService, times(1)).existsById(topicDto.getId());
        verify(usersService, times(1)).checkUser(topicDto.getUser());
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verify(topicsService, times(1)).existsByTitleAndAuthor(topicDto.getTitle().strip(), topicDto.getAuthor());
        verify(topicsService, times(1)).saveTopic(topicEntity);
        verify(mailService, times(1)).sendEmail(topicDto);
        verifyNoMoreInteractions(topicsService);
        verifyNoMoreInteractions(usersService);
        verifyNoMoreInteractions(mailService);
    }

    @Test
    void testEditTopic_WithDiferentsTypes_Successful() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(25);
        topicDto.setTitle("exampleTitleUpdated");
        topicDto.setType(String.valueOf(Constants.TopicType.TEXT_SINGLE));
        topicDto.setQuestion("exampleQuestion");
        topicDto.setOptions("exampleOptions");
        topicDto.setAuthor("exampleAuthor");
        topicDto.setMembers("exampleMembers");
        topicDto.setUser("exampleAuthor");

        TopicsEntity topicEntity = new TopicsEntity();
        topicEntity.setId(25);
        topicEntity.setTitle("exampleTitle");
        topicEntity.setType(String.valueOf(Constants.TopicType.TEXT_MULTIPLE));
        topicEntity.setQuestion("exampleQuestion");
        topicEntity.setOptions("exampleOptions");
        topicEntity.setAuthor("exampleAuthor");
        topicEntity.setMembers("exampleMembers");
        topicEntity.setVisits(0);
        topicEntity.setStatus(Constants.STATUS_OPENED);

        when(topicsService.existsById(topicDto.getId())).thenReturn(true);
        when(usersService.checkUser(topicDto.getUser())).thenReturn(true);
        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(topicEntity);
        when(topicsService.existsByTitleAndAuthor(topicDto.getTitle().strip(), topicDto.getAuthor())).thenReturn(false);

        ResponseEntity<String> response = topicsController.editTopic(topicDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        JSONObject responseJson = new JSONObject(response.getBody());
        assertEquals("Topic edited successfully", responseJson.getString("message"));
        verify(topicsService, times(1)).existsById(topicDto.getId());
        verify(usersService, times(1)).checkUser(topicDto.getUser());
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verify(topicsService, times(1)).existsByTitleAndAuthor(topicDto.getTitle().strip(), topicDto.getAuthor());
        verify(topicsService, times(1)).saveTopic(topicEntity);
        verify(mailService, times(1)).sendEmail(topicDto);
        verify(topicsService, times(1)).initiateVoting(topicDto.getOptions());
        verifyNoMoreInteractions(usersService);
        verifyNoMoreInteractions(mailService);
    }

    @Test
    void testCloseTopic_TopicNotFound() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(25);

        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(null);

        ResponseEntity<String> response = topicsController.closeTopic(topicDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        JSONObject responseJson = new JSONObject(response.getBody());
        assertEquals("There is no topic with that id", responseJson.getString("message"));
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verifyNoMoreInteractions(topicsService);
    }

    @Test
    void testCloseTopic_UnauthorizedUser() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(25);
        topicDto.setUser("unauthorizedUser");

        TopicsEntity topicEntity = new TopicsEntity();
        topicEntity.setAuthor("exampleAuthor");

        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(topicEntity);

        ResponseEntity<String> response = topicsController.closeTopic(topicDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        JSONObject responseJson = new JSONObject(response.getBody());
        assertEquals("The user is not the author of the topic", responseJson.getString("message"));
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verifyNoMoreInteractions(topicsService);
    }

    @Test
    void testCloseTopic_TopicAlreadyClosed() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(25);
        topicDto.setUser("exampleAuthor");

        TopicsEntity topicEntity = new TopicsEntity();
        topicEntity.setAuthor("exampleAuthor");
        topicEntity.setStatus(Constants.STATUS_CLOSED);

        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(topicEntity);

        ResponseEntity<String> response = topicsController.closeTopic(topicDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        JSONObject responseJson = new JSONObject(response.getBody());
        assertEquals("The topic is closed", responseJson.getString("message"));
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verifyNoMoreInteractions(topicsService);
    }

    @Test
    void testCloseTopic_Success() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(25);
        topicDto.setUser("exampleAuthor");

        TopicsEntity topicEntity = new TopicsEntity();
        topicEntity.setAuthor("exampleAuthor");
        topicEntity.setStatus(Constants.STATUS_OPENED);

        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(topicEntity);

        ResponseEntity<String> response = topicsController.closeTopic(topicDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        JSONObject responseJson = new JSONObject(response.getBody());
        assertEquals("The topic has been closed", responseJson.getString("message"));
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verify(topicsService, times(1)).saveTopic(topicEntity);
        verifyNoMoreInteractions(topicsService);
    }

    @Test
    void testDeleteTopic_TopicNotFound() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(25);

        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(null);

        ResponseEntity<String> response = topicsController.deleteTopic(topicDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        JSONObject responseJson = new JSONObject(response.getBody());
        assertEquals("There is no topic with that id", responseJson.getString("message"));
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verifyNoMoreInteractions(topicsService);
    }

    @Test
    void testDeleteTopic_UnauthorizedUser() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(25);
        topicDto.setUser("unauthorizedUser");

        TopicsEntity topicEntity = new TopicsEntity();
        topicEntity.setAuthor("exampleAuthor");

        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(topicEntity);

        ResponseEntity<String> response = topicsController.deleteTopic(topicDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        JSONObject responseJson = new JSONObject(response.getBody());
        assertEquals("The user is not the author of the topic", responseJson.getString("message"));
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verifyNoMoreInteractions(topicsService);
    }

    @Test
    void testDeleteTopic_Success() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(25);
        topicDto.setUser("exampleAuthor");

        TopicsEntity topicEntity = new TopicsEntity();
        topicEntity.setAuthor("exampleAuthor");

        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(topicEntity);

        ResponseEntity<String> response = topicsController.deleteTopic(topicDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        JSONObject responseJson = new JSONObject(response.getBody());
        assertEquals("The topic has been deleted", responseJson.getString("message"));
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verify(topicsService, times(1)).deleteTopic(topicDto.getId());
        verifyNoMoreInteractions(topicsService);
    }

    @Test
    void testGetAllTopicsData_NoTopicsInDatabase() {
        List<TopicsEntity> emptyTopicsList = new ArrayList<>();
        when(topicsService.getAllTopicsData()).thenReturn(emptyTopicsList);

        ResponseEntity<SpecialResponse> response = topicsController.getAllTopicsData();

        assertEquals(HttpStatus.OK, response.getStatusCode());
        SpecialResponse responseBody = response.getBody();
        assert responseBody != null;
        JSONObject responseJson = new JSONObject(responseBody.getMessage());
        assertEquals("There are no topics in database", responseJson.getString("message"));
        assertNull(responseBody.getEntity());
        verify(topicsService, times(1)).getAllTopicsData();
        verifyNoMoreInteractions(topicsService);
    }

    @Test
    void testGetAllTopicsData_TopicsInDatabase() {
        List<TopicsEntity> topicsList = new ArrayList<>();
        TopicsEntity topic1 = new TopicsEntity();
        topicsList.add(topic1);
        TopicsEntity topic2 = new TopicsEntity();
        topicsList.add(topic2);
        when(topicsService.getAllTopicsData()).thenReturn(topicsList);

        ResponseEntity<SpecialResponse> response = topicsController.getAllTopicsData();

        assertEquals(HttpStatus.OK, response.getStatusCode());
        SpecialResponse responseBody = response.getBody();
        assert responseBody != null;
        JSONObject responseJson = new JSONObject(responseBody.getMessage());
        assertEquals("OK", responseJson.getString("message"));
        assertEquals(topicsList, responseBody.getEntity());
        verify(topicsService, times(1)).getAllTopicsData();
        verifyNoMoreInteractions(topicsService);
    }

    @Test
    void testVote_TopicNotFound() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(25);

        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(null);

        ResponseEntity<String> response = topicsController.vote(topicDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertEquals("{\"message\":\"There is no topic with that id\"}", response.getBody());
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verifyNoMoreInteractions(topicsService);
    }

    @Test
    void testVote_UserDoesNotExist() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(25);
        topicDto.setUser("nonexistentUser");

        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(new TopicsEntity());
        when(usersService.checkUser(topicDto.getUser())).thenReturn(false);

        assertThrows(NullPointerException.class, () -> topicsController.vote(topicDto));
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verify(usersService, times(1)).checkUser(topicDto.getUser());
        verifyNoMoreInteractions(topicsService);
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testVote_VotingEmpty() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(25);
        topicDto.setVotation(new ArrayList<>());

        TopicsEntity topicEntity = new TopicsEntity();

        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(topicEntity);
        when(usersService.checkUser(topicDto.getUser())).thenReturn(true);

        ResponseEntity<String> response = topicsController.vote(topicDto);

        assertEquals(HttpStatus.BAD_GATEWAY, response.getStatusCode());
        assertEquals("{\"message\":\"The voting cannot be empty\"}", response.getBody());
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verifyNoMoreInteractions(topicsService);
    }

    @Test
    void testVote_UserNotAllowedToVote() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(25);
        topicDto.setUser("unauthorizedUser");
        topicDto.setVotation(Collections.singletonList("votation"));

        TopicsEntity topicEntity = new TopicsEntity();
        topicEntity.setAuthor("author");
        topicEntity.setMembers("allowedUser");

        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(topicEntity);
        when(usersService.checkUser(topicDto.getUser())).thenReturn(true);

        ResponseEntity<String> response = topicsController.vote(topicDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertEquals("{\"message\":\"The user is not allowed to vote on this topic\"}", response.getBody());
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verifyNoMoreInteractions(topicsService);
    }

    @Test
    void testVote_UserAlreadyVoted() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(25);
        topicDto.setUser("votedUser");
        topicDto.setVotation(Collections.singletonList("votation"));

        TopicsEntity topicEntity = new TopicsEntity();
        topicEntity.setVotedBy("votedUser");
        topicEntity.setMembers("members");
        topicEntity.setAuthor("votedUser");

        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(topicEntity);
        when(usersService.checkUser(topicDto.getUser())).thenReturn(true);

        ResponseEntity<String> response = topicsController.vote(topicDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertEquals("{\"message\":\"The user has already voted\"}", response.getBody());
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verifyNoMoreInteractions(topicsService);
    }

    @Test
    void testVote_InvalidTopicTypeForMultipleOptions() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(25);
        topicDto.setUser("user");
        topicDto.setVotation(List.of("Option1", "Option2"));

        TopicsEntity topicEntity = new TopicsEntity();
        topicEntity.setType(Constants.TopicType.TEXT_SINGLE.toString());
        topicEntity.setAuthor("user");
        topicEntity.setMembers("members");

        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(topicEntity);
        when(usersService.checkUser(topicDto.getUser())).thenReturn(true);

        ResponseEntity<String> response = topicsController.vote(topicDto);

        assertEquals(HttpStatus.BAD_GATEWAY, response.getStatusCode());
        assertEquals("{\"message\":\"The topic type is not valid for multiple voting options\"}", response.getBody());
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verifyNoMoreInteractions(topicsService);
    }

    @Test
    void testVote_SuccessfulVoteUpdate_VotedByNull() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(25);
        topicDto.setUser("validUser");
        topicDto.setVotation(List.of("Option1"));

        TopicsEntity topicEntity = new TopicsEntity();
        topicEntity.setType(Constants.TopicType.TEXT_MULTIPLE.toString());
        topicEntity.setAuthor("validUser");
        topicEntity.setMembers("members");
        topicEntity.setOptions("Option1,Option2");
        topicEntity.setVotedBy(null);

        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(topicEntity);
        when(usersService.checkUser(topicDto.getUser())).thenReturn(true);

        ResponseEntity<String> response = topicsController.vote(topicDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertEquals("{\"message\":\"Votation updated successfully\"}", response.getBody());
        assertEquals("validUser", topicEntity.getVotedBy());
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verify(topicsService, times(1)).saveTopic(topicEntity);
        verify(usersService, times(1)).checkUser(topicDto.getUser());
        verifyNoMoreInteractions(topicsService);
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testVote_SuccessfulVoteUpdate() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(25);
        topicDto.setUser("validUser");
        topicDto.setVotation(List.of("Option1,Option2"));

        TopicsEntity topicEntity = new TopicsEntity();
        topicEntity.setType(Constants.TopicType.TEXT_MULTIPLE.toString());
        topicEntity.setAuthor("validUser");
        topicEntity.setMembers("members");
        topicEntity.setOptions("Option1:1,Option2:0");
        topicEntity.setVotedBy("userOne");

        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(topicEntity);
        when(usersService.checkUser(topicDto.getUser())).thenReturn(true);

        ResponseEntity<String> response = topicsController.vote(topicDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertEquals("{\"message\":\"Votation updated successfully\"}", response.getBody());
        assertEquals("userOne, validUser", topicEntity.getVotedBy());
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verify(topicsService, times(1)).saveTopic(topicEntity);
        verify(usersService, times(1)).checkUser(topicDto.getUser());
        verifyNoMoreInteractions(topicsService);
        verifyNoMoreInteractions(usersService);
    }

    @Test
    void testVotingResults_TopicNotFound() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(25);
        topicDto.setUser("validUser");

        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(null);

        ResponseEntity<SpecialResponse> response = topicsController.votingResults(topicDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        SpecialResponse responseBody = response.getBody();
        assert responseBody != null;
        JSONObject responseJson = new JSONObject(responseBody.getMessage());
        assertEquals("There is no topic with that id", responseJson.getString("message"));
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verifyNoMoreInteractions(topicsService);
    }

    @Test
    void testVotingResults_UserNotAllowed() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(25);
        topicDto.setUser("unauthorizedUser");

        TopicsEntity topicEntity = new TopicsEntity();
        topicEntity.setMembers("members");
        topicEntity.setAuthor("author");

        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(topicEntity);

        ResponseEntity<SpecialResponse> response = topicsController.votingResults(topicDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        SpecialResponse responseBody = response.getBody();
        assert responseBody != null;
        JSONObject responseJson = new JSONObject(responseBody.getMessage());
        assertEquals("The user is not allowed to view the results on this topic", responseJson.getString("message"));
        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verifyNoMoreInteractions(topicsService);
    }

    @Test
    void testVotingResults_Successful() {
        TopicsDto topicDto = new TopicsDto();
        topicDto.setId(25);
        topicDto.setUser("validUser");

        TopicsEntity topicEntity = new TopicsEntity();
        topicEntity.setMembers("members");
        topicEntity.setAuthor("validUser");
        topicEntity.setType("TEXT_MULTIPLE");
        topicEntity.setOptions("Option1:10, Option2:5");

        when(topicsService.findTopicsEntityById(topicDto.getId())).thenReturn(topicEntity);

        ResponseEntity<SpecialResponse> response = topicsController.votingResults(topicDto);

        assertEquals(HttpStatus.OK, response.getStatusCode());
        SpecialResponse responseBody = response.getBody();
        assert responseBody != null;
        JSONObject responseJson = new JSONObject(responseBody.getMessage());
        assertEquals("TEXT_MULTIPLE", responseJson.getString("message"));

        Map<String, String> expectedOptionsMap = new HashMap<>();
        expectedOptionsMap.put("Option1", "10");
        expectedOptionsMap.put("Option2", "5");
        assertEquals(expectedOptionsMap, responseBody.getEntity());

        verify(topicsService, times(1)).findTopicsEntityById(topicDto.getId());
        verifyNoMoreInteractions(topicsService);
    }

}






