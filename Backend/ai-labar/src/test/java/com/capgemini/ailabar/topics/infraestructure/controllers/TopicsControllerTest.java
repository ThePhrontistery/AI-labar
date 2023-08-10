package com.capgemini.ailabar.topics.infraestructure.controllers;

import com.capgemini.ailabar.commons.utils.MailService;
import com.capgemini.ailabar.topics.application.services.TopicsService;
import com.capgemini.ailabar.users.application.services.UsersService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

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

//    @Test
//    void testLogin_UserAndPasswordMissing_ReturnsBadRequest() {
//        UsersModel userDto = new UsersModel();
//        userDto.setUser("");
//        userDto.setPassword("");
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "User and password are required to login");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.BAD_GATEWAY);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.login(userDto);
//
//        assertEquals(HttpStatus.BAD_GATEWAY, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verifyNoInteractions(topicsService);
//    }
//
//    @Test
//    void testLogin_LoginFailed_ReturnsUnauthorized() {
//        UsersModel userDto = new UsersModel();
//        userDto.setUser("exampleUser");
//        userDto.setPassword("examplePassword");
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "Login failed");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.UNAUTHORIZED);
//
//        when(topicsService.login(userDto.getUser(), DigestUtils.sha256Hex(userDto.getPassword()))).thenReturn(false);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.login(userDto);
//
//        assertEquals(HttpStatus.UNAUTHORIZED, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(topicsService, times(1)).login(userDto.getUser(), DigestUtils.sha256Hex(userDto.getPassword()));
//        verifyNoMoreInteractions(topicsService);
//    }
//
//    @Test
//    void testLogin_UserNotFound_ReturnsNotFound() {
//        UsersModel userDto = new UsersModel();
//        userDto.setUser("exampleUser");
//        userDto.setPassword("examplePassword");
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "User not found");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.NOT_FOUND);
//
//        when(topicsService.login(userDto.getUser(), DigestUtils.sha256Hex(userDto.getPassword()))).thenReturn(true);
//        when(usersService.findByUser(userDto.getUser())).thenReturn(null);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.login(userDto);
//
//        assertEquals(HttpStatus.NOT_FOUND, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(topicsService, times(1)).login(userDto.getUser(), DigestUtils.sha256Hex(userDto.getPassword()));
//        verify(usersService, times(1)).findByUser(userDto.getUser());
//        verifyNoMoreInteractions(topicsService, usersService);
//    }
//
//    @Test
//    void testLogin_LoginSuccessful_ReturnsToken() {
//        UsersModel userDto = new UsersModel();
//        userDto.setUser("exampleUser");
//        userDto.setPassword("examplePassword");
//
//        UsersEntity userEntity = new UsersEntity();
//        userEntity.setToken("exampleToken");
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "Login successful");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(userEntity.getToken(), expectedResponseJson), HttpStatus.OK);
//
//        when(topicsService.login(userDto.getUser(), DigestUtils.sha256Hex(userDto.getPassword()))).thenReturn(true);
//        when(usersService.findByUser(userDto.getUser())).thenReturn(userEntity);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.login(userDto);
//
//        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(topicsService, times(1)).login(userDto.getUser(), DigestUtils.sha256Hex(userDto.getPassword()));
//        verify(usersService, times(1)).findByUser(userDto.getUser());
//        verifyNoMoreInteractions(topicsService, usersService);
//    }
//
//    @Test
//    void testLoadTopics_UnauthorizedUser_ReturnsNotFound() {
//        UsersModel userDto = new UsersModel();
//        userDto.setUser("exampleUser");
//        userDto.setToken("invalidToken");
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "Unauthorized user");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.NOT_FOUND);
//
//        when(usersService.checkToken(userDto.getUser(), userDto.getToken())).thenReturn(false);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.loadTopics(userDto);
//
//        assertEquals(HttpStatus.NOT_FOUND, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(usersService, times(1)).checkToken(userDto.getUser(), userDto.getToken());
//        verifyNoMoreInteractions(usersService);
//    }
//
//    @Test
//    void testLoadTopics_NoTopicsFound_ReturnsOk() {
//        UsersModel userDto = new UsersModel();
//        userDto.setUser("exampleUser");
//        userDto.setToken("validToken");
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "There are no topics related to the user");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);
//
//        when(usersService.checkToken(userDto.getUser(), userDto.getToken())).thenReturn(true);
//        when(topicsService.loadTopics(userDto.getUser())).thenReturn(Collections.emptyList());
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.loadTopics(userDto);
//
//        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(usersService, times(1)).checkToken(userDto.getUser(), userDto.getToken());
//        verify(topicsService, times(1)).loadTopics(userDto.getUser());
//        verifyNoMoreInteractions(usersService, topicsService);
//    }
//
//    @Test
//    void testLoadTopics_TopicsFound_ReturnsOk() {
//        UsersModel userDto = new UsersModel();
//        userDto.setUser("exampleUser");
//        userDto.setToken("validToken");
//
//        OptionsData option1 = new OptionsData("Option 1", 0);
//        OptionsData option2 = new OptionsData("Option 2", 0);
//        List<OptionsData> options = Arrays.asList(option1, option2);
//
//        TopicsEntity topic1 = new TopicsEntity();
//        topic1.setId(1);
//        topic1.setTitle("Topic 1");
//        topic1.setType(String.valueOf(Constants.TopicType.TEXT_SINGLE));
//        topic1.setQuestion("Question 1");
//        topic1.setOptions("[{\"option\":\"Sí\",\"votes\":0},{\"option\":\"No\",\"votes\":0}]");
//        topic1.setAuthor("Author 1");
//        topic1.setMembers("[\"Member 1\",\"Member 2\"]");
//        topic1.setVisits(5);
//        topic1.setStatus(Constants.STATUS_OPENED);
//
//        TopicsEntity topic2 = new TopicsEntity();
//        topic2.setId(2);
//        topic2.setTitle("Topic 2");
//        topic2.setType(String.valueOf(Constants.TopicType.TEXT_SINGLE));
//        topic2.setQuestion("Question 2");
//        topic2.setOptions("[{\"option\":\"Sí\",\"votes\":0},{\"option\":\"No\",\"votes\":0}]");
//        topic2.setAuthor("Author 2");
//        topic2.setMembers("[\"Member 3\"]");
//        topic2.setVisits(3);
//        topic2.setStatus(Constants.STATUS_OPENED);
//
//        List<TopicsEntity> topicsList = Arrays.asList(topic1, topic2);
//
//        List<TopicsModel> expectedTopicsDtoList = new ArrayList<>();
//
//        TopicsModel topicsDto1 = new TopicsModel();
//        topicsDto1.setId(topic1.getId());
//        topicsDto1.setTitle(topic1.getTitle());
//        topicsDto1.setType(topic1.getType());
//        topicsDto1.setQuestion(topic1.getQuestion());
//        topicsDto1.setOptions(options);
//        topicsDto1.setAuthor(topic1.getAuthor());
//        topicsDto1.setMembers(Collections.singletonList(topic1.getMembers()));
//        topicsDto1.setVisits(topic1.getVisits());
//        topicsDto1.setStatus(topic1.getStatus());
//
//        TopicsModel topicsDto2 = new TopicsModel();
//        topicsDto2.setId(topic2.getId());
//        topicsDto2.setTitle(topic2.getTitle());
//        topicsDto2.setType(topic2.getType());
//        topicsDto2.setQuestion(topic2.getQuestion());
//        topicsDto2.setOptions(options);
//        topicsDto2.setAuthor(topic2.getAuthor());
//        topicsDto2.setMembers(Collections.singletonList(topic2.getMembers()));
//        topicsDto2.setVisits(topic2.getVisits());
//        topicsDto2.setStatus(topic2.getStatus());
//
//        expectedTopicsDtoList.add(topicsDto1);
//        expectedTopicsDtoList.add(topicsDto2);
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "OK");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(expectedTopicsDtoList, expectedResponseJson), HttpStatus.OK);
//
//        when(usersService.checkToken(userDto.getUser(), userDto.getToken())).thenReturn(true);
//        when(topicsService.loadTopics(userDto.getUser())).thenReturn(topicsList);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.loadTopics(userDto);
//
//        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(usersService, times(1)).checkToken(userDto.getUser(), userDto.getToken());
//        verify(topicsService, times(1)).loadTopics(userDto.getUser());
//        verifyNoMoreInteractions(usersService, topicsService);
//    }
//
//    @Test
//    void testCreateTopic_AllDataMissing_ReturnsBadRequest() {
//        TopicsModel topicModel = new TopicsModel();
//        topicModel.setTitle("");
//        topicModel.setType("");
//        topicModel.setQuestion("");
//        topicModel.setOptions(new ArrayList<>());
//        topicModel.setUser("");
//        topicModel.setMembers(new ArrayList<>());
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "All data is required to edit a topic");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.BAD_GATEWAY);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.createTopic(topicModel);
//
//        assertEquals(HttpStatus.BAD_GATEWAY, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verifyNoMoreInteractions(usersService, topicsService);
//    }
//
//    @Test
//    void testCreateTopic_UnauthorizedUser_ReturnsNotFound() {
//        OptionsData option1 = new OptionsData("Option 1", 0);
//        OptionsData option2 = new OptionsData("Option 2", 0);
//        List<OptionsData> options = Arrays.asList(option1, option2);
//
//        TopicsModel topicModel = new TopicsModel();
//        topicModel.setTitle("Topic 1");
//        topicModel.setType("Type 1");
//        topicModel.setQuestion("Question 1");
//        topicModel.setOptions(options);
//        topicModel.setUser("exampleUser");
//        topicModel.setMembers(Arrays.asList("Member 1", "Member 2"));
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "Unauthorized user");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.NOT_FOUND);
//
//        when(usersService.checkToken(topicModel.getUser(), topicModel.getToken())).thenReturn(false);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.createTopic(topicModel);
//
//        assertEquals(HttpStatus.NOT_FOUND, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(usersService, times(1)).checkToken(topicModel.getUser(), topicModel.getToken());
//        verifyNoMoreInteractions(usersService);
//    }
//
//    @Test
//    void testCreateTopic_DuplicateTopic_ReturnsOk() {
//        OptionsData option1 = new OptionsData("Option 1", 0);
//        OptionsData option2 = new OptionsData("Option 2", 0);
//        List<OptionsData> options = Arrays.asList(option1, option2);
//
//        TopicsModel topicModel = new TopicsModel();
//        topicModel.setTitle("Topic 1");
//        topicModel.setType("Type 1");
//        topicModel.setQuestion("Question 1");
//        topicModel.setOptions(options);
//        topicModel.setUser("exampleUser");
//        topicModel.setMembers(Arrays.asList("Member 1", "Member 2"));
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "There is already a topic assigned to the author with that name");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);
//
//        when(usersService.checkToken(topicModel.getUser(), topicModel.getToken())).thenReturn(true);
//        when(topicsService.existsByTitleAndAuthor(topicModel.getTitle().strip(), topicModel.getUser())).thenReturn(true);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.createTopic(topicModel);
//
//        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(usersService, times(1)).checkToken(topicModel.getUser(), topicModel.getToken());
//        verify(topicsService, times(1)).existsByTitleAndAuthor(topicModel.getTitle().strip(), topicModel.getUser());
//        verifyNoMoreInteractions(usersService, topicsService);
//    }
//
//    @Test
//    void testCreateTopic_InvalidTopicType_ReturnsBadRequest() {
//        OptionsData option1 = new OptionsData("Option 1", 0);
//        OptionsData option2 = new OptionsData("Option 2", 0);
//        List<OptionsData> options = Arrays.asList(option1, option2);
//
//        TopicsModel topicModel = new TopicsModel();
//        topicModel.setTitle("Topic 1");
//        topicModel.setType("InvalidType");
//        topicModel.setQuestion("Question 1");
//        topicModel.setOptions(options);
//        topicModel.setUser("exampleUser");
//        topicModel.setMembers(Arrays.asList("Member 1", "Member 2"));
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "The topic type is not valid");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.BAD_GATEWAY);
//
//        when(usersService.checkToken(topicModel.getUser(), topicModel.getToken())).thenReturn(true);
//        when(topicsService.existsByTitleAndAuthor(topicModel.getTitle().strip(), topicModel.getUser())).thenReturn(false);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.createTopic(topicModel);
//
//        assertEquals(HttpStatus.BAD_GATEWAY, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(usersService, times(1)).checkToken(topicModel.getUser(), topicModel.getToken());
//        verify(topicsService, times(1)).existsByTitleAndAuthor(topicModel.getTitle().strip(), topicModel.getUser());
//        verifyNoMoreInteractions(usersService, topicsService);
//    }
//
//    @Test
//    void testCreateTopic_TypeError_ReturnsBadRequest() {
//        OptionsData option1 = new OptionsData("Option 1", 0);
//        OptionsData option2 = new OptionsData("Option 2", 0);
//        List<OptionsData> options = Arrays.asList(option1, option2);
//
//        TopicsModel topicModel = new TopicsModel();
//        topicModel.setTitle("Topic 1");
//        topicModel.setType("InvalidType");
//        topicModel.setQuestion("Question 1");
//        topicModel.setOptions(options);
//        topicModel.setUser("exampleUser");
//        topicModel.setMembers(Arrays.asList("Member 1", "Member 2"));
//        topicModel.setCloseDate("19/07-2023");
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "The topic type is not valid");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.BAD_GATEWAY);
//
//        when(usersService.checkToken(topicModel.getUser(), topicModel.getToken())).thenReturn(true);
//        when(topicsService.existsByTitleAndAuthor(topicModel.getTitle().strip(), topicModel.getUser())).thenReturn(false);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.createTopic(topicModel);
//
//        assertEquals(HttpStatus.BAD_GATEWAY, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(usersService, times(1)).checkToken(topicModel.getUser(), topicModel.getToken());
//        verify(topicsService, times(1)).existsByTitleAndAuthor(topicModel.getTitle().strip(), topicModel.getUser());
//        verifyNoMoreInteractions(usersService, topicsService, mailService);
//    }
//
//    @Test
//    void testCreateTopic_ImageAndOptionsAreMandatory() {
//        OptionsData option1 = new OptionsData("Option 1", 0);
//        OptionsData option2 = new OptionsData("Option 2", 0);
//        List<OptionsData> options = Arrays.asList(option1, option2);
//
//        TopicsModel topicModel = new TopicsModel();
//        topicModel.setTitle("Topic 1");
//        topicModel.setType(String.valueOf(Constants.TopicType.IMAGE_MULTIPLE));
//        topicModel.setQuestion("Question 1");
//        topicModel.setOptions(options);
//        topicModel.setUser("exampleUser");
//        topicModel.setMembers(Arrays.asList("Member 1", "Member 2"));
//        topicModel.setCloseDate("19/07-2023");
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "It is mandatory to send the images and options for this type of topic");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.BAD_GATEWAY);
//
//        when(usersService.checkToken(topicModel.getUser(), topicModel.getToken())).thenReturn(true);
//        when(topicsService.existsByTitleAndAuthor(topicModel.getTitle().strip(), topicModel.getUser())).thenReturn(false);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.createTopic(topicModel);
//
//        assertEquals(HttpStatus.BAD_GATEWAY, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(usersService, times(1)).checkToken(topicModel.getUser(), topicModel.getToken());
//        verify(topicsService, times(1)).existsByTitleAndAuthor(topicModel.getTitle().strip(), topicModel.getUser());
//        verifyNoMoreInteractions(usersService, topicsService, mailService);
//    }
//
//    @Test
//    void testCreateTopic_InternalServerError_ReturnsInternalServerError() throws IOException {
//        OptionsData option1 = new OptionsData("Option 1", 0);
//        OptionsData option2 = new OptionsData("Option 2", 0);
//        List<OptionsData> options = Arrays.asList(option1, option2);
//
//        TopicsModel topicModel = new TopicsModel();
//        topicModel.setTitle("Title");
//        topicModel.setType(String.valueOf(Constants.TopicType.TEXT_SINGLE));
//        topicModel.setQuestion("Question 1");
//        topicModel.setOptions(options);
//        topicModel.setUser("exampleUser");
//        topicModel.setMembers(List.of("[\"User 1\", \"User 2\"]"));
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "An error occurred --> java.lang.NullPointerException: Members not found in the database");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.INTERNAL_SERVER_ERROR);
//
//        when(usersService.checkToken(topicModel.getUser(), topicModel.getToken())).thenReturn(true);
//        when(topicsService.existsByTitleAndAuthor(topicModel.getTitle().strip(), topicModel.getUser())).thenReturn(false);
//        when(topicsService.initiateVoting(topicModel.getType(), topicModel.getOptions())).thenReturn("[\"Option 1\", \"Option 2\"]");
//        when(topicsService.checkMailActivate()).thenReturn(true);
//        doThrow(new NullPointerException("Members not found in the database")).when(mailService).sendEmail(topicModel);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.createTopic(topicModel);
//
//        assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(usersService, times(1)).checkToken(topicModel.getUser(), topicModel.getToken());
//        verify(topicsService, times(1)).existsByTitleAndAuthor(topicModel.getTitle().strip(), topicModel.getUser());
//        verify(topicsService, times(1)).initiateVoting(topicModel.getType(), topicModel.getOptions());
//        verify(topicsService, times(1)).checkMailActivate();
//        verify(mailService, times(1)).sendEmail(topicModel);
//        verifyNoMoreInteractions(usersService, topicsService, mailService);
//    }
//
//    @Test
//    void testCreateTopic_ValidInput_ReturnsOk() throws IOException {
//        OptionsData option1 = new OptionsData("Option 1", 0);
//        OptionsData option2 = new OptionsData("Option 2", 0);
//        List<OptionsData> options = Arrays.asList(option1, option2);
//
//        TopicsModel topicModel = new TopicsModel();
//        topicModel.setTitle("Topic 1");
//        topicModel.setType(String.valueOf(Constants.TopicType.TEXT_SINGLE));
//        topicModel.setQuestion("Question 1");
//        topicModel.setOptions(options);
//        topicModel.setUser("exampleUser");
//        topicModel.setMembers(List.of("[\"User 1\", \"User 2\"]"));
//        topicModel.setCloseDate("20230720");
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "Topic created successfully");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);
//
//        when(usersService.checkToken(topicModel.getUser(), topicModel.getToken())).thenReturn(true);
//        when(topicsService.existsByTitleAndAuthor(topicModel.getTitle().strip(), topicModel.getUser())).thenReturn(false);
//        when(topicsService.initiateVoting(topicModel.getType(), topicModel.getOptions())).thenReturn("[\"Option 1\", \"Option 2\"]");
//        when(topicsService.checkMailActivate()).thenReturn(true);
//        doNothing().when(topicsService).saveTopic(any(TopicsEntity.class));
//        doNothing().when(mailService).sendEmail(topicModel);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.createTopic(topicModel);
//
//        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(usersService, times(1)).checkToken(topicModel.getUser(), topicModel.getToken());
//        verify(topicsService, times(1)).existsByTitleAndAuthor(topicModel.getTitle().strip(), topicModel.getUser());
//        verify(topicsService, times(1)).initiateVoting(topicModel.getType(), topicModel.getOptions());
//        verify(topicsService, times(1)).checkMailActivate();
//        verify(mailService, times(1)).sendEmail(topicModel);
//        verify(topicsService, times(1)).saveTopic(any(TopicsEntity.class));
//        verifyNoMoreInteractions(usersService, topicsService, mailService);
//    }
//
//    @Test
//    void testEditTopic_TopicNotFound_ReturnsTopicNotFound() {
//        OptionsData option1 = new OptionsData("Option 1", 0);
//        OptionsData option2 = new OptionsData("Option 2", 0);
//        List<OptionsData> options = Arrays.asList(option1, option2);
//
//        TopicsModel topicModel = new TopicsModel();
//        topicModel.setId(1);
//        topicModel.setTitle("Topic 1");
//        topicModel.setType("Type 1");
//        topicModel.setQuestion("Question 1");
//        topicModel.setOptions(options);
//        topicModel.setAuthor("Author 1");
//        topicModel.setMembers(Arrays.asList("Member 1", "Member 2"));
//        topicModel.setUser("exampleUser");
//        topicModel.setToken("validToken");
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "There is no topic with that id");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);
//
//        when(usersService.checkToken(topicModel.getUser(), topicModel.getToken())).thenReturn(true);
//        when(topicsService.existsById(topicModel.getId())).thenReturn(false);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.editTopic(topicModel);
//
//        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(usersService, times(1)).checkToken(topicModel.getUser(), topicModel.getToken());
//        verify(topicsService, times(1)).existsById(topicModel.getId());
//        verifyNoMoreInteractions(usersService, topicsService);
//    }
//
//    @Test
//    void testEditTopic_AllDataRequired_ReturnsBadGateway() {
//        TopicsModel topicModel = new TopicsModel();
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "All data is required to edit a topic");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.BAD_GATEWAY);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.editTopic(topicModel);
//
//        assertEquals(HttpStatus.BAD_GATEWAY, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verifyNoMoreInteractions(usersService, topicsService, mailService);
//    }
//
//    @Test
//    void testEditTopic_UnauthorizedUser_ReturnsNotFound() {
//        OptionsData option1 = new OptionsData("Option 1", 0);
//        OptionsData option2 = new OptionsData("Option 2", 0);
//        List<OptionsData> options = Arrays.asList(option1, option2);
//
//        TopicsModel topicModel = new TopicsModel();
//        topicModel.setId(1);
//        topicModel.setTitle("Topic 1");
//        topicModel.setType(String.valueOf(Constants.TopicType.TEXT_SINGLE));
//        topicModel.setQuestion("Question 1");
//        topicModel.setOptions(options);
//        topicModel.setMembers(List.of("[\"User 1\", \"User 2\"]"));
//        topicModel.setUser("Author 1");
//        topicModel.setToken("validToken");
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "Unauthorized user");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.NOT_FOUND);
//
//        when(usersService.checkToken(topicModel.getUser(), topicModel.getToken())).thenReturn(false);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.editTopic(topicModel);
//
//        assertEquals(HttpStatus.NOT_FOUND, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(usersService, times(1)).checkToken(topicModel.getUser(), topicModel.getToken());
//        verifyNoMoreInteractions(usersService, topicsService, mailService);
//    }
//
//    @Test
//    void testEditTopic_UnauthorizedAuthor_ReturnsOk() {
//        OptionsData option1 = new OptionsData("Option 1", 0);
//        OptionsData option2 = new OptionsData("Option 2", 0);
//        List<OptionsData> options = Arrays.asList(option1, option2);
//
//        TopicsEntity topicEntity = new TopicsEntity();
//        topicEntity.setId(1);
//        topicEntity.setTitle("Topic 2");
//        topicEntity.setType(String.valueOf(Constants.TopicType.TEXT_SINGLE));
//        topicEntity.setQuestion("Question 1");
//        topicEntity.setOptions("{\"Option 1\": 1, \"Option 2\": 2}");
//        topicEntity.setAuthor("Author 1");
//        topicEntity.setMembers("[\"User 1\", \"User 2\"]");
//        topicEntity.setCloseDate(null);
//        topicEntity.setVisits(0);
//        topicEntity.setStatus(Constants.STATUS_OPENED);
//
//        TopicsModel topicModel = new TopicsModel();
//        topicModel.setId(1);
//        topicModel.setTitle("Topic 1");
//        topicModel.setType(String.valueOf(Constants.TopicType.TEXT_SINGLE));
//        topicModel.setQuestion("Question 1");
//        topicModel.setOptions(options);
//        topicModel.setMembers(List.of("[\"User 1\", \"User 2\"]"));
//        topicModel.setUser("Author 2");
//        topicModel.setToken("validToken");
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "The user is not the author of the topic");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);
//
//        when(usersService.checkToken(topicModel.getUser(), topicModel.getToken())).thenReturn(true);
//        when(topicsService.existsById(topicModel.getId())).thenReturn(true);
//        when(topicsService.findTopicsEntityById(topicModel.getId())).thenReturn(topicEntity);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.editTopic(topicModel);
//
//        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(usersService, times(1)).checkToken(topicModel.getUser(), topicModel.getToken());
//        verify(topicsService, times(1)).existsById(topicModel.getId());
//        verify(topicsService, times(1)).findTopicsEntityById(topicModel.getId());
//        verifyNoMoreInteractions(usersService, topicsService, mailService);
//    }
//
//    @Test
//    void testEditTopic_ClosedTopic_ReturnsOk() {
//        OptionsData option1 = new OptionsData("Option 1", 0);
//        OptionsData option2 = new OptionsData("Option 2", 0);
//        List<OptionsData> options = Arrays.asList(option1, option2);
//
//        TopicsEntity topicEntity = new TopicsEntity();
//        topicEntity.setId(1);
//        topicEntity.setTitle("Topic 2");
//        topicEntity.setType(String.valueOf(Constants.TopicType.TEXT_SINGLE));
//        topicEntity.setQuestion("Question 1");
//        topicEntity.setOptions("{\"Option 1\": 1, \"Option 2\": 2}");
//        topicEntity.setAuthor("Author 1");
//        topicEntity.setMembers("[\"User 1\", \"User 2\"]");
//        topicEntity.setCloseDate(null);
//        topicEntity.setVisits(0);
//        topicEntity.setStatus(Constants.STATUS_CLOSED);
//
//        TopicsModel topicModel = new TopicsModel();
//        topicModel.setId(1);
//        topicModel.setTitle("Topic 1");
//        topicModel.setType(String.valueOf(Constants.TopicType.TEXT_SINGLE));
//        topicModel.setQuestion("Question 1");
//        topicModel.setOptions(options);
//        topicModel.setMembers(List.of("[\"User 1\", \"User 2\"]"));
//        topicModel.setUser("Author 1");
//        topicModel.setToken("validToken");
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "The topic is closed");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);
//
//        when(usersService.checkToken(topicModel.getUser(), topicModel.getToken())).thenReturn(true);
//        when(topicsService.existsById(topicModel.getId())).thenReturn(true);
//        when(topicsService.findTopicsEntityById(topicModel.getId())).thenReturn(topicEntity);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.editTopic(topicModel);
//
//        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(usersService, times(1)).checkToken(topicModel.getUser(), topicModel.getToken());
//        verify(topicsService, times(1)).existsById(topicModel.getId());
//        verify(topicsService, times(1)).findTopicsEntityById(topicModel.getId());
//        verifyNoMoreInteractions(usersService, topicsService, mailService);
//    }
//
//    @Test
//    void testEditTopic_DuplicateTopic_ReturnsOk() {
//        OptionsData option1 = new OptionsData("Option 1", 0);
//        OptionsData option2 = new OptionsData("Option 2", 0);
//        List<OptionsData> options = Arrays.asList(option1, option2);
//
//        TopicsEntity topicEntity = new TopicsEntity();
//        topicEntity.setId(1);
//        topicEntity.setTitle("Topic 1");
//        topicEntity.setType(String.valueOf(Constants.TopicType.TEXT_SINGLE));
//        topicEntity.setQuestion("Question 1");
//        topicEntity.setOptions("[{\"option\":\"Sí\",\"votes\":0},{\"option\":\"No\",\"votes\":0}]");
//        topicEntity.setAuthor("Author 1");
//        topicEntity.setMembers("[\"User 1\", \"User 2\"]");
//        topicEntity.setCloseDate(null);
//        topicEntity.setVisits(0);
//        topicEntity.setStatus(Constants.STATUS_OPENED);
//
//        TopicsModel topicModel = new TopicsModel();
//        topicModel.setId(1);
//        topicModel.setTitle("Topic 2");
//        topicModel.setType(String.valueOf(Constants.TopicType.TEXT_SINGLE));
//        topicModel.setQuestion("Question 1");
//        topicModel.setOptions(options);
//        topicModel.setMembers(List.of("[\"User 1\", \"User 2\"]"));
//        topicModel.setUser("Author 1");
//        topicModel.setToken("validToken");
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "There is already a topic assigned to the author with that name");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);
//
//        when(usersService.checkToken(topicModel.getUser(), topicModel.getToken())).thenReturn(true);
//        when(topicsService.existsById(topicModel.getId())).thenReturn(true);
//        when(topicsService.existsByTitleAndAuthor(topicModel.getTitle().strip(), topicModel.getUser())).thenReturn(true);
//        when(topicsService.findTopicsEntityById(topicModel.getId())).thenReturn(topicEntity);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.editTopic(topicModel);
//
//        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(usersService, times(1)).checkToken(topicModel.getUser(), topicModel.getToken());
//        verify(topicsService, times(1)).existsById(topicModel.getId());
//        verify(topicsService, times(1)).existsByTitleAndAuthor(topicModel.getTitle().strip(), topicModel.getUser());
//        verify(topicsService, times(1)).findTopicsEntityById(topicModel.getId());
//        verifyNoMoreInteractions(usersService, topicsService, mailService);
//    }
//
//    @Test
//    void testEditTopic_InvalidTopicType_ReturnsBadGateway() {
//        OptionsData option1 = new OptionsData("Option 1", 0);
//        OptionsData option2 = new OptionsData("Option 2", 0);
//        List<OptionsData> options = Arrays.asList(option1, option2);
//
//        TopicsEntity topicEntity = new TopicsEntity();
//        topicEntity.setId(1);
//        topicEntity.setTitle("Topic 2");
//        topicEntity.setType(String.valueOf(Constants.TopicType.TEXT_SINGLE));
//        topicEntity.setQuestion("Question 1");
//        topicEntity.setOptions("{\"Option 1\": 1, \"Option 2\": 2}");
//        topicEntity.setAuthor("Author 1");
//        topicEntity.setMembers("[\"User 1\", \"User 2\"]");
//        topicEntity.setCloseDate(null);
//        topicEntity.setVisits(0);
//        topicEntity.setStatus(Constants.STATUS_OPENED);
//
//        TopicsModel topicModel = new TopicsModel();
//        topicModel.setId(1);
//        topicModel.setTitle("Topic 1");
//        topicModel.setType("Type");
//        topicModel.setQuestion("Question 1");
//        topicModel.setOptions(options);
//        topicModel.setMembers(List.of("[\"User 1\", \"User 2\"]"));
//        topicModel.setUser("Author 1");
//        topicModel.setToken("validToken");
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "The topic type is not valid");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);
//
//        when(usersService.checkToken(topicModel.getUser(), topicModel.getToken())).thenReturn(true);
//        when(topicsService.existsById(topicModel.getId())).thenReturn(true);
//        when(topicsService.existsByTitleAndAuthor(topicModel.getTitle().strip(), topicModel.getUser())).thenReturn(false);
//        when(topicsService.findTopicsEntityById(topicModel.getId())).thenReturn(topicEntity);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.editTopic(topicModel);
//
//        assertEquals(HttpStatus.BAD_GATEWAY, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(usersService, times(1)).checkToken(topicModel.getUser(), topicModel.getToken());
//        verify(topicsService, times(1)).existsById(topicModel.getId());
//        verify(topicsService, times(1)).existsByTitleAndAuthor(topicModel.getTitle().strip(), topicModel.getUser());
//        verify(topicsService, times(1)).findTopicsEntityById(topicModel.getId());
//        verifyNoMoreInteractions(usersService, topicsService, mailService);
//    }
//
//    @Test
//    void testEditTopic_ImagesAndOptionsAreMandatory() {
//        OptionsData option1 = new OptionsData("Option 1", 0);
//        OptionsData option2 = new OptionsData("Option 2", 0);
//        List<OptionsData> options = Arrays.asList(option1, option2);
//
//        TopicsEntity topicEntity = new TopicsEntity();
//        topicEntity.setId(1);
//        topicEntity.setTitle("Topic 2");
//        topicEntity.setType(String.valueOf(Constants.TopicType.IMAGE_MULTIPLE));
//        topicEntity.setQuestion("Question 1");
//        topicEntity.setOptions("{\"Option 1\": 1, \"Option 2\": 2}");
//        topicEntity.setAuthor("Author 1");
//        topicEntity.setMembers("[\"User 1\", \"User 2\"]");
//        topicEntity.setCloseDate(null);
//        topicEntity.setVisits(0);
//        topicEntity.setStatus(Constants.STATUS_OPENED);
//
//        TopicsModel topicModel = new TopicsModel();
//        topicModel.setId(1);
//        topicModel.setTitle("Topic 1");
//        topicModel.setType(String.valueOf(Constants.TopicType.IMAGE_MULTIPLE));
//        topicModel.setQuestion("Question 1");
//        topicModel.setOptions(options);
//        topicModel.setMembers(List.of("[\"User 1\", \"User 2\"]"));
//        topicModel.setUser("Author 1");
//        topicModel.setToken("validToken");
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "It is mandatory to send the images and options for this type of topic");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);
//
//        when(usersService.checkToken(topicModel.getUser(), topicModel.getToken())).thenReturn(true);
//        when(topicsService.existsById(topicModel.getId())).thenReturn(true);
//        when(topicsService.existsByTitleAndAuthor(topicModel.getTitle().strip(), topicModel.getUser())).thenReturn(false);
//        when(topicsService.findTopicsEntityById(topicModel.getId())).thenReturn(topicEntity);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.editTopic(topicModel);
//
//        assertEquals(HttpStatus.BAD_GATEWAY, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(usersService, times(1)).checkToken(topicModel.getUser(), topicModel.getToken());
//        verify(topicsService, times(1)).existsById(topicModel.getId());
//        verify(topicsService, times(1)).existsByTitleAndAuthor(topicModel.getTitle().strip(), topicModel.getUser());
//        verify(topicsService, times(1)).findTopicsEntityById(topicModel.getId());
//        verifyNoMoreInteractions(usersService, topicsService, mailService);
//    }
//
//    @Test
//    void testEditTopic_DateFormatError_ReturnsBadRequest() {
//        OptionsData option1 = new OptionsData("Option 1", 0);
//        OptionsData option2 = new OptionsData("Option 2", 0);
//        List<OptionsData> options = Arrays.asList(option1, option2);
//
//        TopicsEntity topicEntity = new TopicsEntity();
//        topicEntity.setId(1);
//        topicEntity.setTitle("Topic 2");
//        topicEntity.setType(String.valueOf(Constants.TopicType.TEXT_SINGLE));
//        topicEntity.setQuestion("Question 1");
//        topicEntity.setOptions("{\"Option 1\": 1, \"Option 2\": 2}");
//        topicEntity.setAuthor("Author 1");
//        topicEntity.setMembers("[\"User 1\", \"User 2\"]");
//        topicEntity.setCloseDate(null);
//        topicEntity.setVisits(0);
//        topicEntity.setStatus(Constants.STATUS_OPENED);
//
//        TopicsModel topicModel = new TopicsModel();
//        topicModel.setId(1);
//        topicModel.setTitle("Topic 1");
//        topicModel.setType("Type");
//        topicModel.setQuestion("Question 1");
//        topicModel.setOptions(options);
//        topicModel.setMembers(List.of("[\"User 1\", \"User 2\"]"));
//        topicModel.setUser("Author 1");
//        topicModel.setToken("validToken");
//        topicModel.setCloseDate("19/07-2023");
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "The topic type is not valid");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.BAD_GATEWAY);
//
//        when(usersService.checkToken(topicModel.getUser(), topicModel.getToken())).thenReturn(true);
//        when(topicsService.existsById(topicModel.getId())).thenReturn(true);
//        when(topicsService.existsByTitleAndAuthor(topicModel.getTitle().strip(), topicModel.getUser())).thenReturn(false);
//        when(topicsService.findTopicsEntityById(topicModel.getId())).thenReturn(topicEntity);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.editTopic(topicModel);
//
//        assertEquals(HttpStatus.BAD_GATEWAY, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(usersService, times(1)).checkToken(topicModel.getUser(), topicModel.getToken());
//        verify(topicsService, times(1)).existsById(topicModel.getId());
//        verify(topicsService, times(1)).existsByTitleAndAuthor(topicModel.getTitle().strip(), topicModel.getUser());
//        verify(topicsService, times(1)).findTopicsEntityById(topicModel.getId());
//        verifyNoMoreInteractions(usersService, topicsService, mailService);
//    }
//
//    @Test
//    void testEditTopic_InternalServerError_ReturnsInternalServerError() throws IOException {
//        OptionsData option1 = new OptionsData("Option 1", 0);
//        OptionsData option2 = new OptionsData("Option 2", 0);
//        List<OptionsData> options = Arrays.asList(option1, option2);
//
//        TopicsEntity topicEntity = new TopicsEntity();
//        topicEntity.setId(1);
//        topicEntity.setTitle("Topic 2");
//        topicEntity.setType(String.valueOf(Constants.TopicType.TEXT_SINGLE));
//        topicEntity.setQuestion("Question 1");
//        topicEntity.setOptions("[{\"option\":\"Sí\",\"votes\":0},{\"option\":\"No\",\"votes\":0}]");
//        topicEntity.setAuthor("Author 1");
//        topicEntity.setMembers("[\"User 1\", \"User 2\"]");
//        topicEntity.setCloseDate(null);
//        topicEntity.setVisits(0);
//        topicEntity.setStatus(Constants.STATUS_OPENED);
//
//        TopicsModel topicModel = new TopicsModel();
//        topicModel.setId(1);
//        topicModel.setTitle("Topic 1");
//        topicModel.setType(String.valueOf(Constants.TopicType.TEXT_SINGLE));
//        topicModel.setQuestion("Question 1");
//        topicModel.setOptions(options);
//        topicModel.setMembers(List.of("[\"User 1\", \"User 2\"]"));
//        topicModel.setUser("Author 1");
//        topicModel.setToken("validToken");
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "An error occurred --> java.lang.NullPointerException: Members not found in the database");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.INTERNAL_SERVER_ERROR);
//
//        when(usersService.checkToken(topicModel.getUser(), topicModel.getToken())).thenReturn(true);
//        when(topicsService.existsById(topicModel.getId())).thenReturn(true);
//        when(topicsService.existsByTitleAndAuthor(topicModel.getTitle().strip(), topicModel.getUser())).thenReturn(false);
//        when(topicsService.initiateVoting(topicModel.getType(), topicModel.getOptions())).thenReturn("[\"Option 1\", \"Option 2\"]");
//        when(topicsService.findTopicsEntityById(topicModel.getId())).thenReturn(topicEntity);
//        when(topicsService.checkMailActivate()).thenReturn(true);
//        doThrow(new NullPointerException("Members not found in the database")).when(mailService).sendEmail(topicModel);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.editTopic(topicModel);
//
//        assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(usersService, times(1)).checkToken(topicModel.getUser(), topicModel.getToken());
//        verify(topicsService, times(1)).existsById(topicModel.getId());
//        verify(topicsService, times(1)).existsByTitleAndAuthor(topicModel.getTitle().strip(), topicModel.getUser());
//        verify(topicsService, times(1)).initiateVoting(topicModel.getType(), topicModel.getOptions());
//        verify(topicsService, times(1)).findTopicsEntityById(topicModel.getId());
//        verify(topicsService, times(1)).checkMailActivate();
//        verify(mailService, times(1)).sendEmail(topicModel);
//        verifyNoMoreInteractions(usersService, topicsService, mailService);
//    }
//
//    @Test
//    void testEditTopic_AllDataProvided_TopicEditedSuccessfully() throws IOException {
//        OptionsData option1 = new OptionsData("Option 1", 0);
//        OptionsData option2 = new OptionsData("Option 2", 0);
//        List<OptionsData> options = Arrays.asList(option1, option2);
//
//        TopicsEntity topicEntity = new TopicsEntity();
//        topicEntity.setId(1);
//        topicEntity.setTitle("Topic 2");
//        topicEntity.setType(String.valueOf(Constants.TopicType.TEXT_SINGLE));
//        topicEntity.setQuestion("Question 1");
//        topicEntity.setOptions("[{\"option\":\"Sí\",\"votes\":0},{\"option\":\"No\",\"votes\":0}]");
//        topicEntity.setAuthor("Author 1");
//        topicEntity.setMembers("[\"User 1\", \"User 2\"]");
//        topicEntity.setCloseDate(null);
//        topicEntity.setVisits(0);
//        topicEntity.setStatus(Constants.STATUS_OPENED);
//
//        TopicsModel topicModel = new TopicsModel();
//        topicModel.setId(1);
//        topicModel.setTitle("Topic 1");
//        topicModel.setType(String.valueOf(Constants.TopicType.TEXT_SINGLE));
//        topicModel.setQuestion("Question 1");
//        topicModel.setOptions(options);
//        topicModel.setMembers(List.of("[\"User 1\", \"User 2\"]"));
//        topicModel.setUser("Author 1");
//        topicModel.setToken("validToken");
//        topicModel.setCloseDate("12-07-2023");
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "Topic edited successfully");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);
//
//        when(usersService.checkToken(topicModel.getUser(), topicModel.getToken())).thenReturn(true);
//        when(topicsService.existsById(topicModel.getId())).thenReturn(true);
//        when(topicsService.findTopicsEntityById(topicModel.getId())).thenReturn(topicEntity);
//        when(topicsService.existsByTitleAndAuthor(topicModel.getTitle().strip(), topicModel.getUser())).thenReturn(false);
//        when(topicsService.initiateVoting(topicModel.getType(), topicModel.getOptions())).thenReturn("{\"Option 1\": 1, \"Option 2\": 2}");
//        when(topicsService.checkMailActivate()).thenReturn(true);
//        doNothing().when(topicsService).saveTopic(any(TopicsEntity.class));
//        doNothing().when(mailService).sendEmail(topicModel);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.editTopic(topicModel);
//
//        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(usersService, times(1)).checkToken(topicModel.getUser(), topicModel.getToken());
//        verify(topicsService, times(1)).existsById(topicModel.getId());
//        verify(topicsService, times(1)).findTopicsEntityById(topicModel.getId());
//        verify(topicsService, times(1)).existsByTitleAndAuthor(topicModel.getTitle().strip(), topicModel.getUser());
//        verify(topicsService, times(1)).initiateVoting(topicModel.getType(), topicModel.getOptions());
//        verify(topicsService, times(1)).checkMailActivate();
//        verify(mailService, times(1)).sendEmail(topicModel);
//        verify(topicsService, times(1)).saveTopic(topicEntity);
//        verifyNoMoreInteractions(usersService, topicsService, mailService);
//    }
//
//    @Test
//    void testCloseTopic_UnauthorizedUser() {
//        TopicsModel topicModel = new TopicsModel();
//        topicModel.setId(1);
//        topicModel.setUser("exampleUser");
//        topicModel.setToken("validToken");
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "Unauthorized user");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.NOT_FOUND);
//
//        when(usersService.checkToken(topicModel.getUser(), topicModel.getToken())).thenReturn(false);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.closeTopic(topicModel);
//
//        assertEquals(HttpStatus.NOT_FOUND, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(usersService, times(1)).checkToken(topicModel.getUser(), topicModel.getToken());
//        verifyNoMoreInteractions(usersService);
//    }
//
//    @Test
//    void testCloseTopic_TopicNotFound_ReturnsTopicNotFound() {
//        TopicsModel topicModel = new TopicsModel();
//        topicModel.setId(1);
//        topicModel.setUser("exampleUser");
//        topicModel.setToken("validToken");
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "There is no topic with that id");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);
//
//        when(usersService.checkToken(topicModel.getUser(), topicModel.getToken())).thenReturn(true);
//        when(topicsService.findTopicsEntityById(topicModel.getId())).thenReturn(null);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.closeTopic(topicModel);
//
//        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(usersService, times(1)).checkToken(topicModel.getUser(), topicModel.getToken());
//        verify(topicsService, times(1)).findTopicsEntityById(topicModel.getId());
//        verifyNoMoreInteractions(usersService, topicsService);
//    }
//
//    @Test
//    void testCloseTopic_UserNotAuthor_ReturnsUserNotAuthor() {
//        TopicsModel topicModel = new TopicsModel();
//        topicModel.setId(1);
//        topicModel.setUser("exampleUser");
//        topicModel.setToken("validToken");
//
//        TopicsEntity topicEntity = new TopicsEntity();
//        topicEntity.setId(topicModel.getId());
//        topicEntity.setAuthor("differentUser");
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "The user is not the author of the topic");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);
//
//        when(usersService.checkToken(topicModel.getUser(), topicModel.getToken())).thenReturn(true);
//        when(topicsService.findTopicsEntityById(topicModel.getId())).thenReturn(topicEntity);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.closeTopic(topicModel);
//
//        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(usersService, times(1)).checkToken(topicModel.getUser(), topicModel.getToken());
//        verify(topicsService, times(1)).findTopicsEntityById(topicModel.getId());
//        verifyNoMoreInteractions(usersService, topicsService);
//    }
//
//    @Test
//    void testCloseTopic_TopicAlreadyClosed_ReturnsTopicClosed() {
//        TopicsModel topicModel = new TopicsModel();
//        topicModel.setId(1);
//        topicModel.setUser("exampleUser");
//        topicModel.setToken("validToken");
//
//        TopicsEntity topicEntity = new TopicsEntity();
//        topicEntity.setId(topicModel.getId());
//        topicEntity.setAuthor(topicModel.getUser());
//        topicEntity.setStatus(Constants.STATUS_CLOSED);
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "The topic is currently closed");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);
//
//        when(usersService.checkToken(topicModel.getUser(), topicModel.getToken())).thenReturn(true);
//        when(topicsService.findTopicsEntityById(topicModel.getId())).thenReturn(topicEntity);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.closeTopic(topicModel);
//
//        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(usersService, times(1)).checkToken(topicModel.getUser(), topicModel.getToken());
//        verify(topicsService, times(1)).findTopicsEntityById(topicModel.getId());
//        verifyNoMoreInteractions(usersService, topicsService);
//    }
//
//    @Test
//    void testCloseTopic_TopicClosedSuccessfully() {
//        TopicsModel topicModel = new TopicsModel();
//        topicModel.setId(1);
//        topicModel.setUser("exampleUser");
//        topicModel.setToken("validToken");
//
//        TopicsEntity topicEntity = new TopicsEntity();
//        topicEntity.setId(topicModel.getId());
//        topicEntity.setAuthor(topicModel.getUser());
//        topicEntity.setStatus(Constants.STATUS_OPENED);
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "The topic has been closed");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);
//
//        when(usersService.checkToken(topicModel.getUser(), topicModel.getToken())).thenReturn(true);
//        when(topicsService.findTopicsEntityById(topicModel.getId())).thenReturn(topicEntity);
//        doNothing().when(topicsService).saveTopic(any(TopicsEntity.class));
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.closeTopic(topicModel);
//
//        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(usersService, times(1)).checkToken(topicModel.getUser(), topicModel.getToken());
//        verify(topicsService, times(1)).findTopicsEntityById(topicModel.getId());
//        verify(topicsService, times(1)).saveTopic(topicEntity);
//        verifyNoMoreInteractions(usersService, topicsService);
//    }
//
//    @Test
//    void testReOpenTopic_UnauthorizedUser() {
//        TopicsModel topicModel = new TopicsModel();
//        topicModel.setId(1);
//        topicModel.setUser("exampleUser");
//        topicModel.setToken("validToken");
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "Unauthorized user");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.NOT_FOUND);
//
//        when(usersService.checkToken(topicModel.getUser(), topicModel.getToken())).thenReturn(false);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.reOpenTopic(topicModel);
//
//        assertEquals(HttpStatus.NOT_FOUND, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(usersService, times(1)).checkToken(topicModel.getUser(), topicModel.getToken());
//        verifyNoMoreInteractions(usersService);
//    }
//
//    @Test
//    void testReOpenTopic_TopicNotFound_ReturnsTopicNotFound() {
//        TopicsModel topicModel = new TopicsModel();
//        topicModel.setId(1);
//        topicModel.setUser("exampleUser");
//        topicModel.setToken("validToken");
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "There is no topic with that id");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);
//
//        when(usersService.checkToken(topicModel.getUser(), topicModel.getToken())).thenReturn(true);
//        when(topicsService.findTopicsEntityById(topicModel.getId())).thenReturn(null);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.reOpenTopic(topicModel);
//
//        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(usersService, times(1)).checkToken(topicModel.getUser(), topicModel.getToken());
//        verify(topicsService, times(1)).findTopicsEntityById(topicModel.getId());
//        verifyNoMoreInteractions(usersService, topicsService);
//    }
//
//    @Test
//    void testReOpenTopic_UserNotAuthor_ReturnsUserNotAuthor() {
//        TopicsModel topicModel = new TopicsModel();
//        topicModel.setId(1);
//        topicModel.setUser("exampleUser");
//        topicModel.setToken("validToken");
//
//        TopicsEntity topicEntity = new TopicsEntity();
//        topicEntity.setId(topicModel.getId());
//        topicEntity.setAuthor("differentUser");
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "The user is not the author of the topic");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);
//
//        when(usersService.checkToken(topicModel.getUser(), topicModel.getToken())).thenReturn(true);
//        when(topicsService.findTopicsEntityById(topicModel.getId())).thenReturn(topicEntity);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.reOpenTopic(topicModel);
//
//        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(usersService, times(1)).checkToken(topicModel.getUser(), topicModel.getToken());
//        verify(topicsService, times(1)).findTopicsEntityById(topicModel.getId());
//        verifyNoMoreInteractions(usersService, topicsService);
//    }
//
//    @Test
//    void testReOpenTopic_TopicAlreadyClosed_ReturnsTopicClosed() {
//        TopicsModel topicModel = new TopicsModel();
//        topicModel.setId(1);
//        topicModel.setUser("exampleUser");
//        topicModel.setToken("validToken");
//
//        TopicsEntity topicEntity = new TopicsEntity();
//        topicEntity.setId(topicModel.getId());
//        topicEntity.setAuthor(topicModel.getUser());
//        topicEntity.setStatus(Constants.STATUS_OPENED);
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "The topic is currently open");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);
//
//        when(usersService.checkToken(topicModel.getUser(), topicModel.getToken())).thenReturn(true);
//        when(topicsService.findTopicsEntityById(topicModel.getId())).thenReturn(topicEntity);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.reOpenTopic(topicModel);
//
//        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(usersService, times(1)).checkToken(topicModel.getUser(), topicModel.getToken());
//        verify(topicsService, times(1)).findTopicsEntityById(topicModel.getId());
//        verifyNoMoreInteractions(usersService, topicsService);
//    }
//
//    @Test
//    void testReOpenTopic_TopicClosedSuccessfully() {
//        TopicsModel topicModel = new TopicsModel();
//        topicModel.setId(1);
//        topicModel.setUser("exampleUser");
//        topicModel.setToken("validToken");
//
//        TopicsEntity topicEntity = new TopicsEntity();
//        topicEntity.setId(topicModel.getId());
//        topicEntity.setAuthor(topicModel.getUser());
//        topicEntity.setStatus(Constants.STATUS_CLOSED);
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "Topic reopened");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);
//
//        when(usersService.checkToken(topicModel.getUser(), topicModel.getToken())).thenReturn(true);
//        when(topicsService.findTopicsEntityById(topicModel.getId())).thenReturn(topicEntity);
//        doNothing().when(topicsService).saveTopic(any(TopicsEntity.class));
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.reOpenTopic(topicModel);
//
//        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(usersService, times(1)).checkToken(topicModel.getUser(), topicModel.getToken());
//        verify(topicsService, times(1)).findTopicsEntityById(topicModel.getId());
//        verify(topicsService, times(1)).saveTopic(topicEntity);
//        verifyNoMoreInteractions(usersService, topicsService);
//    }
//
//    @Test
//    void testDeleteTopic_UnauthorizedUser() {
//        TopicsModel topicModel = new TopicsModel();
//        topicModel.setId(1);
//        topicModel.setUser("exampleUser");
//        topicModel.setToken("validToken");
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "Unauthorized user");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.NOT_FOUND);
//
//        when(usersService.checkToken(topicModel.getUser(), topicModel.getToken())).thenReturn(false);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.deleteTopic(topicModel);
//
//        assertEquals(HttpStatus.NOT_FOUND, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(usersService, times(1)).checkToken(topicModel.getUser(), topicModel.getToken());
//        verifyNoMoreInteractions(usersService);
//    }
//
//    @Test
//    void testDeleteTopic_TopicNotFound_ReturnsTopicNotFound() {
//        TopicsModel topicModel = new TopicsModel();
//        topicModel.setId(1);
//        topicModel.setUser("exampleUser");
//        topicModel.setToken("validToken");
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "There is no topic with that id");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);
//
//        when(usersService.checkToken(topicModel.getUser(), topicModel.getToken())).thenReturn(true);
//        when(topicsService.findTopicsEntityById(topicModel.getId())).thenReturn(null);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.deleteTopic(topicModel);
//
//        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(usersService, times(1)).checkToken(topicModel.getUser(), topicModel.getToken());
//        verify(topicsService, times(1)).findTopicsEntityById(topicModel.getId());
//        verifyNoMoreInteractions(usersService, topicsService);
//    }
//
//    @Test
//    void testDeleteTopic_UserNotAuthor_ReturnsUserNotAuthor() {
//        TopicsModel topicModel = new TopicsModel();
//        topicModel.setId(1);
//        topicModel.setUser("exampleUser");
//        topicModel.setToken("validToken");
//
//        TopicsEntity topicEntity = new TopicsEntity();
//        topicEntity.setId(topicModel.getId());
//        topicEntity.setAuthor("differentUser");
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "The user is not the author of the topic");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);
//
//        when(usersService.checkToken(topicModel.getUser(), topicModel.getToken())).thenReturn(true);
//        when(topicsService.findTopicsEntityById(topicModel.getId())).thenReturn(topicEntity);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.deleteTopic(topicModel);
//
//        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(usersService, times(1)).checkToken(topicModel.getUser(), topicModel.getToken());
//        verify(topicsService, times(1)).findTopicsEntityById(topicModel.getId());
//        verifyNoMoreInteractions(usersService, topicsService);
//    }
//
//    @Test
//    void testDeleteTopic_TopicDeletedSuccessfully() {
//        TopicsModel topicModel = new TopicsModel();
//        topicModel.setId(1);
//        topicModel.setUser("exampleUser");
//        topicModel.setToken("validToken");
//
//        TopicsEntity topicEntity = new TopicsEntity();
//        topicEntity.setId(topicModel.getId());
//        topicEntity.setAuthor(topicModel.getUser());
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "The topic has been deleted");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);
//
//        when(usersService.checkToken(topicModel.getUser(), topicModel.getToken())).thenReturn(true);
//        when(topicsService.findTopicsEntityById(topicModel.getId())).thenReturn(topicEntity);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.deleteTopic(topicModel);
//
//        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(usersService, times(1)).checkToken(topicModel.getUser(), topicModel.getToken());
//        verify(topicsService, times(1)).findTopicsEntityById(topicModel.getId());
//        verify(topicsService, times(1)).deleteTopic(topicModel.getId());
//        verifyNoMoreInteractions(usersService, topicsService);
//    }
//
//    @Test
//    void testGetAllTopicsData_NoTopics_ReturnsNoTopicsInDatabase() {
//        List<TopicsEntity> topicsList = new ArrayList<>();
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "There are no topics in database");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);
//
//        when(topicsService.getAllTopicsData()).thenReturn(topicsList);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.getAllTopicsData();
//
//        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(topicsService, times(1)).getAllTopicsData();
//        verifyNoMoreInteractions(topicsService);
//    }
//
//    @Test
//    void testGetAllTopicsData_TopicsExist_ReturnsTopicsData() {
//        List<TopicsEntity> topicsList = new ArrayList<>();
//        TopicsEntity topicEntity1 = new TopicsEntity();
//        topicEntity1.setId(1);
//        topicEntity1.setTitle("Topic 1");
//        topicEntity1.setType("Type 1");
//        topicEntity1.setQuestion("Question 1");
//        topicEntity1.setOptions("[{\"option\":\"Sí\",\"votes\":0},{\"option\":\"No\",\"votes\":0}]");
//        topicEntity1.setVotedBy("User 1");
//        topicEntity1.setAuthor("Author 1");
//        topicEntity1.setMembers("[\"Member 1\", \"Member 2\"]");
//        topicEntity1.setVisits(10);
//        topicEntity1.setStatus("Open");
//        topicsList.add(topicEntity1);
//
//        TopicsEntity topicEntity2 = new TopicsEntity();
//        topicEntity2.setId(2);
//        topicEntity2.setTitle("Topic 2");
//        topicEntity2.setType("Type 2");
//        topicEntity2.setQuestion("Question 2");
//        topicEntity2.setOptions("[{\"option\":\"Sí\",\"votes\":0},{\"option\":\"No\",\"votes\":0}]");
//        topicEntity2.setVotedBy("User 2");
//        topicEntity2.setAuthor("Author 2");
//        topicEntity2.setMembers("[\"Member 3\", \"Member 4\"]");
//        topicEntity2.setVisits(5);
//        topicEntity2.setStatus("Closed");
//        topicsList.add(topicEntity2);
//
//        when(topicsService.getAllTopicsData()).thenReturn(topicsList);
//
//        ResponseEntity<SpecialResponse> response = topicsController.getAllTopicsData();
//
//        assertEquals(HttpStatus.OK, response.getStatusCode());
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "OK");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);
//
//        assertEquals(expectedResponse.getBody().getMessage(), response.getBody().getMessage());
//
//        List<TopicsModel> expectedTopicsModelList = new ArrayList<>();
//        Gson gson = new Gson();
//        Type listType = new TypeToken<List<String>>() {}.getType();
//
//        for (TopicsEntity topicEntity : topicsList) {
//            TopicsModel topicModel = new TopicsModel();
//            topicModel.setId(topicEntity.getId());
//            topicModel.setTitle(topicEntity.getTitle());
//            topicModel.setType(topicEntity.getType());
//            topicModel.setQuestion(topicEntity.getQuestion());
//
//            List<OptionsData> expectedOptionsDataList = gson.fromJson(topicEntity.getOptions(), new TypeToken<List<OptionsData>>() {}.getType());
//            topicModel.setOptionsDataList(expectedOptionsDataList);
//
//            topicModel.setVotedBy(topicEntity.getVotedBy());
//            topicModel.setAuthor(topicEntity.getAuthor());
//            topicModel.setMembers(gson.fromJson(topicEntity.getMembers(), listType));
//            topicModel.setVisits(topicEntity.getVisits());
//            topicModel.setStatus(topicEntity.getStatus());
//
//            expectedTopicsModelList.add(topicModel);
//        }
//
//        List<TopicsModel> actualTopicsModelList = (List<TopicsModel>) Objects.requireNonNull(response.getBody()).getEntity();
//        assertEquals(expectedTopicsModelList.size(), actualTopicsModelList.size());
//
//        for (int i = 0; i < expectedTopicsModelList.size(); i++) {
//            TopicsModel expectedTopicModel = expectedTopicsModelList.get(i);
//            TopicsModel actualTopicModel = actualTopicsModelList.get(i);
//
//            assertEquals(expectedTopicModel.getId(), actualTopicModel.getId());
//            assertEquals(expectedTopicModel.getTitle(), actualTopicModel.getTitle());
//            assertEquals(expectedTopicModel.getType(), actualTopicModel.getType());
//            assertEquals(expectedTopicModel.getQuestion(), actualTopicModel.getQuestion());
//            assertEquals(expectedTopicModel.getOptionsDataList(), actualTopicModel.getOptionsDataList());
//            assertEquals(expectedTopicModel.getVotedBy(), actualTopicModel.getVotedBy());
//            assertEquals(expectedTopicModel.getAuthor(), actualTopicModel.getAuthor());
//            assertEquals(expectedTopicModel.getMembers(), actualTopicModel.getMembers());
//            assertEquals(expectedTopicModel.getVisits(), actualTopicModel.getVisits());
//            assertEquals(expectedTopicModel.getStatus(), actualTopicModel.getStatus());
//        }
//
//        verify(topicsService, times(1)).getAllTopicsData();
//        verifyNoMoreInteractions(topicsService);
//    }
//
//    @Test
//    void testVote_UserUnauthorized_ReturnsUnauthorizedUser() {
//        TopicsModel topicModel = new TopicsModel();
//        topicModel.setUser("User 1");
//        topicModel.setToken("invalidToken");
//        topicModel.setId(1);
//        topicModel.setVotation(Arrays.asList("Option 1"));
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "Unauthorized user");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.NOT_FOUND);
//
//        when(usersService.checkToken(topicModel.getUser(), topicModel.getToken())).thenReturn(false);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.vote(topicModel);
//
//        assertEquals(HttpStatus.NOT_FOUND, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(usersService, times(1)).checkToken(topicModel.getUser(), topicModel.getToken());
//        verifyNoMoreInteractions(usersService);
//    }
//
//    @Test
//    void testVote_TopicNotFound_ReturnsNoTopicWithThatId() {
//        TopicsModel topicModel = new TopicsModel();
//        topicModel.setUser("User 1");
//        topicModel.setToken("validToken");
//        topicModel.setId(1);
//        topicModel.setVotation(Arrays.asList("Option 1"));
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "There is no topic with that id");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);
//
//        when(usersService.checkToken(topicModel.getUser(), topicModel.getToken())).thenReturn(true);
//        when(topicsService.findTopicsEntityById(topicModel.getId())).thenReturn(null);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.vote(topicModel);
//
//        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(usersService, times(1)).checkToken(topicModel.getUser(), topicModel.getToken());
//        verify(topicsService, times(1)).findTopicsEntityById(topicModel.getId());
//        verifyNoMoreInteractions(usersService, topicsService);
//    }
//
//    @Test
//    void testVote_EmptyVotation_ReturnsVotingCannotBeEmpty() {
//        TopicsEntity topicEntity = new TopicsEntity();
//        topicEntity.setId(1);
//        topicEntity.setOptions("{\"Option 1\": 1, \"Option 2\": 2}");
//        topicEntity.setVotedBy(null);
//        topicEntity.setMembers("[\"Member 1\", \"Member 2\"]");
//        topicEntity.setAuthor("Author 1");
//
//        TopicsModel topicModel = new TopicsModel();
//        topicModel.setUser("User 1");
//        topicModel.setToken("validToken");
//        topicModel.setId(1);
//        topicModel.setVotation(Collections.emptyList());
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "The voting cannot be empty");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.BAD_GATEWAY);
//
//        when(usersService.checkToken(topicModel.getUser(), topicModel.getToken())).thenReturn(true);
//        when(topicsService.findTopicsEntityById(topicModel.getId())).thenReturn(topicEntity);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.vote(topicModel);
//
//        assertEquals(HttpStatus.BAD_GATEWAY, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(usersService, times(1)).checkToken(topicModel.getUser(), topicModel.getToken());
//        verify(topicsService, times(1)).findTopicsEntityById(topicModel.getId());
//        verifyNoMoreInteractions(usersService, topicsService);
//    }
//
//    @Test
//    void testVote_UserNotAllowedToVote_ReturnsUserNotAllowedToVote() {
//        TopicsEntity topicEntity = new TopicsEntity();
//        topicEntity.setId(1);
//        topicEntity.setOptions("{\"Option 1\": 1, \"Option 2\": 2}");
//        topicEntity.setVotedBy(null);
//        topicEntity.setMembers("[\"Member 1\", \"Member 2\"]");
//        topicEntity.setAuthor("Author 1");
//
//        TopicsModel topicModel = new TopicsModel();
//        topicModel.setUser("User 3");
//        topicModel.setToken("validToken");
//        topicModel.setId(1);
//        topicModel.setVotation(List.of("Option 1"));
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "The user is not allowed to vote on this topic");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);
//
//        when(usersService.checkToken(topicModel.getUser(), topicModel.getToken())).thenReturn(true);
//        when(topicsService.findTopicsEntityById(topicModel.getId())).thenReturn(topicEntity);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.vote(topicModel);
//
//        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(usersService, times(1)).checkToken(topicModel.getUser(), topicModel.getToken());
//        verify(topicsService, times(1)).findTopicsEntityById(topicModel.getId());
//        verifyNoMoreInteractions(usersService, topicsService);
//    }
//
//    @Test
//    void testVote_UserAlreadyVoted_ReturnsUserAlreadyVoted() {
//        TopicsEntity topicEntity = new TopicsEntity();
//        topicEntity.setId(1);
//        topicEntity.setType(String.valueOf(Constants.TopicType.TEXT_MULTIPLE));
//        topicEntity.setOptions("{\"Option 1\": 1, \"Option 2\": 2}");
//        topicEntity.setVotedBy("User 1");
//        topicEntity.setMembers("[\"User 1\", \"User 2\"]");
//        topicEntity.setAuthor("Author 1");
//
//        TopicsModel topicModel = new TopicsModel();
//        topicModel.setUser("User 1");
//        topicModel.setToken("validToken");
//        topicModel.setId(1);
//        topicModel.setVotation(List.of("Option 1"));
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "The user has already voted");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);
//
//        when(usersService.checkToken(topicModel.getUser(), topicModel.getToken())).thenReturn(true);
//        when(topicsService.findTopicsEntityById(topicModel.getId())).thenReturn(topicEntity);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.vote(topicModel);
//
//        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(usersService, times(1)).checkToken(topicModel.getUser(), topicModel.getToken());
//        verify(topicsService, times(1)).findTopicsEntityById(topicModel.getId());
//        verifyNoMoreInteractions(usersService, topicsService);
//    }
//
//    @Test
//    void testVote_InvalidTopicTypeForMultipleVotingOptions_ReturnsTopicTypeNotValid() {
//        TopicsEntity topicEntity = new TopicsEntity();
//        topicEntity.setId(1);
//        topicEntity.setType(String.valueOf(Constants.TopicType.TEXT_SINGLE));
//        topicEntity.setOptions("{\"Option 1\": 1, \"Option 2\": 2}");
//        topicEntity.setVotedBy(null);
//        topicEntity.setMembers("[\"User 1\", \"User 2\"]");
//        topicEntity.setAuthor("Author 1");
//
//        TopicsModel topicModel = new TopicsModel();
//        topicModel.setUser("User 1");
//        topicModel.setToken("validToken");
//        topicModel.setId(1);
//        topicModel.setVotation(Arrays.asList("Option 1", "Option 2"));
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "The topic type is not valid for multiple voting options");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.BAD_GATEWAY);
//
//        when(usersService.checkToken(topicModel.getUser(), topicModel.getToken())).thenReturn(true);
//        when(topicsService.findTopicsEntityById(topicModel.getId())).thenReturn(topicEntity);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.vote(topicModel);
//
//        assertEquals(HttpStatus.BAD_GATEWAY, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(usersService, times(1)).checkToken(topicModel.getUser(), topicModel.getToken());
//        verify(topicsService, times(1)).findTopicsEntityById(topicModel.getId());
//        verifyNoMoreInteractions(usersService, topicsService);
//    }
//
//    @Test
//    void testVote_InvalidVotesList_ReturnsListVotesDoesNotMatchOptions() {
//        TopicsEntity topicEntity = new TopicsEntity();
//        topicEntity.setId(1);
//        topicEntity.setType(String.valueOf(Constants.TopicType.TEXT_MULTIPLE));
//        topicEntity.setOptions("[{\"option\":\"Sí\",\"votes\":0},{\"option\":\"No\",\"votes\":0}]");
//        topicEntity.setVotedBy(null);
//        topicEntity.setMembers("[\"User 1\", \"User 2\"]");
//        topicEntity.setAuthor("Author 1");
//
//        TopicsModel topicModel = new TopicsModel();
//        topicModel.setUser("User 1");
//        topicModel.setToken("validToken");
//        topicModel.setId(1);
//        topicModel.setVotation(Arrays.asList("Option 3", "Option 4"));
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "The list of votes does not match the options of the topic");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.BAD_GATEWAY);
//
//        when(usersService.checkToken(topicModel.getUser(), topicModel.getToken())).thenReturn(true);
//        when(topicsService.findTopicsEntityById(topicModel.getId())).thenReturn(topicEntity);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.vote(topicModel);
//
//        assertEquals(HttpStatus.BAD_GATEWAY, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(usersService, times(1)).checkToken(topicModel.getUser(), topicModel.getToken());
//        verify(topicsService, times(1)).findTopicsEntityById(topicModel.getId());
//        verifyNoMoreInteractions(usersService, topicsService);
//    }
//
//    @Test
//    void testVote_UserAuthorized_ValidVote_ReturnsVotationUpdatedSuccessfully() {
//        TopicsEntity topicEntity = new TopicsEntity();
//        topicEntity.setId(1);
//        topicEntity.setType(String.valueOf(Constants.TopicType.TEXT_MULTIPLE));
//        topicEntity.setOptions("[{\"option\":\"Sí\",\"votes\":0},{\"option\":\"No\",\"votes\":0}]");
//        topicEntity.setVotedBy(null);
//        topicEntity.setMembers("[\"User 1\", \"User 2\"]");
//        topicEntity.setAuthor("Author 1");
//        topicEntity.setVisits(0);
//
//        TopicsModel topicModel = new TopicsModel();
//        topicModel.setUser("User 1");
//        topicModel.setToken("validToken");
//        topicModel.setId(1);
//        topicModel.setVotation(List.of("No"));
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "Votation updated successfully");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);
//
//        when(usersService.checkToken(topicModel.getUser(), topicModel.getToken())).thenReturn(true);
//        when(topicsService.findTopicsEntityById(topicModel.getId())).thenReturn(topicEntity);
//        doNothing().when(topicsService).saveTopic(any(TopicsEntity.class));
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.vote(topicModel);
//
//        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(usersService, times(1)).checkToken(topicModel.getUser(), topicModel.getToken());
//        verify(topicsService, times(1)).findTopicsEntityById(topicModel.getId());
//        verify(topicsService, times(1)).saveTopic(topicEntity);
//        verifyNoMoreInteractions(usersService, topicsService);
//    }
//
//    @Test
//    void testVotingResults_UserUnauthorized_ReturnsUnauthorizedUser() {
//        TopicsModel topicModel = new TopicsModel();
//        topicModel.setUser("User 1");
//        topicModel.setToken("invalidToken");
//        topicModel.setId(1);
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "Unauthorized user");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.NOT_FOUND);
//
//        when(usersService.checkToken(topicModel.getUser(), topicModel.getToken())).thenReturn(false);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.votingResults(topicModel);
//
//        assertEquals(HttpStatus.NOT_FOUND, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(usersService, times(1)).checkToken(topicModel.getUser(), topicModel.getToken());
//        verifyNoMoreInteractions(usersService);
//    }
//
//    @Test
//    void testVotingResults_TopicNotFound_ReturnsNoTopicWithThatId() {
//        TopicsModel topicModel = new TopicsModel();
//        topicModel.setUser("User 1");
//        topicModel.setToken("validToken");
//        topicModel.setId(1);
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "There is no topic with that id");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);
//
//        when(usersService.checkToken(topicModel.getUser(), topicModel.getToken())).thenReturn(true);
//        when(topicsService.findTopicsEntityById(topicModel.getId())).thenReturn(null);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.votingResults(topicModel);
//
//        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(usersService, times(1)).checkToken(topicModel.getUser(), topicModel.getToken());
//        verify(topicsService, times(1)).findTopicsEntityById(topicModel.getId());
//        verifyNoMoreInteractions(usersService, topicsService);
//    }
//
//    @Test
//    void testVotingResults_UserNotAllowedToViewResults_ReturnsUserNotAllowedToViewResults() {
//        TopicsEntity topicEntity = new TopicsEntity();
//        topicEntity.setId(1);
//        topicEntity.setStatus(Constants.STATUS_CLOSED);
//        topicEntity.setType("Type 1");
//        topicEntity.setOptions("{\"Option 1\": 1, \"Option 2\": 2}");
//        topicEntity.setMembers("[\"Member 2\", \"Member 3\"]");
//        topicEntity.setAuthor("Author 1");
//
//        TopicsModel topicModel = new TopicsModel();
//        topicModel.setUser("User 1");
//        topicModel.setToken("validToken");
//        topicModel.setId(1);
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "The user is not allowed to view the results on this topic");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);
//
//        when(usersService.checkToken(topicModel.getUser(), topicModel.getToken())).thenReturn(true);
//        when(topicsService.findTopicsEntityById(topicModel.getId())).thenReturn(topicEntity);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.votingResults(topicModel);
//
//        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(usersService, times(1)).checkToken(topicModel.getUser(), topicModel.getToken());
//        verify(topicsService, times(1)).findTopicsEntityById(topicModel.getId());
//        verifyNoMoreInteractions(usersService, topicsService);
//    }
//
//    @Test
//    void testVotingResults_TopicNotClosed_ReturnsTopicNotClosed() {
//        TopicsEntity topicEntity = new TopicsEntity();
//        topicEntity.setId(1);
//        topicEntity.setStatus(Constants.STATUS_OPENED);
//        topicEntity.setType("Type 1");
//        topicEntity.setOptions("{\"Option 1\": 1, \"Option 2\": 2}");
//        topicEntity.setMembers("[\"Member 1\", \"Member 2\"]");
//        topicEntity.setAuthor("Author 1");
//
//        TopicsModel topicModel = new TopicsModel();
//        topicModel.setUser("User 1");
//        topicModel.setToken("validToken");
//        topicModel.setId(1);
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "The topic is not closed, so it is not possible to view the results");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);
//
//        when(usersService.checkToken(topicModel.getUser(), topicModel.getToken())).thenReturn(true);
//        when(topicsService.findTopicsEntityById(topicModel.getId())).thenReturn(topicEntity);
//
//        ResponseEntity<SpecialResponse> actualResponse = topicsController.votingResults(topicModel);
//
//        assertEquals(HttpStatus.OK, actualResponse.getStatusCode());
//        assertEquals(Objects.requireNonNull(expectedResponse.getBody()).getMessage(), Objects.requireNonNull(actualResponse.getBody()).getMessage());
//        verify(usersService, times(1)).checkToken(topicModel.getUser(), topicModel.getToken());
//        verify(topicsService, times(1)).findTopicsEntityById(topicModel.getId());
//        verifyNoMoreInteractions(usersService, topicsService);
//    }
//
//    @Test
//    void testVotingResults_UserAuthorized_TopicExists_TopicClosed_ReturnsVotingResults() {
//        TopicsModel topicModel = new TopicsModel();
//        topicModel.setUser("User1");
//        topicModel.setToken("validToken");
//        topicModel.setId(1);
//
//        TopicsEntity topicEntity = new TopicsEntity();
//        topicEntity.setId(1);
//        topicEntity.setStatus(Constants.STATUS_CLOSED);
//        topicEntity.setType("Type 1");
//        topicEntity.setOptions("[{\"option\":\"Sí\",\"votes\":5},{\"option\":\"No\",\"votes\":3}]");
//        topicEntity.setMembers("[\"User1\", \"User2\"]");
//        topicEntity.setAuthor("User1");
//        topicEntity.setVisits(2);
//
//        when(usersService.checkToken(topicModel.getUser(), topicModel.getToken())).thenReturn(true);
//        when(topicsService.findTopicsEntityById(topicModel.getId())).thenReturn(topicEntity);
//        doNothing().when(topicsService).saveTopic(any(TopicsEntity.class));
//
//        ResponseEntity<SpecialResponse> response = topicsController.votingResults(topicModel);
//
//        assertEquals(HttpStatus.OK, response.getStatusCode());
//
//        JSONObject expectedResponseJson = new JSONObject();
//        expectedResponseJson.put("message", "Type 1");
//        ResponseEntity<SpecialResponse> expectedResponse = new ResponseEntity<>(topicsController.specialResponse(null, expectedResponseJson), HttpStatus.OK);
//
//        assertEquals(expectedResponse.getBody().getMessage(), response.getBody().getMessage());
//
//        List<OptionsData> expectedOptionsDataList = new Gson().fromJson(topicEntity.getOptions(), new TypeToken<List<OptionsData>>() {}.getType());
//        List<OptionsData> actualOptionsDataList = (List<OptionsData>) response.getBody().getEntity();
//
//        assertEquals(expectedOptionsDataList, actualOptionsDataList);
//
//        verify(usersService, times(1)).checkToken(topicModel.getUser(), topicModel.getToken());
//        verify(topicsService, times(1)).findTopicsEntityById(topicModel.getId());
//        verify(topicsService, times(1)).saveTopic(any(TopicsEntity.class));
//        verifyNoMoreInteractions(usersService, topicsService);
//    }
}
