package com.capgemini.ailabar.topics.infraestructure.controllers;

import com.capgemini.ailabar.commons.adapters.out.SpecialResponseInterface;
import com.capgemini.ailabar.commons.utils.SpecialResponse;
import com.capgemini.ailabar.options.domain.models.OptionsModel;
import com.capgemini.ailabar.topics.domain.exceptions.*;
import com.capgemini.ailabar.topics.application.services.TopicsService;
import com.capgemini.ailabar.topics.domain.models.TopicsModel;
import com.capgemini.ailabar.users.domain.models.UsersModel;
import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("/topics")
public class TopicsController implements SpecialResponseInterface {
    private final TopicsService topicsService;

    @Autowired
    public TopicsController(TopicsService topicsService) {
        this.topicsService = topicsService;
    }

    /*
     * RETURNS THE NUMBER OF SPECIFIED TOPICS SORTED FROM MOST RECENT TO OLDEST:
     * 1. Returns topics for which the user is the author (creator) and those in which the user is a member (authorized to vote).
     * 2. Adds the 'canVote' field, whose value is false if the user has already voted in this topic and true if they haven't.
     * 3. The 'elements' attribute is mandatory and indicates how many elements we want to display in the call (pagination). A value of 0 is accepted for 'elements', but
     *    an empty entity will be returned.
     * 4. The 'page' attribute is used to return the desired page and is not mandatory; the first page is returned by default if not specified. The first
     *    page will be 1; if 0 is sent, it will be treated as the first page.
     * 5. The 'status' attribute indicates whether the topic is open (1) or closed (0).
     * 6. There is also a field called 'filters,' which is a List<String> for filtering topics and can have values of "mines," "opened," "closed," and/or "votePending."
     */
    @PostMapping("loadTopics")
    public ResponseEntity<SpecialResponse> loadTopics(@RequestBody UsersModel usersModel) {
        JSONObject responseJson = new JSONObject();
        Map<String, Object> topicsEntityList = topicsService.loadTopics(usersModel);
        responseJson.put("message", "OK");
        return new ResponseEntity<>(specialResponse(topicsEntityList, responseJson), HttpStatus.OK);
    }

    /*
     * CREATES A TOPIC IN THE DATABASE:
     * 1. The user is internally transformed into the author of the topic.
     * 2. Options and group members will be sent as an array of strings.
     * 3. If you want to assign a specific group, you can pass its name using the 'groupName' attribute instead of 'members.'
     * 4. If 'members' is sent instead of a 'groupName,' a temporary group is generated and associated with this topic ('groupName' and 'members' cannot be sent together).
     * 5. A closing date is not mandatory, but if applied, it must be greater than the current date. Allowed date formats are
     *    [yyyy-MM-dd][yyyy/MM/dd][dd-MM-yyyy][dd/MM/yyyy][MM/dd/yyyy][yyyyMMdd], although they are stored in the database as yyyyMMdd.
     * 6. The 'Options' field should only contain 'image' if the type is IMAGE_SINGLE or IMAGE_MULTIPLE.
     */
    @PostMapping("/createTopic")
    public ResponseEntity<SpecialResponse> createTopic(@RequestBody TopicsModel topicsModel) {
        JSONObject responseJson = new JSONObject();
        topicsService.createTopic(topicsModel);
        responseJson.put("message", "Topic created successfully");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    /*
     * EDIT A TOPIC IN THE DATABASE:
     * 1. This is similar to createTopic, except it uses a PUT request, and the method is used to edit the topic with the indicated ID only if the user is the author.
     * 2. If the type or options of the topic are modified, the current votes are invalidated, resetting them to 0, and members who had voted are allowed to vote again.
     * 3. If the topic type is IMAGE, it is mandatory to send the 'image' field with its value in Base64 in addition to the 'option' field.
     * 4. If a closeDate exists in the database for the topic and no value is sent in editTopic or an empty value is sent, the close date in the database is removed.
     */
    @PutMapping("/editTopic")
    public ResponseEntity<SpecialResponse> editTopic(@RequestBody TopicsModel topicsModel) {
        JSONObject responseJson = new JSONObject();
        topicsService.editTopic(topicsModel);
        responseJson.put("message", "Topic edited successfully");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    /*
     * CLOSES AN OPEN TOPIC IN THE DATABASE:
     * 1. Changes the status of the topic with the provided ID to Closed (0), which means it can only be deleted or its results viewed from now on.
     */
    @PutMapping("/closeTopic")
    public ResponseEntity<SpecialResponse> closeTopic(@RequestBody TopicsModel topicModel) {
        JSONObject responseJson = new JSONObject();
        topicsService.closeTopic(topicModel);
        responseJson.put("message", "The topic has been closed");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    /*
     * REOPENS A CLOSED TOPIC IN THE DATABASE:
     * 1. Changes the status of the topic with the provided ID to Open (1).
     */
    @PutMapping("/reOpenTopic")
    public ResponseEntity<SpecialResponse> reOpenTopic(@RequestBody TopicsModel topicsModel) {
        JSONObject responseJson = new JSONObject();
        topicsService.reOpenTopic(topicsModel);
        responseJson.put("message", "Topic reopened");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    /*
     * DELETES A TOPIC FROM THE DATABASE:
     * 1. Deletes the topic with the specified ID, along with its references to intermediary tables such as voting options, the members who have voted, and the temporary group assigned if it had one.
     */
    @DeleteMapping("/deleteTopic")
    public ResponseEntity<SpecialResponse> deleteTopic(@RequestBody TopicsModel topicsModel) {
        JSONObject responseJson = new JSONObject();
        topicsService.deleteTopic(topicsModel);
        responseJson.put("message", "The topic has been deleted");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    /* Exclusive for development purposes */
    /*
     * RETURNS ALL DATA FOR ALL TOPICS FROM THE DATABASE (EXCLUSIVE FOR DEVELOPMENT TESTING, SHOULD NOT BE INCLUDED IN THE FINAL VERSION)
     */
    @GetMapping("/getTopicsDatabase")
    public ResponseEntity<SpecialResponse> getTopicsDatabase() {
        JSONObject responseJson = new JSONObject();
        List<TopicsModel> topicsEntityList = topicsService.getTopicsDatabase();
        responseJson.put("message", "OK");
        return new ResponseEntity<>(specialResponse(topicsEntityList, responseJson), HttpStatus.OK);
    }
    /* End of method exclusively for development purposes */

    /*
     * USED TO RECORD USER VOTES:
     * 1. The method adds 1 to the option or options selected by the user, provided that the user has not voted previously.
     * 2. It's important that even if there's only one option, it should be sent as an array of strings.
     * 3. If the voting options sent do not match those available in the topic (which should not occur), an error will be returned.
     */
    @PutMapping("/vote")
    public ResponseEntity<SpecialResponse> vote(@RequestBody TopicsModel topicsModel) {
        JSONObject responseJson = new JSONObject();
        topicsService.vote(topicsModel);
        responseJson.put("message", "Votation updated successfully");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    /*
     * RETURNS VOTING RESULTS:
     * 1. Returns the options and the votes assigned to them only if the topic is in Closed (0) state.
     * 2. If the topic type is AS (Advanced Search), the 'image' field with the user's photo from the database will also be returned if the user exists and has a photo.
     */
    @PostMapping("/votingResults")
    public ResponseEntity<SpecialResponse> votingResults(@RequestBody TopicsModel topicsModel) {
        JSONObject responseJson = new JSONObject();
        Map<String, List<OptionsModel>> mapTopicTypeAndOptionsModelList = topicsService.votingResults(topicsModel);
        String topicType = mapTopicTypeAndOptionsModelList.keySet().stream().findFirst().orElse(null);
        List<OptionsModel> optionsModelList = mapTopicTypeAndOptionsModelList.values().stream().findFirst().orElse(null);
        responseJson.put("message", topicType);
        return new ResponseEntity<>(specialResponse(optionsModelList, responseJson), HttpStatus.OK);
    }

    // Exception handling for each use case
    @ExceptionHandler(LoadTopicException.class)
    ResponseEntity<SpecialResponse> handlerLoadTopicException (LoadTopicException loadTopicException){
        JSONObject responseJson = new JSONObject();
        responseJson.put("message", loadTopicException.getMessage());
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.INTERNAL_SERVER_ERROR);
    }

    @ExceptionHandler(CreateTopicException.class)
    ResponseEntity<SpecialResponse> handlerCreateTopicException (CreateTopicException createTopicException){
        JSONObject responseJson = new JSONObject();
        responseJson.put("message", createTopicException.getMessage());
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.INTERNAL_SERVER_ERROR);
    }

    @ExceptionHandler(EditTopicException.class)
    ResponseEntity<SpecialResponse> handlerEditTopicException (EditTopicException editTopicException){
        JSONObject responseJson = new JSONObject();
        responseJson.put("message", editTopicException.getMessage());
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.INTERNAL_SERVER_ERROR);
    }

    @ExceptionHandler(CloseTopicException.class)
    ResponseEntity<SpecialResponse> handlerCloseTopicException (CloseTopicException closeTopicException){
        JSONObject responseJson = new JSONObject();
        responseJson.put("message", closeTopicException.getMessage());
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.INTERNAL_SERVER_ERROR);
    }

    @ExceptionHandler(ReOpenTopicException.class)
    ResponseEntity<SpecialResponse> handlerReOpenTopicException (ReOpenTopicException reOpenTopicException){
        JSONObject responseJson = new JSONObject();
        responseJson.put("message", reOpenTopicException.getMessage());
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.INTERNAL_SERVER_ERROR);
    }

    @ExceptionHandler(VoteTopicException.class)
    ResponseEntity<SpecialResponse> handlerVoteTopicException (VoteTopicException voteException){
        JSONObject responseJson = new JSONObject();
        responseJson.put("message", voteException.getMessage());
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.INTERNAL_SERVER_ERROR);
    }

    @ExceptionHandler(VotingResultsTopicException.class)
    ResponseEntity<SpecialResponse> handlerVotingResultsTopicException (VotingResultsTopicException votingResultsTopicException){
        JSONObject responseJson = new JSONObject();
        responseJson.put("message", votingResultsTopicException.getMessage());
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.INTERNAL_SERVER_ERROR);
    }

    @ExceptionHandler(MailServiceException.class)
    ResponseEntity<SpecialResponse> handlerMailServiceException (MailServiceException mailServiceException){
        JSONObject responseJson = new JSONObject();
        responseJson.put("message", mailServiceException.getMessage());
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.INTERNAL_SERVER_ERROR);
    }
}
