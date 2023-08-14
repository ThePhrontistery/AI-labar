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

    @PostMapping("loadTopics")
    public ResponseEntity<SpecialResponse> loadTopics(@RequestBody UsersModel usersModel) {
        JSONObject responseJson = new JSONObject();
        Map<String, Object> topicsEntityList = topicsService.loadTopics(usersModel);
        responseJson.put("message", "OK");
        return new ResponseEntity<>(specialResponse(topicsEntityList, responseJson), HttpStatus.OK);
    }

    @PostMapping("/createTopic")
    public ResponseEntity<SpecialResponse> createTopic(@RequestBody TopicsModel topicsModel) {
        JSONObject responseJson = new JSONObject();
        topicsService.createTopic(topicsModel);
        responseJson.put("message", "Topic created successfully");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    @PutMapping("/editTopic")
    public ResponseEntity<SpecialResponse> editTopic(@RequestBody TopicsModel topicsModel) {
        JSONObject responseJson = new JSONObject();
        topicsService.editTopic(topicsModel);
        responseJson.put("message", "Topic edited successfully");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    @PutMapping("/closeTopic")
    public ResponseEntity<SpecialResponse> closeTopic(@RequestBody TopicsModel topicModel) {
        JSONObject responseJson = new JSONObject();
        topicsService.closeTopic(topicModel);
        responseJson.put("message", "The topic has been closed");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    @PutMapping("/reOpenTopic")
    public ResponseEntity<SpecialResponse> reOpenTopic(@RequestBody TopicsModel topicsModel) {
        JSONObject responseJson = new JSONObject();
        topicsService.reOpenTopic(topicsModel);
        responseJson.put("message", "Topic reopened");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    @DeleteMapping("/deleteTopic")
    public ResponseEntity<SpecialResponse> deleteTopic(@RequestBody TopicsModel topicsModel) {
        JSONObject responseJson = new JSONObject();
        topicsService.deleteTopic(topicsModel);
        responseJson.put("message", "The topic has been deleted");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    /* Método exclusivo para desarrollo */
    @GetMapping("/getTopicsDatabase")
    public ResponseEntity<SpecialResponse> getTopicsDatabase() {
        JSONObject responseJson = new JSONObject();
        List<TopicsModel> topicsEntityList = topicsService.getTopicsDatabase();
        responseJson.put("message", "OK");
        return new ResponseEntity<>(specialResponse(topicsEntityList, responseJson), HttpStatus.OK);
    }
    /* Fin del método únicamente para desarrollo */

    @PutMapping("/vote")
    public ResponseEntity<SpecialResponse> vote(@RequestBody TopicsModel topicsModel) {
        JSONObject responseJson = new JSONObject();
        topicsService.vote(topicsModel);
        responseJson.put("message", "Votation updated successfully");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    @PostMapping("/votingResults")
    public ResponseEntity<SpecialResponse> votingResults(@RequestBody TopicsModel topicsModel) {
        JSONObject responseJson = new JSONObject();
        Map<String, List<OptionsModel>> mapTopicTypeAndOptionsModelList = topicsService.votingResults(topicsModel);
        String topicType = mapTopicTypeAndOptionsModelList.keySet().stream().findFirst().orElse(null);
        List<OptionsModel> optionsModelList = mapTopicTypeAndOptionsModelList.values().stream().findFirst().orElse(null);
        responseJson.put("message", topicType);
        return new ResponseEntity<>(specialResponse(optionsModelList, responseJson), HttpStatus.OK);

    }

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
