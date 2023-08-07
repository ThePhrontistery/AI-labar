package com.capgemini.ailabar.topics.infraestructure.controllers;

import com.capgemini.ailabar.commons.adapters.out.SpecialResponseInterface;
import com.capgemini.ailabar.commons.utils.SpecialResponse;
import com.capgemini.ailabar.topics.domain.exceptions.*;
import com.capgemini.ailabar.topics.application.services.TopicsService;
import com.capgemini.ailabar.topics.domain.models.TopicsModel;
import com.capgemini.ailabar.topics.infraestructure.entities.TopicsEntity;
import com.capgemini.ailabar.users.domain.models.UsersModel;
import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

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
        List<TopicsEntity> topicsEntityList = topicsService.loadTopics(usersModel);
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
        List<TopicsEntity> topicsEntityList = topicsService.getTopicsDatabase();
        responseJson.put("message", "OK");
        return new ResponseEntity<>(specialResponse(topicsEntityList, responseJson), HttpStatus.OK);
    }
    /* Fin del método únicamente para desarrollo */

    /* Revisar en la nueva versión ya que cambiará */
//    @PutMapping("/vote")
//    public ResponseEntity<SpecialResponse> vote(@RequestBody TopicsModel topicModel) {
//        JSONObject responseJson = new JSONObject();
//
//        if(Boolean.FALSE.equals(usersService.checkAuthorization(topicModel.getUser(), topicModel.getToken()))) {
//            responseJson.put("message", "Unauthorized user");
//            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.NOT_FOUND);
//        }
//
//        TopicsEntity topicEntity = topicsService.findTopicsEntityById(topicModel.getId());
//
//        if(topicEntity == null) {
//            responseJson.put("message", "There is no topic with that id");
//            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
//        }
//
//        if(topicModel.getVotation().isEmpty()) {
//            responseJson.put("message", "The voting cannot be empty");
//            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.BAD_GATEWAY);
//        }
//
//        if(!topicEntity.getMembers().contains(topicModel.getUser()) && !topicEntity.getAuthor().equals(topicModel.getUser())) {
//            responseJson.put("message", "The user is not allowed to vote on this topic");
//            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
//        }
//
//        if(topicEntity.getVotedBy() != null && topicEntity.getVotedBy().contains(topicModel.getUser())) {
//            responseJson.put("message", "The user has already voted");
//            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
//        }
//
//        Constants.TopicType topicType = Constants.TopicType.valueOf(topicEntity.getType());
//        if (topicModel.getVotation().size() > 1 && !(topicType == Constants.TopicType.TEXT_MULTIPLE || topicType == Constants.TopicType.IMAGE_MULTIPLE)) {
//            responseJson.put("message", "The topic type is not valid for multiple voting options");
//            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.BAD_GATEWAY);
//        }
//
//        String vote = updateVotation(topicEntity.getOptions(), topicModel.getVotation());
//
//        if(vote.equals("KO")) {
//            responseJson.put("message", "The list of votes does not match the options of the topic");
//            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.BAD_GATEWAY);
//        }
//
//        topicEntity.setVisits(topicEntity.getVisits() + 1);
//        topicEntity.setOptions(vote);
//
//        if(topicEntity.getVotedBy() == null) {
//            topicEntity.setVotedBy(topicModel.getUser());
//        } else {
//            topicEntity.setVotedBy(topicEntity.getVotedBy().concat(", ").concat(topicModel.getUser()));
//        }
//
//        topicsService.saveTopic(topicEntity);
//        responseJson.put("message", "Votation updated successfully");
//        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
//    }
//
//    @PostMapping("/votingResults")
//    public ResponseEntity<SpecialResponse> votingResults(@RequestBody TopicsModel topicModel) {
//        JSONObject responseJson = new JSONObject();
//
//        if(Boolean.FALSE.equals(usersService.checkAuthorization(topicModel.getUser(), topicModel.getToken()))) {
//            responseJson.put("message", "Unauthorized user");
//            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.NOT_FOUND);
//        }
//
//        TopicsEntity topicEntity = topicsService.findTopicsEntityById(topicModel.getId());
//
//        if(topicEntity == null) {
//            responseJson.put("message", "There is no topic with that id");
//            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
//        }
//
//        if(topicEntity.getStatus().equals(Constants.STATUS_OPENED)) {
//            responseJson.put("message", "The topic is not closed, so it is not possible to view the results");
//            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
//        }
//
//        if(!topicEntity.getMembers().contains(topicModel.getUser()) && !topicEntity.getAuthor().equals(topicModel.getUser())) {
//            responseJson.put("message", "The user is not allowed to view the results on this topic");
//            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
//        }
//
//        topicEntity.setVisits(topicEntity.getVisits() + 1);
//        topicsService.saveTopic(topicEntity);
//
//        Gson gson = new Gson();
//        List<OptionsData> optionsDataList = gson.fromJson(topicEntity.getOptions(), new TypeToken<List<OptionsData>>() {}.getType());
//        if(topicEntity.getType().equals(String.valueOf(Constants.TopicType.AS))) {
//            optionsDataList = usersService.getUsersPhotos(optionsDataList);
//        }
//        topicModel.setOptionsDataList(optionsDataList);
//
//        responseJson.put("message", topicEntity.getType());
//        return new ResponseEntity<>(specialResponse(optionsDataList, responseJson), HttpStatus.OK);
//    }
//
//
//
//    private boolean containsExactMatch(List<String> userList, String userToFind) {
//        return userList.stream().anyMatch(user -> user.equals(userToFind));
//    }
//
//
//

//    private String updateVotation(String options, List<String> votation) {
//        boolean coincidence = false;
//
//        Gson gson = new Gson();
//        List<OptionsData> optionsDataList = gson.fromJson(options, new TypeToken<List<OptionsData>>() {}.getType());
//
//        for (OptionsData optionsData : optionsDataList) {
//            if (votation.contains(optionsData.getOption())) {
//                optionsData.setVotes(optionsData.getVotes() + 1);
//                coincidence = true;
//            }
//        }
//
//        if (coincidence) {
//            return gson.toJson(optionsDataList);
//        } else {
//            return "KO";
//        }
//    }
//
//

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

    @ExceptionHandler(MailServiceException.class)
    ResponseEntity<SpecialResponse> handlerMailServiceException (MailServiceException mailServiceException){
        JSONObject responseJson = new JSONObject();
        responseJson.put("message", mailServiceException.getMessage());
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.INTERNAL_SERVER_ERROR);
    }
}
