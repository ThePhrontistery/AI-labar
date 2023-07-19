package com.capgemini.beni.ailabar.controller;

import com.capgemini.beni.ailabar.dto.TopicsDto;
import com.capgemini.beni.ailabar.dto.UsersDto;
import com.capgemini.beni.ailabar.entity.TopicsEntity;
import com.capgemini.beni.ailabar.entity.UsersEntity;
import com.capgemini.beni.ailabar.service.MailService;
import com.capgemini.beni.ailabar.service.TopicsService;
import com.capgemini.beni.ailabar.service.UsersService;
import com.capgemini.beni.ailabar.utils.Constants;
import com.capgemini.beni.ailabar.utils.SpecialResponseInterface;
import com.capgemini.beni.ailabar.utils.SpecialResponse;
import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
import net.minidev.json.JSONArray;
import org.apache.commons.codec.digest.DigestUtils;
import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.lang.reflect.Type;
import java.util.*;

@RestController
@RequestMapping("/topics")
public class TopicsController implements SpecialResponseInterface {
    private final TopicsService topicsService;
    private final UsersService usersService;
    private final MailService mailService;

    @Autowired
    public TopicsController(TopicsService topicsService, UsersService usersService, MailService mailService) {
        this.topicsService = topicsService;
        this.usersService = usersService;
        this.mailService = mailService;
    }

    @PostMapping("/login")
    public ResponseEntity<SpecialResponse> login(@RequestBody UsersDto userDto) {
        JSONObject responseJson = new JSONObject();

        if (userDto.getUser().isBlank() || userDto.getPassword().isBlank()) {
            responseJson.put("message", "User and password are required to login");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.BAD_GATEWAY);
        }

        if (Boolean.FALSE.equals(topicsService.login(userDto.getUser(), DigestUtils.sha256Hex(userDto.getPassword())))) {
            responseJson.put("message", "Login failed");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.UNAUTHORIZED);
        }

        UsersEntity userEntity = usersService.findByUser(userDto.getUser());

        if(userEntity == null) {
            responseJson.put("message", "User not found");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.NOT_FOUND);
        }

        responseJson.put("message", "Login successful");
        return new ResponseEntity<>(specialResponse(userEntity.getToken(), responseJson), HttpStatus.OK);
    }

    @PostMapping("loadTopics")
    public ResponseEntity<SpecialResponse> loadTopics(@RequestBody UsersDto userDto) {
        JSONObject responseJson = new JSONObject();

        if(Boolean.FALSE.equals(usersService.checkToken(userDto.getUser(), userDto.getToken()))) {
            responseJson.put("message", "Unauthorized user");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.NOT_FOUND);
        }

        List<TopicsEntity> topicsList = topicsService.loadTopics(userDto.getUser());

        if(topicsList.isEmpty()) {
            responseJson.put("message", "There are no topics related to the user");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
        }

        List<TopicsDto> topicsDtoList = new ArrayList<>();
        Gson gson = new Gson();
        Type listType = new TypeToken<List<String>>() {}.getType();

        topicsList.forEach(topic -> {
            TopicsDto topicDto = new TopicsDto();
            topicDto.setId(topic.getId());
            topicDto.setTitle(topic.getTitle());
            topicDto.setType(topic.getType());
            topicDto.setQuestion(topic.getQuestion());
            String options = getOnlyOptions(topic.getOptions());
            topicDto.setOptions(gson.fromJson(options, listType));
            topicDto.setAuthor(topic.getAuthor());
            topicDto.setMembers(gson.fromJson(topic.getMembers(), listType));
            topicDto.setVisits(topic.getVisits());
            topicDto.setStatus(topic.getStatus());
            topicsDtoList.add(topicDto);
        });

        responseJson.put("message", "OK");
        return new ResponseEntity<>(specialResponse(topicsDtoList, responseJson), HttpStatus.OK);
    }

    @PostMapping("openTopic")
    public ResponseEntity<SpecialResponse> openTopic(@RequestBody TopicsDto topicDto) {JSONObject responseJson = new JSONObject();

        if(Boolean.FALSE.equals(usersService.checkToken(topicDto.getUser(), topicDto.getToken()))) {
            responseJson.put("message", "Unauthorized user");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.NOT_FOUND);
        }

        TopicsEntity topicEntity = topicsService.getTopicForEdit(topicDto.getId());

        if(topicEntity == null) {
            responseJson.put("message", "There is no topic with that id");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
        }

        if(topicEntity.getStatus().equals(Constants.STATUS_CLOSED)) {
            responseJson.put("message", "The topic is closed");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
        }

        if(!topicEntity.getAuthor().equals(topicDto.getUser())) {
            responseJson.put("message", "The user is not the author of the topic");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
        }

        topicEntity.setVisits(topicEntity.getVisits() + 1);
        topicsService.saveTopic(topicEntity);

        Gson gson = new Gson();
        Type listType = new TypeToken<List<String>>() {}.getType();

        TopicsDto topicsDto = new TopicsDto();
        topicsDto.setId(topicEntity.getId());
        topicsDto.setTitle(topicEntity.getTitle());
        topicsDto.setType(topicEntity.getType());
        topicsDto.setQuestion(topicEntity.getQuestion());
        String options = getOnlyOptions(topicEntity.getOptions());
        topicsDto.setOptions(gson.fromJson(options, listType));
        topicsDto.setAuthor(topicEntity.getAuthor());
        topicsDto.setMembers(gson.fromJson(topicEntity.getMembers(), listType));
        topicsDto.setVisits(topicEntity.getVisits());
        topicsDto.setStatus(topicEntity.getStatus());

        responseJson.put("message", "OK");
        return new ResponseEntity<>(specialResponse(topicsDto, responseJson), HttpStatus.OK);
    }

    /* Hay que trabajar las fechas de cierre y crear una forma de que se cierren automáticamente *//* Hay que trabajar las fechas de cierre y crear una forma de que se cierren automáticamente */
    @PostMapping("/createTopic")
    public ResponseEntity<SpecialResponse> createTopic(@RequestBody TopicsDto topicDto) {
        try {
            JSONObject responseJson = new JSONObject();

            if(topicDto.getTitle().isBlank() || topicDto.getType().isBlank() || topicDto.getQuestion().isBlank()
                    || topicDto.getOptions().isEmpty() || topicDto.getUser().isBlank() || topicDto.getMembers().isEmpty()) {
                responseJson.put("message", "All data is required to edit a topic");
                return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.BAD_GATEWAY);
            }

            if(Boolean.FALSE.equals(usersService.checkToken(topicDto.getUser(), topicDto.getToken()))) {
                responseJson.put("message", "Unauthorized user");
                return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.NOT_FOUND);
            }

            if(Boolean.TRUE.equals(topicsService.existsByTitleAndAuthor(topicDto.getTitle().strip(), topicDto.getUser()))) {
                responseJson.put("message", "There is already a topic assigned to the author with that name");
                return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
            }

            topicDto.setTitle(topicDto.getTitle().strip());
            topicDto.setAuthor(topicDto.getUser().strip());
            if(checkTopicType(topicDto.getType()).equals("KO")) {
                responseJson.put("message", "The topic type is not valid");
                return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.BAD_GATEWAY);
            }
            topicDto.setType(topicDto.getType());
            topicDto.setVisits(topicDto.getVisits() != null ? topicDto.getVisits() : 0);
            topicDto.setStatus(topicDto.getStatus() != null ? topicDto.getStatus() : Constants.STATUS_OPENED);

            TopicsEntity topicEntity = new TopicsEntity(topicDto);
            topicEntity.setOptions(topicsService.initiateVoting(topicDto.getOptions()));
            topicEntity.setMembers(new Gson().toJson(topicDto.getMembers()));

            mailService.sendEmail(topicDto);

            topicsService.saveTopic(topicEntity);
            responseJson.put("message", "Topic created successfully");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
        } catch (Exception e) {
            JSONObject responseJson = new JSONObject();
            responseJson.put("message", "An error occurred --> " + e);
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }

    /* Hay que trabajar las fechas de cierre y crear una forma de que se cierren automáticamente */
    @PutMapping("/editTopic")
    public ResponseEntity<SpecialResponse> editTopic(@RequestBody TopicsDto topicDto) {
        try {
            JSONObject responseJson = new JSONObject();

            if(topicDto.getId() == null || topicDto.getTitle().isBlank() || topicDto.getType().isBlank() || topicDto.getQuestion().isBlank()
                    || topicDto.getOptions().isEmpty() || topicDto.getMembers().isEmpty() || topicDto.getUser().isBlank()) {
                responseJson.put("message", "All data is required to edit a topic");
                return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.BAD_GATEWAY);
            }

            if(Boolean.FALSE.equals(usersService.checkToken(topicDto.getUser(), topicDto.getToken()))) {
                responseJson.put("message", "Unauthorized user");
                return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.NOT_FOUND);
            }

            if(Boolean.TRUE.equals(topicsService.existsById(topicDto.getId()))) {
                TopicsEntity topicEntity = topicsService.findTopicsEntityById(topicDto.getId());

                if (!topicDto.getUser().equals(topicEntity.getAuthor())) {
                    responseJson.put("message", "The user is not the author of the topic");
                    return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
                }

                if(topicEntity.getStatus().equals(Constants.STATUS_CLOSED)) {
                    responseJson.put("message", "The topic is closed");
                    return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
                }

                if(Boolean.TRUE.equals(topicsService.existsByTitleAndAuthor(topicDto.getTitle().strip(), topicDto.getUser()))) {
                    responseJson.put("message", "There is already a topic assigned to the author with that name");
                    return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
                } else {
                    topicEntity.setTitle(topicDto.getTitle().strip());
                }

                if(checkTopicType(topicDto.getType()).equals("KO")) {
                    responseJson.put("message", "The topic type is not valid");
                    return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.BAD_GATEWAY);
                }

                topicEntity.setQuestion(topicDto.getQuestion());

                if(!topicDto.getOptions().toString().equals(getOnlyOptions(topicEntity.getOptions())) || !topicDto.getType().equals(topicEntity.getType())) {
                    topicEntity.setType(topicDto.getType());
                    topicEntity.setOptions(topicsService.initiateVoting(topicDto.getOptions()));
                    topicEntity.setVotedBy(null);
                } else {
                    topicEntity.setType(topicDto.getType());
                }

                topicEntity.setAuthor(topicDto.getAuthor());
                topicEntity.setMembers(new Gson().toJson(topicDto.getMembers()));
                topicEntity.setCloseDate(topicDto.getCloseDate());
                topicEntity.setVisits(topicDto.getVisits() != null ? topicDto.getVisits() : 0);
                topicEntity.setStatus(topicDto.getStatus() != null ? topicDto.getStatus() : Constants.STATUS_OPENED);

                //mailService.sendEmail(topicDto);

                topicsService.saveTopic(topicEntity);
                responseJson.put("message", "Topic edited successfully");
            } else {
                responseJson.put("message", "There is no topic with that id");
            }
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
        } catch (Exception e) {
            JSONObject responseJson = new JSONObject();
            responseJson.put("message", "An error occurred --> " + e);
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }

    @PutMapping("/closeTopic")
    public ResponseEntity<SpecialResponse> closeTopic(@RequestBody TopicsDto topicDto) {
        TopicsEntity topicEntity = topicsService.findTopicsEntityById(topicDto.getId());

        JSONObject responseJson = new JSONObject();

        if(Boolean.FALSE.equals(usersService.checkToken(topicDto.getUser(), topicDto.getToken()))) {
            responseJson.put("message", "Unauthorized user");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.NOT_FOUND);
        }

        if(topicEntity == null) {
            responseJson.put("message", "There is no topic with that id");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
        }

        if(!topicEntity.getAuthor().equals(topicDto.getUser())) {
            responseJson.put("message", "The user is not the author of the topic");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
        }

        if(topicEntity.getStatus().equals(Constants.STATUS_CLOSED)) {
            responseJson.put("message", "The topic is currently closed");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
        }

        topicEntity.setStatus(Constants.STATUS_CLOSED);
        topicsService.saveTopic(topicEntity);
        responseJson.put("message", "The topic has been closed");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    @PutMapping("/reOpenTopic")
    public ResponseEntity<SpecialResponse> reOpenTopic(@RequestBody TopicsDto topicDto) {
        TopicsEntity topicEntity = topicsService.findTopicsEntityById(topicDto.getId());

        JSONObject responseJson = new JSONObject();

        if(Boolean.FALSE.equals(usersService.checkToken(topicDto.getUser(), topicDto.getToken()))) {
            responseJson.put("message", "Unauthorized user");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.NOT_FOUND);
        }

        if(topicEntity == null) {
            responseJson.put("message", "There is no topic with that id");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
        }

        if(!topicEntity.getAuthor().equals(topicDto.getUser())) {
            responseJson.put("message", "The user is not the author of the topic");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
        }

        if(topicEntity.getStatus().equals(Constants.STATUS_OPENED)) {
            responseJson.put("message", "The topic is currently open");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
        }

        topicEntity.setStatus(Constants.STATUS_OPENED);
        topicsService.saveTopic(topicEntity);
        responseJson.put("message", "Topic reopened");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    @DeleteMapping("/deleteTopic")
    public ResponseEntity<SpecialResponse> deleteTopic(@RequestBody TopicsDto topicDto) {
        JSONObject responseJson = new JSONObject();

        if(Boolean.FALSE.equals(usersService.checkToken(topicDto.getUser(), topicDto.getToken()))) {
            responseJson.put("message", "Unauthorized user");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.NOT_FOUND);
        }

        TopicsEntity topicEntity = topicsService.findTopicsEntityById(topicDto.getId());

        if(topicEntity == null) {
            responseJson.put("message", "There is no topic with that id");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
        }

        if(!topicEntity.getAuthor().equals(topicDto.getUser())) {
            responseJson.put("message", "The user is not the author of the topic");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
        }

        topicsService.deleteTopic(topicDto.getId());
        responseJson.put("message", "The topic has been deleted");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    /* Método exclusivo para desarrollo */
    @GetMapping("/getAllTopicsData")
    public ResponseEntity<SpecialResponse> getAllTopicsData() {
        List<TopicsEntity> topicsList = topicsService.getAllTopicsData();

        JSONObject responseJson = new JSONObject();

        if(topicsList.isEmpty()) {
            responseJson.put("message", "There are no topics in database");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
        }

        List<TopicsDto> topicsDtoList = new ArrayList<>();
        Gson gson = new Gson();
        Type listType = new TypeToken<List<String>>() {}.getType();

        topicsList.forEach(topic -> {
            TopicsDto topicDto = new TopicsDto();
            topicDto.setId(topic.getId());
            topicDto.setTitle(topic.getTitle());
            topicDto.setType(topic.getType());
            topicDto.setQuestion(topic.getQuestion());
            Map<String, Integer> map = gson.fromJson(topic.getOptions(), new TypeToken<Map<String, Integer>>() {}.getType());
            topicDto.setOptionsMap(map);
            topicDto.setVotedBy(topic.getVotedBy());
            topicDto.setAuthor(topic.getAuthor());
            topicDto.setMembers(gson.fromJson(topic.getMembers(), listType));
            topicDto.setVisits(topic.getVisits());
            topicDto.setStatus(topic.getStatus());
            topicsDtoList.add(topicDto);
        });

        responseJson.put("message", "OK");
        return new ResponseEntity<>(specialResponse(topicsDtoList, responseJson), HttpStatus.OK);
    }
    /* Fin del método únicamente para desarrollo */

    @PutMapping("/vote")
    public ResponseEntity<SpecialResponse> vote(@RequestBody TopicsDto topicDto) {
        JSONObject responseJson = new JSONObject();

        if(Boolean.FALSE.equals(usersService.checkToken(topicDto.getUser(), topicDto.getToken()))) {
            responseJson.put("message", "Unauthorized user");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.NOT_FOUND);
        }

        TopicsEntity topicEntity = topicsService.findTopicsEntityById(topicDto.getId());

        if(topicEntity == null) {
            responseJson.put("message", "There is no topic with that id");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
        }

        if(topicDto.getVotation().isEmpty()) {
            responseJson.put("message", "The voting cannot be empty");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.BAD_GATEWAY);
        }

        if(!topicEntity.getMembers().contains(topicDto.getUser()) && !topicEntity.getAuthor().equals(topicDto.getUser())) {
            responseJson.put("message", "The user is not allowed to vote on this topic");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
        }

        if(topicEntity.getVotedBy() != null && topicEntity.getVotedBy().contains(topicDto.getUser())) {
            responseJson.put("message", "The user has already voted");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
        }

        Constants.TopicType topicType = Constants.TopicType.valueOf(topicEntity.getType());
        if (topicDto.getVotation().size() > 1 && !(topicType == Constants.TopicType.TEXT_MULTIPLE || topicType == Constants.TopicType.IMAGE_MULTIPLE)) {
            responseJson.put("message", "The topic type is not valid for multiple voting options");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.BAD_GATEWAY);
        }

        String vote = updateVotation(topicEntity.getOptions(), topicDto.getVotation());

        if(vote.equals("KO")) {
            responseJson.put("message", "The list of votes does not match the options of the topic");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.BAD_GATEWAY);
        }

        topicEntity.setOptions(vote);

        if(topicEntity.getVotedBy() == null) {
            topicEntity.setVotedBy(topicDto.getUser());
        } else {
            topicEntity.setVotedBy(topicEntity.getVotedBy().concat(", ").concat(topicDto.getUser()));
        }

        topicsService.saveTopic(topicEntity);
        responseJson.put("message", "Votation updated successfully");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    @GetMapping("/votingResults")
    public ResponseEntity<SpecialResponse> votingResults(@RequestBody TopicsDto topicDto) {
        JSONObject responseJson = new JSONObject();

        if(Boolean.FALSE.equals(usersService.checkToken(topicDto.getUser(), topicDto.getToken()))) {
            responseJson.put("message", "Unauthorized user");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.NOT_FOUND);
        }

        TopicsEntity topicEntity = topicsService.findTopicsEntityById(topicDto.getId());

        if(topicEntity == null) {
            responseJson.put("message", "There is no topic with that id");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
        }

        if(topicEntity.getStatus().equals(Constants.STATUS_OPENED)) {
            responseJson.put("message", "The topic is not closed, so it is not possible to view the results");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
        }

        if(!topicEntity.getMembers().contains(topicDto.getUser()) && !topicEntity.getAuthor().equals(topicDto.getUser())) {
            responseJson.put("message", "The user is not allowed to view the results on this topic");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
        }

        Gson gson = new Gson();
        Map<String, Integer> optionsMap = gson.fromJson(topicEntity.getOptions(), new TypeToken<Map<String, Integer>>() {}.getType());

        responseJson.put("message", topicEntity.getType());
        return new ResponseEntity<>(specialResponse(optionsMap, responseJson), HttpStatus.OK);
    }

    private String checkTopicType(String type) {
        try {
            return String.valueOf(Constants.TopicType.valueOf(type));
        } catch (IllegalArgumentException e) {
            return "KO";
        }
    }

    private String getOnlyOptions(String input) {
        Gson gson = new Gson();
        Map<String, Integer> map = gson.fromJson(input, new TypeToken<Map<String, Integer>>() {}.getType());

        return JSONArray.toJSONString(new ArrayList<>(map.keySet()));
    }

    private String updateVotation(String options, List<String> votation) {
        boolean coincidence = false;

        Gson gson = new Gson();
        Map<String, Integer> optionsMap = gson.fromJson(options, new TypeToken<Map<String, Integer>>() {}.getType());

        for (String key : votation) {
            if (optionsMap.containsKey(key)) {
                int value = optionsMap.get(key);
                optionsMap.put(key, value + 1);
                coincidence = true;
            }
        }

        if(Boolean.FALSE.equals(coincidence)) {
            return "KO";
        } else {
            return gson.toJson(optionsMap);
        }
    }
}
