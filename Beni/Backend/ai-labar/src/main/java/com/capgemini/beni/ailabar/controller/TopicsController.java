package com.capgemini.beni.ailabar.controller;

import com.capgemini.beni.ailabar.dto.TopicsDto;
import com.capgemini.beni.ailabar.dto.UsersDto;
import com.capgemini.beni.ailabar.entity.TopicsEntity;
import com.capgemini.beni.ailabar.service.MailService;
import com.capgemini.beni.ailabar.service.TopicsService;
import com.capgemini.beni.ailabar.service.UsersService;
import com.capgemini.beni.ailabar.utils.Constants;
import com.capgemini.beni.ailabar.utils.SpecialResponseInterface;
import com.capgemini.beni.ailabar.utils.SpecialResponse;
import org.apache.commons.codec.digest.DigestUtils;
import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.*;
import java.util.stream.Collectors;

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
    public ResponseEntity<String> login(@RequestBody UsersDto userDto) {
        JSONObject responseJson = new JSONObject();

        if (userDto.getUser().isBlank() || userDto.getPassword().isBlank()) {
            responseJson.put("message", "User and password are required to login");
            return new ResponseEntity<>(responseJson.toString(), HttpStatus.BAD_GATEWAY);
        }

        if (Boolean.FALSE.equals(topicsService.login(userDto.getUser(), DigestUtils.sha256Hex(userDto.getPassword())))) {
            responseJson.put("message", "Login failed");
            return new ResponseEntity<>(responseJson.toString(), HttpStatus.UNAUTHORIZED);
        }

        responseJson.put("message", DigestUtils.sha256Hex(userDto.getUser()+DigestUtils.sha256Hex(userDto.getPassword())));
        return new ResponseEntity<>(responseJson.toString(), HttpStatus.OK);
    }

    @PostMapping("loadTopics")
    public ResponseEntity<SpecialResponse> loadTopics(@RequestBody UsersDto userDto) {
        JSONObject responseJson = new JSONObject();

        if(Boolean.FALSE.equals(usersService.checkToken(userDto.getUser(), userDto.getToken()))) {
            responseJson.put("message", "The token does not match");
            return new ResponseEntity<>(specialResponse(null, responseJson.toString()), HttpStatus.NOT_FOUND);
        }

        List<TopicsEntity> topicsList = topicsService.loadTopics(userDto.getUser());

        topicsList.removeIf(topic -> !topic.getAuthor().equals(userDto.getUser()));

        if(topicsList.isEmpty()) {
            responseJson.put("message", "There are no topics related to the user");
            return new ResponseEntity<>(specialResponse(null, responseJson.toString()), HttpStatus.OK);
        }

        topicsList.forEach(topic -> topic.setOptions(getOnlyOptions(topic.getOptions()).toString()));

        responseJson.put("message", "OK");
        return new ResponseEntity<>(specialResponse(topicsList, responseJson.toString()), HttpStatus.OK);
    }

    @PostMapping("openTopic")
    public ResponseEntity<SpecialResponse> openTopic(@RequestBody TopicsDto topicDto) {
        JSONObject responseJson = new JSONObject();

        if(Boolean.FALSE.equals(usersService.checkToken(topicDto.getUser(), topicDto.getToken()))) {
            responseJson.put("message", "The token does not match");
            return new ResponseEntity<>(specialResponse(null, responseJson.toString()), HttpStatus.NOT_FOUND);
        }

        TopicsEntity topicEntity = topicsService.openTopic(topicDto.getId());

        if(topicEntity == null) {
            responseJson.put("message", "There is no topic with that id");
            return new ResponseEntity<>(specialResponse(null, responseJson.toString()), HttpStatus.OK);
        }

        if(!topicEntity.getMembers().contains(topicDto.getUser()) && !topicEntity.getAuthor().equals(topicDto.getUser())) {
            responseJson.put("message", "The user is not authorized to open the topic");
            return new ResponseEntity<>(specialResponse(null, responseJson.toString()), HttpStatus.OK);
        }

        topicEntity.setVisits(topicEntity.getVisits() + 1);
        topicsService.saveTopic(topicEntity);
        responseJson.put("message", "OK");
        return new ResponseEntity<>(specialResponse(topicEntity, responseJson.toString()), HttpStatus.OK);
    }

    /* Hay que trabajar las fechas de cierre y crear una forma de que se cierren automáticamente *//* Hay que trabajar las fechas de cierre y crear una forma de que se cierren automáticamente */
    @PostMapping("/createTopic")
    public ResponseEntity<String> createTopic(@RequestBody TopicsDto topicDto) {
        try {
            JSONObject responseJson = new JSONObject();

            if(topicDto.getTitle().isBlank() || topicDto.getType().isBlank() || topicDto.getQuestion().isBlank()
                    || topicDto.getOptions().isBlank() || topicDto.getUser().isBlank() || topicDto.getMembers().isBlank()) {
                responseJson.put("message", "All data is required to edit a topic");
                return new ResponseEntity<>(responseJson.toString(), HttpStatus.BAD_GATEWAY);
            }

            if (Boolean.FALSE.equals(usersService.checkUser(topicDto.getUser()))) {
                responseJson.put("message", "The user does not exist");
                return new ResponseEntity<>(responseJson.toString(), HttpStatus.BAD_GATEWAY);
            }

            if(Boolean.FALSE.equals(usersService.checkToken(topicDto.getUser(), topicDto.getToken()))) {
                responseJson.put("message", "The token does not match");
                return new ResponseEntity<>(responseJson.toString(), HttpStatus.NOT_FOUND);
            }

            if(Boolean.TRUE.equals(topicsService.existsByTitleAndAuthor(topicDto.getTitle().strip(), topicDto.getUser()))) {
                responseJson.put("message", "There is already a topic assigned to the author with that name");
                return new ResponseEntity<>(responseJson.toString(), HttpStatus.OK);
            }

            topicDto.setTitle(topicDto.getTitle().strip());
            topicDto.setAuthor(topicDto.getUser().strip());
            if(checkTopicType(topicDto.getType()).equals("KO")) {
                responseJson.put("message", "The topic type is not valid");
                return new ResponseEntity<>(responseJson.toString(), HttpStatus.BAD_GATEWAY);
            }
            topicDto.setType(topicDto.getType());
            topicDto.setVisits(topicDto.getVisits() != null ? topicDto.getVisits() : 0);
            topicDto.setStatus(topicDto.getStatus() != null ? topicDto.getStatus() : Constants.STATUS_OPENED);

            TopicsEntity topicEntity = new TopicsEntity(topicDto);
            topicEntity.setOptions(topicsService.initiateVoting(topicDto.getOptions()));

            mailService.sendEmail(topicDto);

            topicsService.saveTopic(topicEntity);
            responseJson.put("message", "Group saved successfully");
            return new ResponseEntity<>(responseJson.toString(), HttpStatus.OK);
        } catch (Exception e) {
            JSONObject responseJson = new JSONObject();
            responseJson.put("message", "An error occurred --> " + e);
            return new ResponseEntity<>(responseJson.toString(), HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }

    @PostMapping("/getTopicForEdit")
    public ResponseEntity<SpecialResponse> getTopicForEdit(@RequestBody TopicsDto topicDto) {
        JSONObject responseJson = new JSONObject();

        if(Boolean.FALSE.equals(usersService.checkToken(topicDto.getUser(), topicDto.getToken()))) {
            responseJson.put("message", "The token does not match");
            return new ResponseEntity<>(specialResponse(null, responseJson.toString()), HttpStatus.NOT_FOUND);
        }

        TopicsEntity topicEntity = topicsService.getTopicForEdit(topicDto.getId());

        if(topicEntity == null) {
            responseJson.put("message", "There is no topic with that id");
            return new ResponseEntity<>(specialResponse(null, responseJson.toString()), HttpStatus.OK);
        }

        if(topicEntity.getStatus().equals(Constants.STATUS_CLOSED)) {
            responseJson.put("message", "The topic is closed");
            return new ResponseEntity<>(specialResponse(null, responseJson.toString()), HttpStatus.OK);
        }

        if(!topicEntity.getAuthor().equals(topicDto.getUser())) {
            responseJson.put("message", "The user is not the author of the topic");
            return new ResponseEntity<>(specialResponse(null, responseJson.toString()), HttpStatus.OK);
        }

        topicEntity.setOptions(getOnlyOptions(topicEntity.getOptions()).toString());

        responseJson.put("message", "OK");
        return new ResponseEntity<>(specialResponse(topicEntity, responseJson.toString()), HttpStatus.OK);
    }

    /* Hay que trabajar las fechas de cierre y crear una forma de que se cierren automáticamente */
    @PutMapping("/editTopic")
    public ResponseEntity<String> editTopic(@RequestBody TopicsDto topicDto) {
        try {
            JSONObject responseJson = new JSONObject();

            if(topicDto.getId() == null || topicDto.getTitle().isBlank() || topicDto.getType().isBlank() || topicDto.getQuestion().isBlank()
                    || topicDto.getOptions().isBlank() || topicDto.getAuthor().isBlank() || topicDto.getMembers().isBlank() ||topicDto.getUser().isBlank()) {
                responseJson.put("message", "All data is required to edit a topic");
                return new ResponseEntity<>(responseJson.toString(), HttpStatus.BAD_GATEWAY);
            }

            if(Boolean.FALSE.equals(usersService.checkToken(topicDto.getUser(), topicDto.getToken()))) {
                responseJson.put("message", "The token does not match");
                return new ResponseEntity<>(responseJson.toString(), HttpStatus.NOT_FOUND);
            }

            if(Boolean.TRUE.equals(topicsService.existsById(topicDto.getId()))) {
                if(Boolean.FALSE.equals(usersService.checkUser(topicDto.getUser()))) {
                    throw new NullPointerException("The user does not exist");
                }

                if (!topicDto.getUser().equals(topicDto.getAuthor())) {
                    responseJson.put("message", "The user is not the author of the topic");
                    return new ResponseEntity<>(responseJson.toString(), HttpStatus.OK);
                }

                TopicsEntity topicEntity = topicsService.findTopicsEntityById(topicDto.getId());

                if(topicEntity.getStatus().equals(Constants.STATUS_CLOSED)) {
                    responseJson.put("message", "The topic is closed");
                    return new ResponseEntity<>(responseJson.toString(), HttpStatus.OK);
                }

                if(!topicDto.getTitle().equals(topicEntity.getTitle())) {
                    if(Boolean.TRUE.equals(topicsService.existsByTitleAndAuthor(topicDto.getTitle().strip(), topicDto.getAuthor()))) {
                        responseJson.put("message", "There is already a topic assigned to the author with that name");
                        return new ResponseEntity<>(responseJson.toString(), HttpStatus.OK);
                    } else {
                        topicEntity.setTitle(topicDto.getTitle().strip());
                    }
                }

                if(checkTopicType(topicDto.getType()).equals("KO")) {
                    responseJson.put("message", "The topic type is not valid");
                    return new ResponseEntity<>(responseJson.toString(), HttpStatus.BAD_GATEWAY);
                }

                topicEntity.setQuestion(topicDto.getQuestion());

                if(!topicDto.getOptions().equals(topicEntity.getOptions()) || !topicDto.getType().equals(topicEntity.getType())) {
                    topicEntity.setType(topicDto.getType());
                    topicEntity.setOptions(topicsService.initiateVoting(topicDto.getOptions()));
                    topicEntity.setVotedBy(null);
                } else {
                    topicEntity.setType(topicDto.getType());
                    topicEntity.setOptions(topicDto.getOptions());
                }

                topicEntity.setAuthor(topicDto.getAuthor());
                topicEntity.setMembers(topicDto.getMembers());
                topicEntity.setCloseDate(topicDto.getCloseDate());
                topicEntity.setVisits(topicDto.getVisits() != null ? topicDto.getVisits() : 0);
                topicEntity.setStatus(topicDto.getStatus() != null ? topicDto.getStatus() : Constants.STATUS_OPENED);

                mailService.sendEmail(topicDto);

                topicsService.saveTopic(topicEntity);
                responseJson.put("message", "Topic edited successfully");
            } else {
                responseJson.put("message", "There is no topic with that id");
            }
            return new ResponseEntity<>(responseJson.toString(), HttpStatus.OK);
        } catch (Exception e) {
            JSONObject responseJson = new JSONObject();
            responseJson.put("message", "An error occurred --> " + e);
            return new ResponseEntity<>(responseJson.toString(), HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }

    @PutMapping("/closeTopic")
    public ResponseEntity<String> closeTopic(@RequestBody TopicsDto topicDto) {
        TopicsEntity topicEntity = topicsService.findTopicsEntityById(topicDto.getId());

        JSONObject responseJson = new JSONObject();

        if(Boolean.FALSE.equals(usersService.checkToken(topicDto.getUser(), topicDto.getToken()))) {
            responseJson.put("message", "The token does not match");
            return new ResponseEntity<>(responseJson.toString(), HttpStatus.NOT_FOUND);
        }

        if(topicEntity == null) {
            responseJson.put("message", "There is no topic with that id");
            return new ResponseEntity<>(responseJson.toString(), HttpStatus.OK);
        }

        if(!topicEntity.getAuthor().equals(topicDto.getUser())) {
            responseJson.put("message", "The user is not the author of the topic");
            return new ResponseEntity<>(responseJson.toString(), HttpStatus.OK);
        }

        if(topicEntity.getStatus().equals(Constants.STATUS_CLOSED)) {
            responseJson.put("message", "The topic is closed");
            return new ResponseEntity<>(responseJson.toString(), HttpStatus.OK);
        }

        topicEntity.setStatus(Constants.STATUS_CLOSED);
        topicsService.saveTopic(topicEntity);
        responseJson.put("message", "The topic has been closed");
        return new ResponseEntity<>(responseJson.toString(), HttpStatus.OK);
    }

    @DeleteMapping("/deleteTopic")
    public ResponseEntity<String> deleteTopic(@RequestBody TopicsDto topicDto) {
        JSONObject responseJson = new JSONObject();

        if(Boolean.FALSE.equals(usersService.checkToken(topicDto.getUser(), topicDto.getToken()))) {
            responseJson.put("message", "The token does not match");
            return new ResponseEntity<>(responseJson.toString(), HttpStatus.NOT_FOUND);
        }

        TopicsEntity topicEntity = topicsService.findTopicsEntityById(topicDto.getId());

        if(topicEntity == null) {
            responseJson.put("message", "There is no topic with that id");
            return new ResponseEntity<>(responseJson.toString(), HttpStatus.OK);
        }

        if(!topicEntity.getAuthor().equals(topicDto.getUser())) {
            responseJson.put("message", "The user is not the author of the topic");
            return new ResponseEntity<>(responseJson.toString(), HttpStatus.OK);
        }

        topicsService.deleteTopic(topicDto.getId());
        responseJson.put("message", "The topic has been deleted");
        return new ResponseEntity<>(responseJson.toString(), HttpStatus.OK);
    }

    @GetMapping("/getAllTopicsData")
    public ResponseEntity<SpecialResponse> getAllTopicsData() {
        List<TopicsEntity> topicsList = topicsService.getAllTopicsData();

        JSONObject responseJson = new JSONObject();

        if(topicsList.isEmpty()) {
            responseJson.put("message", "There are no topics in database");
            return new ResponseEntity<>(specialResponse(null, responseJson.toString()), HttpStatus.OK);
        }
        responseJson.put("message", "OK");
        return new ResponseEntity<>(specialResponse(topicsList, responseJson.toString()), HttpStatus.OK);
    }

    @PutMapping("/vote")
    public ResponseEntity<String> vote(@RequestBody TopicsDto topicDto) {
        JSONObject responseJson = new JSONObject();

        if(Boolean.FALSE.equals(usersService.checkToken(topicDto.getUser(), topicDto.getToken()))) {
            responseJson.put("message", "The token does not match");
            return new ResponseEntity<>(responseJson.toString(), HttpStatus.NOT_FOUND);
        }

        TopicsEntity topicEntity = topicsService.findTopicsEntityById(topicDto.getId());

        if(topicEntity == null) {
            responseJson.put("message", "There is no topic with that id");
            return new ResponseEntity<>(responseJson.toString(), HttpStatus.OK);
        }

        if(Boolean.FALSE.equals(usersService.checkUser(topicDto.getUser()))) {
            throw new NullPointerException("The user does not exist");
        }

        if(topicDto.getVotation().isEmpty()) {
            responseJson.put("message", "The voting cannot be empty");
            return new ResponseEntity<>(responseJson.toString(), HttpStatus.BAD_GATEWAY);
        }

        if(!topicEntity.getMembers().contains(topicDto.getUser()) && !topicEntity.getAuthor().equals(topicDto.getUser())) {
            responseJson.put("message", "The user is not allowed to vote on this topic");
            return new ResponseEntity<>(responseJson.toString(), HttpStatus.OK);
        }

        if(topicEntity.getVotedBy() != null && topicEntity.getVotedBy().contains(topicDto.getUser())) {
            responseJson.put("message", "The user has already voted");
            return new ResponseEntity<>(responseJson.toString(), HttpStatus.OK);
        }

        Constants.TopicType topicType = Constants.TopicType.valueOf(topicEntity.getType());
        if (topicDto.getVotation().size() > 1 && !(topicType == Constants.TopicType.TEXT_MULTIPLE || topicType == Constants.TopicType.IMAGE_MULTIPLE)) {
            responseJson.put("message", "The topic type is not valid for multiple voting options");
            return new ResponseEntity<>(responseJson.toString(), HttpStatus.BAD_GATEWAY);
        }

        topicEntity.setOptions(updateVotation(topicEntity.getOptions(), topicDto.getVotation()));

        if(topicEntity.getVotedBy() == null) {
            topicEntity.setVotedBy(topicDto.getUser());
        } else {
            topicEntity.setVotedBy(topicEntity.getVotedBy().concat(", ").concat(topicDto.getUser()));
        }

        topicsService.saveTopic(topicEntity);
        responseJson.put("message", "Votation updated successfully");
        return new ResponseEntity<>(responseJson.toString(), HttpStatus.OK);
    }

    @GetMapping("/votingResults")
    public ResponseEntity<SpecialResponse> votingResults(@RequestBody TopicsDto topicDto) {
        JSONObject responseJson = new JSONObject();

        if(Boolean.FALSE.equals(usersService.checkToken(topicDto.getUser(), topicDto.getToken()))) {
            responseJson.put("message", "The token does not match");
            return new ResponseEntity<>(specialResponse(null, responseJson.toString()), HttpStatus.NOT_FOUND);
        }

        TopicsEntity topicEntity = topicsService.findTopicsEntityById(topicDto.getId());

        if(topicEntity == null) {
            responseJson.put("message", "There is no topic with that id");
            return new ResponseEntity<>(specialResponse(null, responseJson.toString()), HttpStatus.OK);
        }

        if(!topicEntity.getMembers().contains(topicDto.getUser()) && !topicEntity.getAuthor().equals(topicDto.getUser())) {
            responseJson.put("message", "The user is not allowed to view the results on this topic");
            return new ResponseEntity<>(specialResponse(null, responseJson.toString()), HttpStatus.OK);
        }

        Map<String, String> optionsMap = new HashMap<>();

        Arrays.stream(topicEntity.getOptions().split(","))
                .map(option -> option.split(":"))
                .filter(parts -> parts.length == 2)
                .forEach(parts -> optionsMap.put(parts[0].strip(), parts[1].strip()));

        responseJson.put("message", topicEntity.getType());
        return new ResponseEntity<>(specialResponse(optionsMap, responseJson.toString()), HttpStatus.OK);
    }

    private String checkTopicType(String type) {
        try {
            return String.valueOf(Constants.TopicType.valueOf(type));
        } catch (IllegalArgumentException e) {
            return "KO";
        }
    }

    private List<String> getOnlyOptions(String options) {
        return Arrays.stream(options.split(","))
                .map(option -> option.split(":")[0].strip())
                .collect(Collectors.toList());
    }

    private String updateVotation(String options, List<String> votation) {
        Map<String, Integer> optionsMap = new HashMap<>();

        Arrays.stream(options.split(","))
                .map(option -> option.split(":"))
                .filter(parts -> parts.length == 2)
                .forEach(parts -> optionsMap.put(parts[0].strip(), Integer.parseInt(parts[1].strip())));

        for (String key : votation) {
            optionsMap.computeIfPresent(key, (k, v) -> v + 1);
        }

        StringBuilder result = new StringBuilder();
        for (Map.Entry<String, Integer> entry : optionsMap.entrySet()) {
            result.append(entry.getKey()).append(":").append(entry.getValue()).append(", ");
        }
        if (result.length() > 1) {
            result.setLength(result.length() - 2);
        }

        return result.toString();
    }

}