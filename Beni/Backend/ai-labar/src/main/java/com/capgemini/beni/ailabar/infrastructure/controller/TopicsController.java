package com.capgemini.beni.ailabar.infrastructure.controller;

import com.capgemini.beni.ailabar.domain.model.TopicsModel;
import com.capgemini.beni.ailabar.domain.model.UsersModel;
import com.capgemini.beni.ailabar.infrastructure.entity.TopicsEntity;
import com.capgemini.beni.ailabar.infrastructure.entity.UsersEntity;
import com.capgemini.beni.ailabar.application.service.MailService;
import com.capgemini.beni.ailabar.application.service.TopicsService;
import com.capgemini.beni.ailabar.application.service.UsersService;
import com.capgemini.beni.ailabar.infrastructure.utils.Constants;
import com.capgemini.beni.ailabar.infrastructure.utils.OptionsData;
import com.capgemini.beni.ailabar.infrastructure.utils.SpecialResponseInterface;
import com.capgemini.beni.ailabar.infrastructure.utils.SpecialResponse;
import com.google.gson.*;
import com.google.gson.reflect.TypeToken;
import org.apache.commons.codec.digest.DigestUtils;
import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.lang.reflect.Type;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

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
    public ResponseEntity<SpecialResponse> login(@RequestBody UsersModel userDto) {
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
    public ResponseEntity<SpecialResponse> loadTopics(@RequestBody UsersModel userDto) {
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

        List<TopicsModel> topicsModelList = new ArrayList<>();
        Gson gson = new Gson();
        Type listType = new TypeToken<List<String>>() {}.getType();

        topicsList.forEach(topic -> {
            TopicsModel topicModel = new TopicsModel();
            topicModel.setId(topic.getId());
            topicModel.setTitle(topic.getTitle());
            topicModel.setType(topic.getType());
            topicModel.setQuestion(topic.getQuestion());
            topicModel.setOptionsDataList(getOptionsWithoutVotes(topic.getOptions()));
            topicModel.setAuthor(topic.getAuthor());
            topicModel.setMembers(gson.fromJson(topic.getMembers(), listType));
            topicModel.setVisits(topic.getVisits());
            topicModel.setCanVote(topic.getVotedBy() == null || !containsExactMatch(Collections.singletonList(topic.getVotedBy()), userDto.getUser()));
            if(topic.getCloseDate() != null && !topic.getCloseDate().isBlank()) {
                topicModel.setCloseDate(topic.getCloseDate());
            }
            topicModel.setStatus(topic.getStatus());
            topicsModelList.add(topicModel);
        });

        responseJson.put("message", "OK");
        return new ResponseEntity<>(specialResponse(topicsModelList, responseJson), HttpStatus.OK);
    }

    @PostMapping("/createTopic")
    public ResponseEntity<SpecialResponse> createTopic(@RequestBody TopicsModel topicModel) {
        try {
            JSONObject responseJson = new JSONObject();

            if(topicModel.getTitle().isBlank() || topicModel.getType().isBlank() || topicModel.getQuestion().isBlank() || topicModel.getOptions().isEmpty()
                    || topicModel.getUser().isBlank() || topicModel.getMembers().isEmpty()) {
                responseJson.put("message", "All data is required to edit a topic");
                return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.BAD_GATEWAY);
            }

            if(Boolean.FALSE.equals(usersService.checkToken(topicModel.getUser(), topicModel.getToken()))) {
                responseJson.put("message", "Unauthorized user");
                return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.NOT_FOUND);
            }

            if(Boolean.TRUE.equals(topicsService.existsByTitleAndAuthor(topicModel.getTitle().strip(), topicModel.getUser()))) {
                responseJson.put("message", "There is already a topic assigned to the author with that name");
                return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
            }

            topicModel.setTitle(topicModel.getTitle().strip());
            topicModel.setAuthor(topicModel.getUser().strip());
            if(checkTopicType(topicModel.getType()).equals("KO")) {
                responseJson.put("message", "The topic type is not valid");
                return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.BAD_GATEWAY);
            }
            topicModel.setType(topicModel.getType());

            if ((topicModel.getType().equals(Constants.TopicType.IMAGE_SINGLE.toString()) || topicModel.getType().equals(Constants.TopicType.IMAGE_MULTIPLE.toString()))
                && !validateOptionsDataList(topicModel.getOptions())) {
                responseJson.put("message", "It is mandatory to send the images and options for this type of topic");
                return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.BAD_GATEWAY);
            }

            topicModel.setVisits(topicModel.getVisits() != null ? topicModel.getVisits() : 0);
            topicModel.setStatus(topicModel.getStatus() != null ? topicModel.getStatus() : Constants.STATUS_OPENED);

            TopicsEntity topicEntity = new TopicsEntity(topicModel);
            topicEntity.setOptions(topicsService.initiateVoting(topicModel.getType(), topicModel.getOptions()));
            topicEntity.setMembers(new Gson().toJson(topicModel.getMembers()));

            if(topicModel.getCloseDate() != null && !topicModel.getCloseDate().isBlank()) {
                String dateString = formatDateToYYYYMMdd(topicModel.getCloseDate());
                if(dateString.contains("KO")) {
                    responseJson.put("message", dateString);
                    return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.BAD_GATEWAY);
                } else {
                    topicEntity.setCloseDate(topicModel.getCloseDate());
                }
            }

            if(topicsService.checkMailActivate()) {
                mailService.sendEmail(topicModel);
            }

            topicsService.saveTopic(topicEntity);
            responseJson.put("message", "Topic created successfully");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
        } catch (Exception e) {
            JSONObject responseJson = new JSONObject();
            responseJson.put("message", "An error occurred --> " + e);
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }

    @PutMapping("/editTopic")
    public ResponseEntity<SpecialResponse> editTopic(@RequestBody TopicsModel topicModel) {
        try {
            JSONObject responseJson = new JSONObject();

            if(topicModel.getId() == null || topicModel.getTitle().isBlank() || topicModel.getType().isBlank() || topicModel.getQuestion().isBlank()
                    || topicModel.getOptions().isEmpty() || topicModel.getMembers().isEmpty() || topicModel.getUser().isBlank()) {
                responseJson.put("message", "All data is required to edit a topic");
                return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.BAD_GATEWAY);
            }

            if(Boolean.FALSE.equals(usersService.checkToken(topicModel.getUser(), topicModel.getToken()))) {
                responseJson.put("message", "Unauthorized user");
                return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.NOT_FOUND);
            }

            if(Boolean.TRUE.equals(topicsService.existsById(topicModel.getId()))) {
                TopicsEntity topicEntity = topicsService.findTopicsEntityById(topicModel.getId());

                if (!topicModel.getUser().equals(topicEntity.getAuthor())) {
                    responseJson.put("message", "The user is not the author of the topic");
                    return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
                }

                if(topicEntity.getStatus().equals(Constants.STATUS_CLOSED)) {
                    responseJson.put("message", "The topic is closed");
                    return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
                }

                if(!topicModel.getTitle().equals(topicEntity.getTitle()) && Boolean.TRUE.equals(topicsService.existsByTitleAndAuthor(topicModel.getTitle().strip(), topicModel.getUser()))) {
                    responseJson.put("message", "There is already a topic assigned to the author with that name");
                    return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
                } else {
                    topicEntity.setTitle(topicModel.getTitle().strip());
                }

                if(checkTopicType(topicModel.getType()).equals("KO")) {
                    responseJson.put("message", "The topic type is not valid");
                    return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.BAD_GATEWAY);
                }

                if ((topicModel.getType().equals(Constants.TopicType.IMAGE_SINGLE.toString()) || topicModel.getType().equals(Constants.TopicType.IMAGE_MULTIPLE.toString()))
                        && !validateOptionsDataList(topicModel.getOptions())) {
                    responseJson.put("message", "It is mandatory to send the images and options for this type of topic");
                    return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.BAD_GATEWAY);
                }

                topicEntity.setQuestion(topicModel.getQuestion());

                if(!topicModel.getOptions().toString().equals(getOptionsWithoutVotes(topicEntity.getOptions())) || !topicModel.getType().equals(topicEntity.getType())) {
                    topicEntity.setType(topicModel.getType());
                    topicEntity.setOptions(topicsService.initiateVoting(topicModel.getType(), topicModel.getOptions()));
                    topicEntity.setVotedBy(null);
                } else {
                    topicEntity.setType(topicModel.getType());
                }

                topicEntity.setAuthor(topicModel.getUser());
                topicEntity.setMembers(new Gson().toJson(topicModel.getMembers()));

                if(topicModel.getCloseDate() != null) {
                    if(topicModel.getCloseDate().isBlank() && topicEntity.getCloseDate() != null) {
                        topicEntity.setCloseDate(null);
                    } else {
                        topicEntity.setCloseDate(topicModel.getCloseDate());
                    }
                } else {
                    if(topicEntity.getCloseDate() != null) {
                        topicEntity.setCloseDate(null);
                    }
                }

                topicEntity.setVisits(topicModel.getVisits() != null ? topicModel.getVisits() : 0);
                topicEntity.setStatus(topicModel.getStatus() != null ? topicModel.getStatus() : Constants.STATUS_OPENED);

                if(topicModel.getCloseDate() != null && !topicModel.getCloseDate().isBlank()) {
                    String dateString = formatDateToYYYYMMdd(topicModel.getCloseDate());
                    if(dateString.contains("KO")) {
                        responseJson.put("message", dateString);
                        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.BAD_GATEWAY);
                    } else {
                        topicEntity.setCloseDate(topicModel.getCloseDate());
                    }
                }

                if(topicsService.checkMailActivate()) {
                    mailService.sendEmail(topicModel);
                }

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
    public ResponseEntity<SpecialResponse> closeTopic(@RequestBody TopicsModel topicModel) {
        TopicsEntity topicEntity = topicsService.findTopicsEntityById(topicModel.getId());

        JSONObject responseJson = new JSONObject();

        if(Boolean.FALSE.equals(usersService.checkToken(topicModel.getUser(), topicModel.getToken()))) {
            responseJson.put("message", "Unauthorized user");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.NOT_FOUND);
        }

        if(topicEntity == null) {
            responseJson.put("message", "There is no topic with that id");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
        }

        if(!topicEntity.getAuthor().equals(topicModel.getUser())) {
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
    public ResponseEntity<SpecialResponse> reOpenTopic(@RequestBody TopicsModel topicModel) {
        TopicsEntity topicEntity = topicsService.findTopicsEntityById(topicModel.getId());

        JSONObject responseJson = new JSONObject();

        if(Boolean.FALSE.equals(usersService.checkToken(topicModel.getUser(), topicModel.getToken()))) {
            responseJson.put("message", "Unauthorized user");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.NOT_FOUND);
        }

        if(topicEntity == null) {
            responseJson.put("message", "There is no topic with that id");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
        }

        if(!topicEntity.getAuthor().equals(topicModel.getUser())) {
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
    public ResponseEntity<SpecialResponse> deleteTopic(@RequestBody TopicsModel topicModel) {
        JSONObject responseJson = new JSONObject();

        if(Boolean.FALSE.equals(usersService.checkToken(topicModel.getUser(), topicModel.getToken()))) {
            responseJson.put("message", "Unauthorized user");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.NOT_FOUND);
        }

        TopicsEntity topicEntity = topicsService.findTopicsEntityById(topicModel.getId());

        if(topicEntity == null) {
            responseJson.put("message", "There is no topic with that id");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
        }

        if(!topicEntity.getAuthor().equals(topicModel.getUser())) {
            responseJson.put("message", "The user is not the author of the topic");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
        }

        topicsService.deleteTopic(topicModel.getId());
        responseJson.put("message", "The topic has been deleted");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    /* Método exclusivo para desarrollo */
    @GetMapping("/getAllTopicsData")
    public ResponseEntity<SpecialResponse> getAllTopicsData() {
        List<TopicsEntity> topicsList = topicsService.getAllTopicsData();

        JSONObject responseJson = new JSONObject();

        if (topicsList.isEmpty()) {
            responseJson.put("message", "There are no topics in database");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
        }

        List<TopicsModel> topicsModelList = new ArrayList<>();
        Gson gson = new Gson();
        Type listType = new TypeToken<List<String>>() {}.getType();

        topicsList.forEach(topic -> {
            TopicsModel topicModel = new TopicsModel();
            topicModel.setId(topic.getId());
            topicModel.setTitle(topic.getTitle());
            topicModel.setType(topic.getType());
            topicModel.setQuestion(topic.getQuestion());

            List<OptionsData> optionsDataList = gson.fromJson(topic.getOptions(), new TypeToken<List<OptionsData>>() {}.getType());
            topicModel.setOptionsDataList(optionsDataList);

            topicModel.setVotedBy(topic.getVotedBy());
            topicModel.setAuthor(topic.getAuthor());
            topicModel.setMembers(gson.fromJson(topic.getMembers(), listType));
            topicModel.setVisits(topic.getVisits());
            if(topic.getCloseDate() != null && !topic.getCloseDate().isBlank()) {
                topicModel.setCloseDate(topic.getCloseDate());
            }
            topicModel.setStatus(topic.getStatus());
            topicsModelList.add(topicModel);
        });

        responseJson.put("message", "OK");
        return new ResponseEntity<>(specialResponse(topicsModelList, responseJson), HttpStatus.OK);
    }
    /* Fin del método únicamente para desarrollo */

    @PutMapping("/vote")
    public ResponseEntity<SpecialResponse> vote(@RequestBody TopicsModel topicModel) {
        JSONObject responseJson = new JSONObject();

        if(Boolean.FALSE.equals(usersService.checkToken(topicModel.getUser(), topicModel.getToken()))) {
            responseJson.put("message", "Unauthorized user");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.NOT_FOUND);
        }

        TopicsEntity topicEntity = topicsService.findTopicsEntityById(topicModel.getId());

        if(topicEntity == null) {
            responseJson.put("message", "There is no topic with that id");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
        }

        if(topicModel.getVotation().isEmpty()) {
            responseJson.put("message", "The voting cannot be empty");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.BAD_GATEWAY);
        }

        if(!topicEntity.getMembers().contains(topicModel.getUser()) && !topicEntity.getAuthor().equals(topicModel.getUser())) {
            responseJson.put("message", "The user is not allowed to vote on this topic");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
        }

        if(topicEntity.getVotedBy() != null && topicEntity.getVotedBy().contains(topicModel.getUser())) {
            responseJson.put("message", "The user has already voted");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
        }

        Constants.TopicType topicType = Constants.TopicType.valueOf(topicEntity.getType());
        if (topicModel.getVotation().size() > 1 && !(topicType == Constants.TopicType.TEXT_MULTIPLE || topicType == Constants.TopicType.IMAGE_MULTIPLE)) {
            responseJson.put("message", "The topic type is not valid for multiple voting options");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.BAD_GATEWAY);
        }

        String vote = updateVotation(topicEntity.getOptions(), topicModel.getVotation());

        if(vote.equals("KO")) {
            responseJson.put("message", "The list of votes does not match the options of the topic");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.BAD_GATEWAY);
        }

        topicEntity.setVisits(topicEntity.getVisits() + 1);
        topicEntity.setOptions(vote);

        if(topicEntity.getVotedBy() == null) {
            topicEntity.setVotedBy(topicModel.getUser());
        } else {
            topicEntity.setVotedBy(topicEntity.getVotedBy().concat(", ").concat(topicModel.getUser()));
        }

        topicsService.saveTopic(topicEntity);
        responseJson.put("message", "Votation updated successfully");
        return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
    }

    @PostMapping("/votingResults")
    public ResponseEntity<SpecialResponse> votingResults(@RequestBody TopicsModel topicModel) {
        JSONObject responseJson = new JSONObject();

        if(Boolean.FALSE.equals(usersService.checkToken(topicModel.getUser(), topicModel.getToken()))) {
            responseJson.put("message", "Unauthorized user");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.NOT_FOUND);
        }

        TopicsEntity topicEntity = topicsService.findTopicsEntityById(topicModel.getId());

        if(topicEntity == null) {
            responseJson.put("message", "There is no topic with that id");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
        }

        if(topicEntity.getStatus().equals(Constants.STATUS_OPENED)) {
            responseJson.put("message", "The topic is not closed, so it is not possible to view the results");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
        }

        if(!topicEntity.getMembers().contains(topicModel.getUser()) && !topicEntity.getAuthor().equals(topicModel.getUser())) {
            responseJson.put("message", "The user is not allowed to view the results on this topic");
            return new ResponseEntity<>(specialResponse(null, responseJson), HttpStatus.OK);
        }

        topicEntity.setVisits(topicEntity.getVisits() + 1);
        topicsService.saveTopic(topicEntity);

        Gson gson = new Gson();
        List<OptionsData> optionsDataList = gson.fromJson(topicEntity.getOptions(), new TypeToken<List<OptionsData>>() {}.getType());
        topicModel.setOptionsDataList(optionsDataList);

        responseJson.put("message", topicEntity.getType());
        return new ResponseEntity<>(specialResponse(optionsDataList, responseJson), HttpStatus.OK);
    }

    private String checkTopicType(String type) {
        try {
            return String.valueOf(Constants.TopicType.valueOf(type));
        } catch (IllegalArgumentException e) {
            return "KO";
        }
    }

    private boolean containsExactMatch(List<String> userList, String userToFind) {
        return userList.stream().anyMatch(user -> user.equals(userToFind));
    }

    private boolean validateOptionsDataList(List<OptionsData> list) {
        return list.stream()
                .allMatch(optionsData -> optionsData.getImage() != null
                        && !optionsData.getImage().isEmpty()
                        && optionsData.getOption() != null
                        && !optionsData.getOption().isEmpty());
    }

    private List<OptionsData> getOptionsWithoutVotes(String input) {
        Gson gson = new Gson();
        JsonElement jsonElement = JsonParser.parseString(input);

        return StreamSupport.stream(jsonElement.getAsJsonArray().spliterator(), false)
                .map(JsonElement::getAsJsonObject)
                .map(jsonObject -> {
                    jsonObject.remove("votes");
                    return gson.fromJson(jsonObject, OptionsData.class);
                })
                .collect(Collectors.toList());
    }

    private String updateVotation(String options, List<String> votation) {
        boolean coincidence = false;

        Gson gson = new Gson();
        List<OptionsData> optionsDataList = gson.fromJson(options, new TypeToken<List<OptionsData>>() {}.getType());

        for (OptionsData optionsData : optionsDataList) {
            if (votation.contains(optionsData.getOption())) {
                optionsData.setVotes(optionsData.getVotes() + 1);
                coincidence = true;
            }
        }

        if (coincidence) {
            return gson.toJson(optionsDataList);
        } else {
            return "KO";
        }
    }

    private String formatDateToYYYYMMdd(String inputDate) {
        String cleanedDate = inputDate.replaceAll("\\s+", "");

        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("[yyyy-MM-dd][yyyy/MM/dd][dd-MM-yyyy][dd/MM/yyyy][MM/dd/yyyy][yyyyMMdd]");
        try {
            LocalDate localDate = LocalDate.parse(cleanedDate, formatter);
            return localDate.format(DateTimeFormatter.ofPattern("yyyyMMdd"));
        } catch (DateTimeParseException e) {
            return "KO - Error while parsing the date: " + e.getMessage();
        }
    }
}
