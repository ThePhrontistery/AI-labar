package com.capgemini.beni.ailabar.controller;

import com.capgemini.beni.ailabar.dto.TopicsDto;
import com.capgemini.beni.ailabar.entity.TopicsEntity;
import com.capgemini.beni.ailabar.service.TopicsService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/topics")
public class TopicsController {
    private final TopicsService topicsService;

    @Autowired
    public TopicsController(TopicsService topicsService) {
        this.topicsService = topicsService;
    }

    @GetMapping("loadTopics/{user}")
    public ResponseEntity<List<TopicsEntity>> loadTopics(@PathVariable("user") String user) {
        if(user.isBlank()) {
            return null;
        }

        return new ResponseEntity<>(topicsService.loadTopics(user), HttpStatus.OK);
    }

    @PostMapping("/saveTopic/{user}")
    public ResponseEntity<String> saveTopic(@PathVariable("user") String user, @RequestBody TopicsDto topicDto) {
        if(topicDto.getTitle().isBlank() || topicDto.getType().isBlank() || topicDto.getQuestion().isBlank()
                || topicDto.getOptions().isBlank() || topicDto.getAuthor().isBlank() || topicDto.getGroupName().isBlank()
                || topicDto.getMembers().isBlank() || topicDto.getCloseDate().isBlank()) {
            return new ResponseEntity<>("All data is required to save a topic", HttpStatus.BAD_GATEWAY);
        }

        if(Boolean.TRUE.equals(topicsService.existsByTitleAndAuthor(topicDto.getTitle(), topicDto.getAuthor()))) {
            if(user.equals(topicDto.getAuthor())) {
                TopicsEntity topicEntity = new TopicsEntity(topicDto);
                topicsService.saveTopic(topicEntity);
                return new ResponseEntity<>("Group saved successfully", HttpStatus.OK);
            } else {
                return new ResponseEntity<>("The user is not the author of the topic", HttpStatus.OK);
            }
        }

        TopicsEntity topicEntity = new TopicsEntity(topicDto);
        topicsService.saveTopic(topicEntity);
        return new ResponseEntity<>("Group saved successfully", HttpStatus.OK);
    }

    @GetMapping("/editTopic/{id}/{author}")
    public ResponseEntity<List<TopicsEntity>> editTopic(@PathVariable("id") Integer id, @PathVariable("author") String author) {
        return null;
    }

    @PutMapping("/closeTopic/{id}/{author}")
    public ResponseEntity<String> closeTopic(@PathVariable("id") Integer id, @PathVariable("author") String author) {
        return null;
    }

    @DeleteMapping("/deleteTopic/{id}/{author}")
    public ResponseEntity<String> deleteTopic(@PathVariable("id") Integer id, @PathVariable("author") String author) {
        /* Hay que borrar la votación que corresponde a este topic */
        return null;
    }

    /* Existiría un método que validaría que la llamada a editTopic y saveTopic
       la realiza el usuario autor del topic a editar o guardar */
}