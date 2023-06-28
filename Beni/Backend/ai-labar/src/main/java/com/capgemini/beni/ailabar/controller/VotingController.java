package com.capgemini.beni.ailabar.controller;

import com.capgemini.beni.ailabar.service.VotingService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/voting")
public class VotingController {
    private final VotingService votingService;

    @Autowired
    public VotingController(VotingService votingService) {
        this.votingService = votingService;
    }

    @PostMapping("/vote")
    public ResponseEntity<String> vote(String topicTitle, String author, List<String> options, String user) {
        /* Hay que asegurar que si el usuario ya ha votado, no pueda volver a hacerlo */
        return null;
    }

    @GetMapping("/votingResults/{topicTitle}/{author}")
    public ResponseEntity<List<String>> votingResults(@PathVariable("topicTitle") String topicTitle, @PathVariable("author") String author) {
        return null;
    }

    @DeleteMapping("/deleteVotation/{topicTitle}/{author}")
    public ResponseEntity<String> vote(@PathVariable("topicTitle") String topicTitle, @PathVariable("author") String author) {
        return null;
    }
}
