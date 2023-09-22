package com.capgemini.ailabar.votedby.domain.models;

import lombok.Data;

import java.sql.Timestamp;

@Data
public class VotedByModel {
    private Integer id;
    private Integer topicId;
    private Integer userId;
    private Timestamp votingDate;
}
