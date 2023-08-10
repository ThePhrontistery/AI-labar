package com.capgemini.ailabar.votedby.domain.models;

import lombok.Data;

@Data
public class VotedByModel {
    private Integer id;
    private Integer topicId;
    private Integer userId;
}
