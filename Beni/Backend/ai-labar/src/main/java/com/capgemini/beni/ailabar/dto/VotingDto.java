package com.capgemini.beni.ailabar.dto;

import lombok.Data;

import javax.persistence.Column;

@Data
public class VotingDto {
    private Integer id;
    private String topicTitle;
    private String author;
    private String votes;
    private String votedBy;
}
