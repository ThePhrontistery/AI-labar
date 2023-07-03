package com.capgemini.beni.ailabar.dto;

import lombok.Data;

import java.util.List;

@Data
public class TopicsDto {
    private Integer id;
    private String title;
    private String type;
    private String question;
    private String options;
    private String votedBy;
    private String author;
    private String members;
    private String closeDate;
    private Integer visits;
    private String status;
    private String user;
    private List<String> votation;
}
