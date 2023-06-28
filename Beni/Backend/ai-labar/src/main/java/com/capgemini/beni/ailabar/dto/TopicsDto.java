package com.capgemini.beni.ailabar.dto;

import lombok.Data;

@Data
public class TopicsDto {
    private Integer id;
    private String title;
    private String type;
    private String question;
    private String options;
    private String author;
    private String groupName;
    private String members;
    private String closeDate;
    private Integer visits;
    private Integer status;
}
