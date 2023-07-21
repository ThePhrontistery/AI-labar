package com.capgemini.beni.ailabar.domain.model;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Data;

import java.util.List;
import java.util.Map;

@Data
public class TopicsModel {
    private Integer id;
    private String title;
    private String type;
    private String question;
    private List<String> options;
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private Map<String, Integer> optionsMap;
    private String votedBy;
    private String author;
    private List<String> members;
    private String closeDate;
    private Integer visits;
    private String status;
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private String user;
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private String token;
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private List<String> votation;
}
