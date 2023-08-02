package com.capgemini.beni.ailabar.topics.domain.models;

import com.capgemini.beni.ailabar.commons.utils.OptionsData;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Data;

import java.util.List;

@Data
public class TopicsModel {
    private Integer id;
    private String title;
    private String type;
    private String question;
    private List<OptionsData> options;
    @JsonInclude(JsonInclude.Include.NON_NULL)
    List<OptionsData> optionsDataList;
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
    private Boolean canVote;
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private List<String> votation;
}
