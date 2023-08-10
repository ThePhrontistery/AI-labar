package com.capgemini.ailabar.topics.domain.models;

import com.capgemini.ailabar.options.domain.models.OptionsModel;
import com.capgemini.ailabar.topics.infraestructure.entities.TopicsEntity;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Data;

import java.util.List;

@Data
public class TopicsModel {
    private Integer id;
    private String title;
    private String type;
    private String question;
    private List<OptionsModel> options;
    private List<String> votedByList;
    private String author;
    private String groupName;
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private Integer groupId;
    private List<String> members;
    private String closeDate;
    private Integer visits;
    private Integer status;
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private String user;
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private String token;
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private Boolean canVote;
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private List<String> votation;

    public TopicsModel() {}

    public TopicsModel(TopicsEntity topicsEntity) {
        this.id = topicsEntity.getId();
        this.title = topicsEntity.getTitle();
        this.type = topicsEntity.getType();
        this.question = topicsEntity.getQuestion();
        this.author = topicsEntity.getAuthor();
        this.groupId = topicsEntity.getGroupId();
        this.closeDate = topicsEntity.getCloseDate();
        this.visits = topicsEntity.getVisits();
        this.status = topicsEntity.getStatus();
    }
}
