package com.capgemini.ailabar.groups.domain.models;

import com.capgemini.ailabar.commons.utils.DateTime;
import com.capgemini.ailabar.groups.infraestructure.entities.GroupsEntity;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Data;

import java.sql.Timestamp;
import java.util.List;

@Data
public class GroupsModel {
    private Integer id;
    private String groupName;
    private List<String> members;
    private String admin;
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private Timestamp creationDate;
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private String creationDateFormatted;
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private Timestamp lastModificationDate;
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private String lastModificationDateFormatted;
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private String newGroupName;
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private String user;
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private String token;

    public GroupsModel() {}

    public GroupsModel(GroupsEntity groupsEntity) {
        this.id = groupsEntity.getId();
        this.groupName = groupsEntity.getGroupName();
        this.admin = groupsEntity.getAdmin();
        this.creationDateFormatted = DateTime.timestampToString(groupsEntity.getCreationDate());
        if(groupsEntity.getLastModificationDate() != null) {
            this.lastModificationDateFormatted = DateTime.timestampToString(groupsEntity.getLastModificationDate());
        }
    }
}

