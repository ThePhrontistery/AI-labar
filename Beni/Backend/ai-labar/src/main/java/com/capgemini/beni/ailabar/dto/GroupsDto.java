package com.capgemini.beni.ailabar.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Data;

import java.util.List;

@Data
public class GroupsDto {
    private Integer id;
    private String groupName;
    private List<String> members;
    private String admin;
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private String newGroupName;
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private String user;
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private String token;
}

