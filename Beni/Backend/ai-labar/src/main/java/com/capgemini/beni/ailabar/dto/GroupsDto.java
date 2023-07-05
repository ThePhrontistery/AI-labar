package com.capgemini.beni.ailabar.dto;

import lombok.Data;

@Data
public class GroupsDto {
    private Integer id;
    private String groupName;
    private String members;
    private String admin;
    private String newGroupName;
}

