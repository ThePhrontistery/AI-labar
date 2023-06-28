package com.capgemini.beni.ailabar.entity;

import com.capgemini.beni.ailabar.dto.GroupsDto;
import lombok.Data;

import javax.persistence.*;

@Data
@Entity
@Table(name = "groups")
public class GroupsEntity {
    @Id
    @Column(name = "id")
    @GeneratedValue(strategy=GenerationType.IDENTITY)
    private Integer id;
    @Column(name = "group_name")
    private String groupName;
    @Column(name = "members")
    private String members;
    @Column(name = "admin")
    private String admin;

    public GroupsEntity() {}

    public GroupsEntity(GroupsDto groupsDto) {
        this.groupName = groupsDto.getGroupName();
        this.members = groupsDto.getMembers();
        this.admin = groupsDto.getAdmin();
    }
}
