package com.capgemini.ailabar.infrastructure.entity;

import com.capgemini.ailabar.domain.model.GroupsModel;
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

    public GroupsEntity(GroupsModel groupsModel) {
        this.groupName = groupsModel.getGroupName();
        this.members = groupsModel.getMembers().toString();
        this.admin = groupsModel.getAdmin();
    }
}
