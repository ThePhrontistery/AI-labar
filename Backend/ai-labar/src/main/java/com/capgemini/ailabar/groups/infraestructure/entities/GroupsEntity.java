package com.capgemini.ailabar.groups.infraestructure.entities;

import com.capgemini.ailabar.groups.domain.models.GroupsModel;
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
    @Column(name = "admin")
    private String admin;

    public GroupsEntity() {}

    public GroupsEntity(GroupsModel groupsModel) {
        this.groupName = groupsModel.getGroupName();
        this.admin = groupsModel.getUser().strip();
    }
}
