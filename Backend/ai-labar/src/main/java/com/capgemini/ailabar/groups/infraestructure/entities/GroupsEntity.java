package com.capgemini.ailabar.groups.infraestructure.entities;

import com.capgemini.ailabar.groups.domain.models.GroupsModel;
import lombok.Data;

import javax.persistence.*;
import java.sql.Timestamp;

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
    @Column(name = "creation_date")
    private Timestamp creationDate;
    @Column(name = "last_modification_date")
    private Timestamp lastModificationDate;

    public GroupsEntity() {}

    public GroupsEntity(GroupsModel groupsModel) {
        this.groupName = groupsModel.getGroupName();
        this.admin = groupsModel.getUser().strip();
    }
}
