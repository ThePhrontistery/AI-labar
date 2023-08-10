package com.capgemini.ailabar.members.infraestructure.entities;

import com.capgemini.ailabar.groups.infraestructure.entities.GroupsEntity;
import com.capgemini.ailabar.members.domain.models.MembersModel;
import com.capgemini.ailabar.users.infraestructure.entities.UsersEntity;
import lombok.Data;

import javax.persistence.*;

@Data
@Entity
@Table(name = "members")
public class MembersEntity {
    @Id
    @Column(name = "id")
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Integer id;
    @ManyToOne
    @JoinColumn(name = "group_id")
    private GroupsEntity group;
    @ManyToOne
    @JoinColumn(name = "user_id")
    private UsersEntity user;

    public MembersEntity() {}

    public MembersEntity(MembersModel membersModel) {
        this.id = membersModel.getId();
        this.group.setId(membersModel.getGroupId());
        this.user.setId(membersModel.getUserId());
    }
}
