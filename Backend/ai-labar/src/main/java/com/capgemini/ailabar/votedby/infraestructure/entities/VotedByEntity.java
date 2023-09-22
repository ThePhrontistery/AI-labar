package com.capgemini.ailabar.votedby.infraestructure.entities;

import com.capgemini.ailabar.commons.utils.DateTime;
import com.capgemini.ailabar.topics.infraestructure.entities.TopicsEntity;
import com.capgemini.ailabar.users.infraestructure.entities.UsersEntity;
import com.capgemini.ailabar.votedby.domain.models.VotedByModel;
import lombok.Data;

import javax.persistence.*;
import java.sql.Timestamp;

@Data
@Entity
@Table(name = "voted_by")
public class VotedByEntity {
    @Id
    @Column(name = "id")
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Integer id;
    @ManyToOne
    @JoinColumn(name = "topic_id")
    private TopicsEntity topic;
    @ManyToOne
    @JoinColumn(name = "user_id")
    private UsersEntity user;
    @Column(name = "voting_date")
    private Timestamp votingDate;

    public VotedByEntity() {}

    public VotedByEntity(VotedByModel votedByModel) {
        this.id = votedByModel.getId();
        this.topic.setId(votedByModel.getTopicId());
        this.user.setId(votedByModel.getUserId());
        this.votingDate = DateTime.actualDateAndTime();
    }
}
