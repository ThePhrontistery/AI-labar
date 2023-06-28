package com.capgemini.beni.ailabar.entity;

import lombok.Data;

import javax.persistence.*;

@Data
@Entity
@Table(name = "voting")
public class VotingEntity {
    @Id
    @Column(name = "id")
    @GeneratedValue(strategy=GenerationType.IDENTITY)
    private Integer id;
    @Column(name = "topic_title")
    private String topicTitle;
    @Column(name = "author")
    private String author;
    @Column(name = "votes")
    private String votes;
    @Column(name = "voted_by")
    private String votedBy;

    public VotingEntity() {}
}
