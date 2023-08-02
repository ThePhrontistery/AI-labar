package com.capgemini.beni.ailabar.topics.infraestructure.entities;

import com.capgemini.beni.ailabar.topics.domain.models.TopicsModel;
import lombok.Data;

import javax.persistence.*;

@Data
@Entity
@Table(name = "topics")
public class TopicsEntity {
    @Id
    @Column(name = "id")
    @GeneratedValue(strategy=GenerationType.IDENTITY)
    private Integer id;
    @Column(name = "title")
    private String title;
    @Column(name = "type")
    private String type;
    @Column(name = "question")
    private String question;
    @Column(name = "options")
    private String options;
    @Column(name = "voted_by")
    private String votedBy;
    @Column(name = "author")
    private String author;
    @Column(name = "members")
    private String members;
    @Column(name = "close_date")
    private String closeDate;
    @Column(name = "visits")
    private Integer visits;
    @Column(name = "status")
    private String status;

    public TopicsEntity() {}

    public TopicsEntity(TopicsModel topicModel) {
        this.title = topicModel.getTitle();
        this.type = topicModel.getType();
        this.question = topicModel.getQuestion();
        this.options = topicModel.getOptions().toString();
        this.author = topicModel.getAuthor();
        this.members = topicModel.getMembers().toString();
        this.closeDate = topicModel.getCloseDate();
        this.visits = topicModel.getVisits();
        this.status = topicModel.getStatus();
    }
}
