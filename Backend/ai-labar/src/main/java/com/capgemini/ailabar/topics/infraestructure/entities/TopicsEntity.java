package com.capgemini.ailabar.topics.infraestructure.entities;

import com.capgemini.ailabar.topics.domain.models.TopicsModel;
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
    @Column(name = "author")
    private String author;
    @Column(name = "group_id")
    private Integer groupId;
    @Column(name = "close_date")
    private String closeDate;
    @Column(name = "visits")
    private Integer visits;
    @Column(name = "status")
    private Integer status;

    public TopicsEntity() {}

    public TopicsEntity(TopicsModel topicModel) {
        this.title = topicModel.getTitle();
        this.type = topicModel.getType();
        this.question = topicModel.getQuestion();
        this.author = topicModel.getAuthor();
        this.groupId = topicModel.getGroupId();
        this.closeDate = topicModel.getCloseDate();
        this.visits = topicModel.getVisits();
        this.status = topicModel.getStatus();
    }
}
