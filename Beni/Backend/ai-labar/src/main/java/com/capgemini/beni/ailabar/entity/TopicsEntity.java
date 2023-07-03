package com.capgemini.beni.ailabar.entity;

import com.capgemini.beni.ailabar.dto.TopicsDto;
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

    public TopicsEntity(TopicsDto topicDto) {
        this.title = topicDto.getTitle();
        this.type = topicDto.getType();
        this.question = topicDto.getQuestion();
        this.options = topicDto.getOptions();
        this.author = topicDto.getAuthor();
        this.members = topicDto.getMembers();
        this.closeDate = topicDto.getCloseDate();
        this.visits = topicDto.getVisits();
        this.status = topicDto.getStatus();
    }
}
