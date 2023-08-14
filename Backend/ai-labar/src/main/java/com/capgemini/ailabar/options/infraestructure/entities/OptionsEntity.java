package com.capgemini.ailabar.options.infraestructure.entities;

import com.capgemini.ailabar.options.domain.models.OptionsModel;
import com.capgemini.ailabar.topics.infraestructure.entities.TopicsEntity;
import lombok.Data;

import javax.persistence.*;

@Data
@Entity
@Table(name = "options")
public class OptionsEntity {
    @Id
    @Column(name = "id")
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Integer id;
    @ManyToOne
    @JoinColumn(name = "topic_id")
    private TopicsEntity topic;
    @Column(name = "image")
    private String image;
    @Column(name = "option")
    private String option;
    @Column(name = "votes")
    private Integer votes;

    public OptionsEntity() {}

    public OptionsEntity(OptionsModel optionsModel) {
        this.id = optionsModel.getId();
        this.topic.setId(optionsModel.getTopicId());
        this.image = optionsModel.getImage();
        this.option = optionsModel.getOption();
        this.votes = optionsModel.getVotes();
    }
}
