package com.capgemini.ailabar.options.domain.models;

import lombok.Data;

@Data
public class OptionsModel {
    private Integer id;
    private Integer topicId;
    private String image;
    private String option;
    private Integer votes;

    public OptionsModel() {
    }

    public OptionsModel(Integer id, String image) {
        this.id = id;
        this.image = image;
    }

    public OptionsModel(String option, Integer votes) {
        this.option = option;
        this.votes = votes;
    }

    public OptionsModel(String image, String option, Integer votes) {
        this.image = image;
        this.option = option;
        this.votes = votes;
    }
}
