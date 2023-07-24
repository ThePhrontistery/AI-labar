package com.capgemini.beni.ailabar.infrastructure.utils;

import lombok.Data;

@Data
public class OptionsData {
    private String image;
    private String option;
    private Integer votes;

    public OptionsData() {
    }

    public OptionsData(String option, Integer votes) {
        this.option = option;
        this.votes = votes;
    }

    public OptionsData(String image, String option, Integer votes) {
        this.image = image;
        this.option = option;
        this.votes = votes;
    }
}
