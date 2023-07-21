package com.capgemini.beni.ailabar.infrastructure.utils;

import lombok.Data;

@Data
public class OptionsData {
    private final String option;
    private final Integer votes;

    public OptionsData(String option, Integer votes) {
        this.option = option;
        this.votes = votes;
    }
}
