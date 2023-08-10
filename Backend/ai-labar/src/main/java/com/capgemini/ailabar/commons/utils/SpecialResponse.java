package com.capgemini.ailabar.commons.utils;

public final class SpecialResponse {
    private final Object entity;
    private final String message;

    public SpecialResponse(Object entity, String message) {
        this.entity = entity;
        this.message = message;
    }

    public Object getEntity() {
        return entity;
    }

    public String getMessage() {
        return message;
    }
}
