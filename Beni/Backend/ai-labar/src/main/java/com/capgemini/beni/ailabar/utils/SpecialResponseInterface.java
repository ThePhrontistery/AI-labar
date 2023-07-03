package com.capgemini.beni.ailabar.utils;

public interface SpecialResponseInterface {
    default SpecialResponse specialResponse(Object entity, String message) {
        return new SpecialResponse(entity, message);
    }

}
