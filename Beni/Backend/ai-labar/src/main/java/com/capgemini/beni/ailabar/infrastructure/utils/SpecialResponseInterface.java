package com.capgemini.beni.ailabar.infrastructure.utils;

import org.json.JSONObject;

public interface SpecialResponseInterface {
    default SpecialResponse specialResponse(Object entity, JSONObject message) {
        return new SpecialResponse(entity, message.getString("message"));
    }
}
