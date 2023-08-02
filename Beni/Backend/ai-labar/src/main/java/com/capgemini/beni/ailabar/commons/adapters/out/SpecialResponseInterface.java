package com.capgemini.beni.ailabar.commons.adapters.out;

import com.capgemini.beni.ailabar.commons.utils.SpecialResponse;
import org.json.JSONObject;

public interface SpecialResponseInterface {
    default SpecialResponse specialResponse(Object entity, JSONObject message) {
        return new SpecialResponse(entity, message.getString("message"));
    }
}
