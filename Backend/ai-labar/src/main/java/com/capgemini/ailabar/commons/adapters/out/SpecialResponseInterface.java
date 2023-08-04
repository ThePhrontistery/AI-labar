package com.capgemini.ailabar.commons.adapters.out;

import com.capgemini.ailabar.commons.utils.SpecialResponse;
import org.json.JSONObject;

public interface SpecialResponseInterface {
    default SpecialResponse specialResponse(Object entity, JSONObject message) {
        return new SpecialResponse(entity, message.getString("message"));
    }
}
