package com.capgemini.ailabar.topics.domain.ports.in;

import com.capgemini.ailabar.topics.domain.models.TopicsModel;

public interface EditTopicUseCase {
    void editTopic(TopicsModel topicsModel);
}
