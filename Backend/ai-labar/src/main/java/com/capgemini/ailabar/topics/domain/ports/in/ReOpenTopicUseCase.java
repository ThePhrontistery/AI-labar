package com.capgemini.ailabar.topics.domain.ports.in;

import com.capgemini.ailabar.topics.domain.models.TopicsModel;

public interface ReOpenTopicUseCase {
    void reOpenTopic(TopicsModel topicsModel);
}
