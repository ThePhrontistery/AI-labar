package com.capgemini.ailabar.topics.domain.ports.in;

import com.capgemini.ailabar.topics.domain.models.TopicsModel;

import java.util.List;

public interface GetTopicsDatabaseUseCase {
    List<TopicsModel> getTopicsDatabase();
}
