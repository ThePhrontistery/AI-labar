package com.capgemini.ailabar.topics.domain.ports.in;

import com.capgemini.ailabar.topics.infraestructure.entities.TopicsEntity;

import java.util.List;

public interface GetTopicsDatabaseUseCase {
    List<TopicsEntity> getTopicsDatabase();
}
