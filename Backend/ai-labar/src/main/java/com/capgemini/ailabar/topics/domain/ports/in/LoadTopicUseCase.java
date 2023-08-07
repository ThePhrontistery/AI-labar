package com.capgemini.ailabar.topics.domain.ports.in;

import com.capgemini.ailabar.topics.infraestructure.entities.TopicsEntity;
import com.capgemini.ailabar.users.domain.models.UsersModel;

import java.util.List;

public interface LoadTopicUseCase {
    List<TopicsEntity> loadTopics(UsersModel usersModel);
}
