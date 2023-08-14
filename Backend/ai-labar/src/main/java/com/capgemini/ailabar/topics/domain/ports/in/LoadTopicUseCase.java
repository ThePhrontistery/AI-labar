package com.capgemini.ailabar.topics.domain.ports.in;

import com.capgemini.ailabar.users.domain.models.UsersModel;

import java.util.Map;

public interface LoadTopicUseCase {
    Map<String, Object> loadTopics(UsersModel usersModel);
}
