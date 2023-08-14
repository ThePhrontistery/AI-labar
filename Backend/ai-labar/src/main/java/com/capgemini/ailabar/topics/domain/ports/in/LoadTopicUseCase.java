package com.capgemini.ailabar.topics.domain.ports.in;

import com.capgemini.ailabar.topics.domain.models.TopicsModel;
import com.capgemini.ailabar.users.domain.models.UsersModel;

import java.util.List;

public interface LoadTopicUseCase {
    List<TopicsModel> loadTopics(UsersModel usersModel);
}
