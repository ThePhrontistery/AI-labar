package com.capgemini.ailabar.topics.domain.ports.in;

import com.capgemini.ailabar.options.domain.models.OptionsModel;
import com.capgemini.ailabar.topics.domain.models.TopicsModel;

import java.util.List;
import java.util.Map;

public interface VotingResultsTopicUseCase {
    Map<String, List<OptionsModel>> votingResults(TopicsModel topicsModel);
}
