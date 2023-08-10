package com.capgemini.ailabar.topics.application.services;

import com.capgemini.ailabar.groups.domain.exceptions.GetGroupsDatabaseException;
import com.capgemini.ailabar.options.domain.models.OptionsModel;
import com.capgemini.ailabar.topics.domain.exceptions.*;
import com.capgemini.ailabar.topics.domain.models.TopicsModel;
import com.capgemini.ailabar.topics.domain.ports.in.*;
import com.capgemini.ailabar.users.domain.models.UsersModel;
import org.springframework.stereotype.Service;
import javax.transaction.Transactional;
import java.util.*;

@Service
@Transactional
public class TopicsService implements LoadTopicUseCase, CreateTopicUseCase, EditTopicUseCase, CloseTopicUseCase,
        ReOpenTopicUseCase, DeleteTopicUseCase, GetTopicsDatabaseUseCase, VoteTopicUseCase,
        VotingResultsTopicUseCase {
    private final LoadTopicUseCase loadTopicUseCase;
    private final CreateTopicUseCase createTopicUseCase;
    private final EditTopicUseCase editTopicUseCase;
    private final CloseTopicUseCase closeTopicUseCase;
    private final ReOpenTopicUseCase reOpenTopicUseCase;
    private final DeleteTopicUseCase deleteTopicUseCase;
    private final GetTopicsDatabaseUseCase getTopicsDatabaseUseCase;
    private final VoteTopicUseCase voteTopicUseCase;
    private final VotingResultsTopicUseCase votingResultsTopicUseCase;

    public TopicsService(LoadTopicUseCase loadTopicUseCase, CreateTopicUseCase createTopicUseCase,
                         EditTopicUseCase editTopicUseCase, CloseTopicUseCase closeTopicUseCase,
                         ReOpenTopicUseCase reOpenTopicUseCase, DeleteTopicUseCase deleteTopicUseCase,
                         GetTopicsDatabaseUseCase getTopicsDatabaseUseCase, VoteTopicUseCase voteTopicUseCase,
                         VotingResultsTopicUseCase votingResultsTopicUseCase) {
        this.loadTopicUseCase = loadTopicUseCase;
        this.createTopicUseCase = createTopicUseCase;
        this.editTopicUseCase = editTopicUseCase;
        this.closeTopicUseCase = closeTopicUseCase;
        this.reOpenTopicUseCase = reOpenTopicUseCase;
        this.deleteTopicUseCase = deleteTopicUseCase;
        this.getTopicsDatabaseUseCase = getTopicsDatabaseUseCase;
        this.voteTopicUseCase = voteTopicUseCase;
        this.votingResultsTopicUseCase = votingResultsTopicUseCase;
    }

    @Override
    public List<TopicsModel> loadTopics(UsersModel usersModel) {
        try {
            return loadTopicUseCase.loadTopics(usersModel);
        } catch (LoadTopicException loadTopicsException) {
            throw loadTopicsException;
        }
    }

    @Override
    public void createTopic(TopicsModel topicsModel) {
        try {
            createTopicUseCase.createTopic(topicsModel);
        } catch (CreateTopicException createTopicsException) {
            throw createTopicsException;
        }
    }

    @Override
    public void editTopic(TopicsModel topicsModel) {
        try {
            editTopicUseCase.editTopic(topicsModel);
        } catch (EditTopicException editTopicsException) {
            throw editTopicsException;
        }
    }

    @Override
    public void closeTopic(TopicsModel topicsModel) {
        try {
            closeTopicUseCase.closeTopic(topicsModel);
        } catch (CloseTopicException closeTopicsException) {
            throw closeTopicsException;
        }
    }

    @Override
    public void reOpenTopic(TopicsModel topicsModel) {
        try {
            reOpenTopicUseCase.reOpenTopic(topicsModel);
        } catch (ReOpenTopicException reOpenTopicsException) {
            throw reOpenTopicsException;
        }
    }

    @Override
    public void deleteTopic(TopicsModel topicsModel) {
        try {
            deleteTopicUseCase.deleteTopic(topicsModel);
        } catch (DeleteTopicException deleteTopicException) {
            throw deleteTopicException;
        }
    }

    @Override
    public List<TopicsModel> getTopicsDatabase() {
        try {
            return getTopicsDatabaseUseCase.getTopicsDatabase();
        } catch (GetGroupsDatabaseException getGroupsDatabaseException) {
            throw getGroupsDatabaseException;
        }
    }

    @Override
    public void vote(TopicsModel topicsModel) {
        try {
            voteTopicUseCase.vote(topicsModel);
        } catch (VoteTopicException voteException) {
            throw voteException;
        }
    }

    @Override
    public Map<String, List<OptionsModel>> votingResults(TopicsModel topicsModel) {
        try {
            return votingResultsTopicUseCase.votingResults(topicsModel);
        } catch (VotingResultsTopicException votingResultsTopicException) {
            throw votingResultsTopicException;
        }
    }
}
