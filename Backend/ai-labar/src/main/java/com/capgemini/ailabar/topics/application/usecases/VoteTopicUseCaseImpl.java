package com.capgemini.ailabar.topics.application.usecases;

import com.capgemini.ailabar.commons.utils.Constants;
import com.capgemini.ailabar.commons.utils.DateTime;
import com.capgemini.ailabar.options.infraestructure.entities.OptionsEntity;
import com.capgemini.ailabar.topics.domain.exceptions.VoteTopicException;
import com.capgemini.ailabar.topics.domain.models.TopicsModel;
import com.capgemini.ailabar.topics.domain.ports.in.VoteTopicUseCase;
import com.capgemini.ailabar.topics.domain.ports.out.TopicsRepositoryPort;
import com.capgemini.ailabar.topics.infraestructure.entities.TopicsEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
@Transactional
public class VoteTopicUseCaseImpl implements VoteTopicUseCase {
    private final TopicsRepositoryPort topicsRepositoryPort;

    public VoteTopicUseCaseImpl(TopicsRepositoryPort topicsRepositoryPort) {
        this.topicsRepositoryPort = topicsRepositoryPort;
    }

    @Override
    public void vote(TopicsModel topicsModel) {
        if(Boolean.FALSE.equals(topicsRepositoryPort.checkAuthorization(topicsModel.getUser(), topicsModel.getToken()))) {
            throw new VoteTopicException("Unauthorized user");
        }

        TopicsEntity topicsEntity = topicsRepositoryPort.getTopicsEntityById(topicsModel.getId());
        Integer userId = topicsRepositoryPort.getUserIdByUserName(topicsModel.getUser());

        if(topicsEntity == null) {
            throw new VoteTopicException("There is no topic with that id");
        }

        if(topicsModel.getVotation().isEmpty()) {
            throw new VoteTopicException("The voting cannot be empty");
        }

        if(!topicsRepositoryPort.checkIfUserCanVoteOnTopic(topicsEntity.getGroupId(), userId) && !topicsModel.getUser().equals(topicsEntity.getAuthor())) {
            throw new VoteTopicException("The user is not allowed to vote on this topic");
        }


        if(topicsRepositoryPort.checkIfUserAlreadyVoted(topicsModel.getId(), userId)) {
            throw new VoteTopicException("The user has already voted");
        }

        Constants.TopicType topicType = Constants.TopicType.valueOf(topicsEntity.getType());
        if (topicsModel.getVotation().size() > 1 && !(topicType == Constants.TopicType.TEXT_MULTIPLE || topicType == Constants.TopicType.IMAGE_MULTIPLE)) {
            throw new VoteTopicException("The topic type is not valid for multiple voting options");
        }

        updateVotations(topicsEntity.getId(), topicsModel.getVotation());

        topicsEntity.setVisits(topicsEntity.getVisits() + 1);

        registerUserVoted(topicsModel.getId(), topicsRepositoryPort.getUserIdByUserName(topicsModel.getUser()));
    }

    private void updateVotations(Integer topicId, List<String> votationList) {
        boolean matchFound = false;

        for (String vote : votationList) {
            for (OptionsEntity option : topicsRepositoryPort.getOptions(topicId)) {
                if (option.getOption().equals(vote)) {
                    topicsRepositoryPort.updateVotes(option.getId());
                    matchFound = true;
                }
            }
        }

        if (!matchFound) {
            throw new VoteTopicException("No matching options found");
        }
    }

    private void registerUserVoted(Integer topicId, Integer userId) {
        try {
            topicsRepositoryPort.registerUserVoted(topicId, userId, DateTime.actualDateAndTime());
        } catch (VoteTopicException voteException) {
            throw new VoteTopicException("An error occurred while registering the user as a voter");
        }
    }
}
