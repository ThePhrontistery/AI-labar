package com.capgemini.ailabar.topics.application.usecases;

import com.capgemini.ailabar.commons.utils.Constants;
import com.capgemini.ailabar.options.domain.models.OptionsModel;
import com.capgemini.ailabar.topics.domain.exceptions.VotingResultsTopicException;
import com.capgemini.ailabar.topics.domain.models.TopicsModel;
import com.capgemini.ailabar.topics.domain.ports.in.VotingResultsTopicUseCase;
import com.capgemini.ailabar.topics.domain.ports.out.TopicsRepositoryPort;
import com.capgemini.ailabar.topics.infraestructure.entities.TopicsEntity;
import com.capgemini.ailabar.topics.infraestructure.utils.TopicsUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Service
@Transactional(readOnly = true)
public class VotingResultsTopicUseCaseImpl implements VotingResultsTopicUseCase {
    private final TopicsRepositoryPort topicsRepositoryPort;

    public VotingResultsTopicUseCaseImpl(TopicsRepositoryPort topicsRepositoryPort) {
        this.topicsRepositoryPort = topicsRepositoryPort;
    }

    @Override
    public Map<String, List<OptionsModel>> votingResults(TopicsModel topicsModel) {
        if(Boolean.FALSE.equals(topicsRepositoryPort.checkAuthorization(topicsModel.getUser(), topicsModel.getToken()))) {
            throw new VotingResultsTopicException("Unauthorized user");
        }

        TopicsEntity topicsEntity = topicsRepositoryPort.getTopicsEntityById(topicsModel.getId());
        Integer userId = topicsRepositoryPort.getUserIdByUserName(topicsModel.getUser());

        if(topicsEntity == null) {
            throw new VotingResultsTopicException("There is no topic with that id");
        }

        if(topicsEntity.getStatus().equals(1)) {
            throw new VotingResultsTopicException("The topic is not closed, so it is not possible to view the results");
        }

        if(!topicsRepositoryPort.checkIfUserCanVoteOnTopic(topicsEntity.getGroupId(), userId) && !topicsModel.getUser().equals(topicsEntity.getAuthor())) {
            throw new VotingResultsTopicException("The user is not allowed to view the results on this topic");
        }

        topicsEntity.setVisits(topicsEntity.getVisits() + 1);

        String topicType = topicsEntity.getType();
        List<OptionsModel> optionsModelList = TopicsUtils.transformToOptionsModelList(topicsRepositoryPort.getOptions(topicsModel.getId()));

        if(topicType.equals(String.valueOf(Constants.TopicType.AS))) {
            getUsersPhotos(optionsModelList);
        }

        Map<String, List<OptionsModel>> mapTopicTypeAndOptionsModelList = new HashMap<>();
        mapTopicTypeAndOptionsModelList.put(topicType, optionsModelList);

        return mapTopicTypeAndOptionsModelList;
    }

    private void getUsersPhotos(List<OptionsModel> optionsModelList) {
        for (OptionsModel optionsModel : optionsModelList) {
            String userPhoto = topicsRepositoryPort.getUserPhotoByOption(optionsModel.getOption());
            if (userPhoto != null && !userPhoto.isEmpty()) {
                optionsModel.setImage(userPhoto);
            }
        }
    }
}
