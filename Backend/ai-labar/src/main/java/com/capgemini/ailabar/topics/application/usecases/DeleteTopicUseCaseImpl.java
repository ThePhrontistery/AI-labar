package com.capgemini.ailabar.topics.application.usecases;

import com.capgemini.ailabar.topics.domain.exceptions.DeleteTopicException;
import com.capgemini.ailabar.topics.domain.models.TopicsModel;
import com.capgemini.ailabar.topics.domain.ports.in.DeleteTopicUseCase;
import com.capgemini.ailabar.topics.domain.ports.out.TopicsRepositoryPort;
import com.capgemini.ailabar.topics.infraestructure.entities.TopicsEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional
public class DeleteTopicUseCaseImpl implements DeleteTopicUseCase {
    private final TopicsRepositoryPort topicsRepositoryPort;

    public DeleteTopicUseCaseImpl(TopicsRepositoryPort topicsRepositoryPort) {
        this.topicsRepositoryPort = topicsRepositoryPort;
    }

    @Override
    public void deleteTopic(TopicsModel topicsModel) {
        if(Boolean.FALSE.equals(topicsRepositoryPort.checkAuthorization(topicsModel.getUser(), topicsModel.getToken()))) {
            throw new DeleteTopicException("Unauthorized user");
        }

        TopicsEntity topicsEntity = topicsRepositoryPort.getTopicsEntityById(topicsModel.getId());

        if(topicsEntity == null) {
            throw new DeleteTopicException("There is no topic with that id");
        }

        if(!topicsEntity.getAuthor().equals(topicsModel.getUser())) {
            throw new DeleteTopicException("The user is not the author of the topic");
        }

        String groupName = topicsRepositoryPort.getGroupNameByGroupId(topicsEntity.getGroupId());
        if(groupName.startsWith("*temp*")) {
            topicsRepositoryPort.deleteMembersByGroupId(topicsEntity.getGroupId());
            topicsRepositoryPort.deleteGroup(topicsEntity.getGroupId());
        }
        topicsRepositoryPort.deleteOptions(topicsModel.getId());
        topicsRepositoryPort.deleteVotedByOnTopic(topicsModel.getId());
        topicsRepositoryPort.deleteTopic(topicsModel.getId());
    }
}
