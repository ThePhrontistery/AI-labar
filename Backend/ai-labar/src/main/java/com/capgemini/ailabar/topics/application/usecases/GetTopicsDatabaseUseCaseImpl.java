package com.capgemini.ailabar.topics.application.usecases;

import com.capgemini.ailabar.groups.domain.ports.in.GetGroupsDatabaseUseCase;
import com.capgemini.ailabar.groups.infraestructure.entities.GroupsEntity;
import com.capgemini.ailabar.topics.domain.exceptions.GetTopicsDatabaseException;
import com.capgemini.ailabar.topics.domain.models.TopicsModel;
import com.capgemini.ailabar.topics.domain.ports.in.GetTopicsDatabaseUseCase;
import com.capgemini.ailabar.topics.domain.ports.out.TopicsRepositoryPort;
import com.capgemini.ailabar.topics.infraestructure.entities.TopicsEntity;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
@Transactional(readOnly = true)
public class GetTopicsDatabaseUseCaseImpl implements GetTopicsDatabaseUseCase {
    private final TopicsRepositoryPort topicsRepositoryPort;

    public GetTopicsDatabaseUseCaseImpl(TopicsRepositoryPort topicsRepositoryPort) {
        this.topicsRepositoryPort = topicsRepositoryPort;
    }

    @Override
    public List<TopicsEntity> getTopicsDatabase() {
        List<TopicsEntity> topicsList = topicsRepositoryPort.getTopicsDatabase();

        if (topicsList.isEmpty()) {
            throw new GetTopicsDatabaseException("There are no topics in database");
        }

        /* Revisar en la nueva versión ya que cambiará */
//        Gson gson = new Gson();
//        Type listType = new TypeToken<List<String>>() {}.getType();
//
//        topicsList.forEach(topic -> {
//            TopicsModel topicModel = new TopicsModel();
//            topicModel.setId(topic.getId());
//            topicModel.setTitle(topic.getTitle());
//            topicModel.setType(topic.getType());
//            topicModel.setQuestion(topic.getQuestion());
//
//            List<OptionsData> optionsDataList = gson.fromJson(topic.getOptions(), new TypeToken<List<OptionsData>>() {}.getType());
//            topicModel.setOptionsDataList(optionsDataList);
//
//            topicModel.setVotedBy(topic.getVotedBy());
//            topicModel.setAuthor(topic.getAuthor());
//            topicModel.setMembers(gson.fromJson(topic.getMembers(), listType));
//            topicModel.setVisits(topic.getVisits());
//            if(topic.getCloseDate() != null && !topic.getCloseDate().isBlank()) {
//                topicModel.setCloseDate(topic.getCloseDate());
//            }
//            topicModel.setStatus(topic.getStatus());
//            topicsModelList.add(topicModel);
//        });

        return topicsRepositoryPort.getTopicsDatabase();
    }
}
