package com.capgemini.ailabar.topics.infraestructure.adapters.out;

import com.capgemini.ailabar.options.infraestructure.entities.OptionsEntity;
import com.capgemini.ailabar.topics.domain.ports.out.TopicsRepositoryPort;
import com.capgemini.ailabar.topics.infraestructure.entities.TopicsEntity;
import com.capgemini.ailabar.topics.infraestructure.repositories.TopicsRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public class TopicsRepositoryAdapter implements TopicsRepositoryPort {
    private final TopicsRepository topicsRepository;

    public TopicsRepositoryAdapter(TopicsRepository topicsRepository) {
        this.topicsRepository = topicsRepository;
    }

    @Override
    public boolean checkAuthorization(String user, String token) {
        return topicsRepository.checkAuthorization(user, token);
    }

    @Override
    public boolean checkByTitleAndAuthor(String title, String user) {
        return topicsRepository.checkByTitleAndAuthor(title, user);
    }

    @Override
    public boolean checkId(Integer id) {
        return topicsRepository.existsById(id);
    }

    @Override
    public boolean checkIfGroupExists(String groupName) {
        return topicsRepository.checkIfGroupExists(groupName);
    }

    @Override
    public boolean checkIfUserAlreadyVoted(Integer topidId, Integer userId) {
        return topicsRepository.checkIfUserAlreadyVoted(topidId, userId);
    }

    @Override
    public boolean checkMember(String member) {
        return topicsRepository.checkMember(member);
    }

    @Override
    public void closeTopic(TopicsEntity topicsEntity) {
        topicsRepository.save(topicsEntity);
    }

    @Override
    public boolean checkIfUserCanVoteOnTopic(Integer groupId, Integer userId) {
        return topicsRepository.checkIfUserCanVoteOnTopic(groupId, userId);
    }

    @Override
    public void createTemporalGroup(String groupName, String admin) {
        topicsRepository.createTemporalGroup(groupName, admin);
    }

    @Override
    public void createTopic(TopicsEntity topicsEntity) {
        topicsRepository.save(topicsEntity);
    }

    @Override
    public void deleteGroup(Integer groupId) {
        topicsRepository.deleteGroup(groupId);
    }

    @Override
    public void deleteMembersByGroupId(Integer groupId) {
        topicsRepository.deleteMembersByGroupId(groupId);
    }

    @Override
    public void deleteOptions(Integer topicId) {
        topicsRepository.deleteOptions(topicId);
    }

    @Override
    public void deleteTopic(Integer id) {
        topicsRepository.deleteById(id);
    }

    @Override
    public void deleteVotedByOnTopic(Integer topidId) {
        topicsRepository.deleteVotedByOnTopic(topidId);
    }

    @Override
    public void editTopic(TopicsEntity topicsEntity) {
        topicsRepository.save(topicsEntity);
    }

    @Override
    public Integer getGroupIdByGroupNameAndAdmin(String groupName, String admin) {
        return topicsRepository.getGroupIdByGroupNameAndAdmin(groupName, admin);
    }

    @Override
    public String getGroupNameByGroupId(Integer groupId) {
        return topicsRepository.getGroupNameByGroupId(groupId);
    }

    @Override
    public List<Integer> getGroupsWithMemberId(Integer memberId) {
        return topicsRepository.getGroupsWithMemberId(memberId);
    }

    @Override
    public List<OptionsEntity> getOptions(Integer topicId) {
        return topicsRepository.getOptions(topicId);
    }

    @Override
    public Integer getTopicIdByTopicName(String topicTitle) {
        return topicsRepository.getTopicIdByTopicName(topicTitle);
    }

    @Override
    public List<TopicsEntity> getTopicsDatabase() {
        return topicsRepository.findAll();
    }

    @Override
    public TopicsEntity getTopicsEntityById(Integer id) {
        return topicsRepository.getTopicsEntityById(id);
    }

    @Override
    public Integer getUserIdByUserName(String user) {
        return topicsRepository.getUserIdByUserName(user);
    }

    @Override
    public String getUserNameByUserId(Integer userId) {
        return topicsRepository.getUserNameByUserId(userId);
    }

    @Override
    public String getUserPhotoByOption(String user) {
        return topicsRepository.getUserPhotoByOption(user);
    }

    @Override
    public List<Integer> getUsersHasVotedByTopicId(Integer topicId) {
        return topicsRepository.getUsersHasVotedByTopicId(topicId);
    }

    @Override
    public void insertMember(Integer groupId, Integer userId) {
        topicsRepository.insertMember(groupId, userId);
    }

    @Override
    public void insertOption(Integer topicId, String option, Integer votes) {
        topicsRepository.insertOption(topicId, option, votes);
    }

    @Override
    public void insertOption(Integer topicId, String image, String option, Integer votes) {
        topicsRepository.insertOption(topicId, image, option, votes);
    }

    @Override
    public List<TopicsEntity> loadTopicsByAuthor(String user) {
        return topicsRepository.loadTopicsByAuthor(user);
    }

    @Override
    public List<TopicsEntity> loadTopicsByGroupId(Integer groupId) {
        return topicsRepository.loadTopicsByGroupId(groupId);
    }

    @Override
    public void reOpenTopic(TopicsEntity topicsEntity) {
        topicsRepository.save(topicsEntity);
    }

    @Override
    public void registerUserVoted(Integer topicId, Integer userId) {
        topicsRepository.registerUserVoted(topicId, userId);
    }

    @Override
    public void updateOptionImage(Integer optionId, String newImage) {
        topicsRepository.updateOptionImage(optionId, newImage);
    }

    @Override
    public void updateVotes(Integer optionId) {
        topicsRepository.updateVotes(optionId);
    }
}
