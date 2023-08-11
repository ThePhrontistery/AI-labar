package com.capgemini.ailabar.topics.domain.ports.out;

import com.capgemini.ailabar.options.infraestructure.entities.OptionsEntity;
import com.capgemini.ailabar.topics.infraestructure.entities.TopicsEntity;

import java.util.List;

public interface TopicsRepositoryPort {
    boolean checkAuthorization(String user, String token);

    boolean checkByTitleAndAuthor(String title, String user);

    boolean checkId(Integer id);

    boolean checkIfGroupExists(String groupName);

    boolean checkIfUserAlreadyVoted(Integer topicId, Integer userId);

    boolean checkIfUserCanVoteOnTopic(Integer groupId, Integer userId);

    boolean checkIfUserIsMemberOfGroup(Integer groupId, Integer userId);

    boolean checkMember(String member);

    void closeTopic(TopicsEntity topicsEntity);

    void createTemporalGroup(String groupName, String admin);

    void createTopic(TopicsEntity topicsEntity);

    void deleteGroup(Integer groupId);

    void deleteMembersByGroupId(Integer groupId);

    void deleteOptions(Integer topicId);

    void deleteTopic(Integer id);

    void deleteVotedByOnTopic(Integer topicId);

    void editTopic(TopicsEntity topicsEntity);

    Integer getGroupIdByGroupNameAndAdmin(String groupName, String admin);

    String getGroupNameByGroupId(Integer groupId);

    List<Integer> getGroupsWithMemberId(Integer memberId);

    List<OptionsEntity> getOptions(Integer topicId);

    Integer getTopicIdByTopicName(String topicTitle);

    List<TopicsEntity> getTopicsDatabase();

    TopicsEntity getTopicsEntityById(Integer id);

    Integer getUserIdByUserName(String user);

    String getUserNameByUserId(Integer userId);

    String getUserPhotoByOption(String user);

    List<Integer> getUsersHasVotedByTopicId(Integer topicId);

    void insertMember(Integer groupId, Integer userId);

    void insertOption(Integer topicId, String option, Integer votes);

    void insertOption(Integer topicId, String image, String option, Integer votes);

    List<TopicsEntity> loadTopicsByAuthor(String user);

    List<TopicsEntity> loadTopicsByGroupId(Integer groupId);

//    List<TopicsEntity> loadTopics(@Param("user") String user, @Param("groupId") Integer groupId);

    void reOpenTopic(TopicsEntity topicsEntity);

    void registerUserVoted(Integer topicId, Integer userId);

    void updateOptionImage(Integer optionId, String newImage);

    void updateVotes(Integer optionId);
}
