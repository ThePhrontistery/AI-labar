package com.capgemini.ailabar.topics.infraestructure.repositories;

import com.capgemini.ailabar.options.infraestructure.entities.OptionsEntity;
import com.capgemini.ailabar.topics.infraestructure.entities.TopicsEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface TopicsRepository extends JpaRepository<TopicsEntity, Integer> {
    @Query("SELECT CASE WHEN COUNT(u) > 0 THEN TRUE ELSE FALSE END FROM UsersEntity u WHERE u.user = :user AND u.token = :token")
    boolean checkAuthorization(@Param("user") String user, @Param("token") String token);

    @Query("SELECT CASE WHEN COUNT(t) > 0 THEN true ELSE false END FROM TopicsEntity t WHERE t.title = :title AND t.author = :user")
    boolean checkByTitleAndAuthor(@Param("title") String title, @Param("user") String user);

    @Query("SELECT CASE WHEN COUNT(g) > 0 THEN true ELSE false END FROM GroupsEntity g WHERE g.groupName = :groupName")
    boolean checkIfGroupExists(String groupName);

    @Query("SELECT CASE WHEN COUNT(v) > 0 THEN TRUE ELSE FALSE END FROM VotedByEntity v WHERE v.topic.id = :topicId AND v.user.id = :userId")
    boolean checkIfUserAlreadyVoted(@Param("topicId") Integer topicId, @Param("userId") Integer userId);

    @Query("SELECT CASE WHEN COUNT(m) > 0 THEN TRUE ELSE FALSE END FROM MembersEntity m WHERE m.group.id = :groupId AND m.user.id = :userId")
    boolean checkIfUserCanVoteOnTopic(@Param("groupId") Integer groupId, @Param("userId") Integer userId);

    @Query("SELECT CASE WHEN COUNT(m) > 0 THEN TRUE ELSE FALSE END FROM MembersEntity m WHERE m.group.id = :groupId AND m.user.id = :userId")
    boolean checkIfUserIsMemberOfGroup(@Param("groupId") Integer groupId, @Param("userId") Integer userId);

    @Query("SELECT CASE WHEN COUNT(u) > 0 THEN true ELSE false END FROM UsersEntity u WHERE u.user = :member")
    boolean checkMember(@Param("member") String member);

    @Query(value = "SELECT COUNT(*) FROM topics " +
            "WHERE author = :user",
            nativeQuery = true)
    Integer countTopicsByAuthor(@Param("user") String user);

    @Query(value = "SELECT COUNT(*) FROM topics " +
            "WHERE author = :user " +
            "AND status = :status",
            nativeQuery = true)
    Integer countTopicsByAuthorWithStatus(@Param("user") String user,
                                          @Param("status") Integer status);

    @Query(value = "SELECT COUNT(*) FROM topics " +
            "WHERE (author = :user OR (group_id IN :groupIds AND author != :user)) " +
            "AND status = :status",
            nativeQuery = true)
    Integer countTopicsWithStatus(@Param("user") String user,
                                  @Param("groupIds") List<Integer> groupIds,
                                  @Param("status") Integer status);

    @Query(value = "SELECT COUNT(*) FROM topics " +
            "WHERE (author = :user OR (group_id IN :groupIds AND author != :user))",
            nativeQuery = true)
    Integer countTotalTopics(@Param("user") String user,
                             @Param("groupIds") List<Integer> groupIds);

    @Query(value = "SELECT COUNT(*) FROM topics t " +
            "WHERE (t.author = :user OR (t.group_id IN :groupIds AND t.author != :user)) " +
            "AND t.id NOT IN (SELECT v.topic_id FROM voted_by v WHERE v.user_id = :userId)",
            nativeQuery = true)
    Integer countVotableTopics(@Param("user") String user,
                               @Param("groupIds") List<Integer> groupIds,
                               @Param("userId") Integer userId);

    @Query(value = "SELECT COUNT(*) FROM topics t " +
            "WHERE t.author = :user " +
            "AND t.status = 1 " +
            "AND t.id NOT IN (SELECT v.topic_id FROM voted_by v WHERE v.user_id = :userId)",
            nativeQuery = true)
    Integer countVotableTopicsByAuthorWithStatus(@Param("user") String user,
                                                 @Param("userId") Integer userId);

    @Query(value = "SELECT COUNT(*) FROM topics t " +
            "WHERE (t.author = :user OR (t.group_id IN :groupIds AND t.author != :user)) " +
            "AND t.status = 1 " +
            "AND t.id NOT IN (SELECT v.topic_id FROM voted_by v WHERE v.user_id = :userId)",
            nativeQuery = true)
    Integer countVotableTopicsWithStatus(@Param("user") String user,
                                         @Param("groupIds") List<Integer> groupIds,
                                         @Param("userId") Integer userId);

    @Modifying
    @Query(value = "INSERT INTO groups (group_name, admin) VALUES (:groupName, :admin)", nativeQuery = true)
    void createTemporalGroup(@Param("groupName") String groupName, @Param("admin") String admin);

    @Modifying
    @Query("DELETE FROM GroupsEntity g WHERE g.id = :groupId")
    void deleteGroup(@Param("groupId") Integer groupId);

    @Modifying
    @Query("DELETE FROM MembersEntity m WHERE m.group.id = :groupId")
    void deleteMembersByGroupId(@Param("groupId") Integer groupId);

    @Modifying
    @Query("DELETE FROM OptionsEntity o WHERE o.topic.id = :topicId")
    void deleteOptions(@Param("topicId") Integer topicId);

    @Modifying
    @Query("DELETE FROM VotedByEntity v WHERE v.topic.id = :topicId")
    void deleteVotedByOnTopic(@Param("topicId") Integer topicId);

    @Query("SELECT t FROM TopicsEntity t WHERE t.status = :status AND t.closeDate <= :date")
    List<TopicsEntity> getByStatusAndCloseDateLessThanEqual(@Param("status") Integer status, @Param("date") String date);

    @Query("SELECT u.email FROM UsersEntity u WHERE u.id IN (SELECT m.user.id FROM MembersEntity m WHERE m.group.id = :groupId)")
    List<String> getEmailsByGroupId(@Param("groupId") Integer groupId);

    @Query("SELECT g.id FROM GroupsEntity g WHERE g.groupName = :groupName AND g.admin = :admin")
    Integer getGroupIdByGroupNameAndAdmin(@Param("groupName") String groupName, @Param("admin") String admin);

    @Query("SELECT g.groupName FROM GroupsEntity g WHERE g.id = :groupId")
    String getGroupNameByGroupId(@Param("groupId") Integer groupId);

    @Query("SELECT m.group.id FROM MembersEntity m WHERE m.user.id = :memberId")
    List<Integer> getGroupsWithMemberId(@Param("memberId") Integer memberId);

    @Query("SELECT o FROM OptionsEntity o WHERE o.topic.id = :topicId")
    List<OptionsEntity> getOptions(@Param("topicId") Integer topicId);

    @Query("SELECT t.id FROM TopicsEntity t WHERE t.title = :topicTitle")
    Integer getTopicIdByTopicName(@Param("topicTitle") String topicTitle);

    @Query("SELECT t FROM TopicsEntity t WHERE t.id = :id")
    TopicsEntity getTopicsEntityById(@Param("id") Integer id);

    @Query("SELECT u.id FROM UsersEntity u WHERE u.user = :user")
    Integer getUserIdByUserName(@Param("user") String user);

    @Query("SELECT u.user FROM UsersEntity u WHERE u.id = :userId")
    String getUserNameByUserId(@Param("userId") Integer userId);

    @Query("SELECT u.photo FROM UsersEntity u WHERE u.user = :user")
    String getUserPhotoByOption(@Param("user") String user);

    @Query("SELECT DISTINCT v.user.id FROM VotedByEntity v WHERE v.topic.id = :topicId")
    List<Integer> getUsersHasVotedByTopicId(@Param("topicId") Integer topicId);

    @Modifying
    @Query(value = "INSERT INTO members (group_id, user_id) VALUES (:groupId, :userId)", nativeQuery = true)
    void insertMember(@Param("groupId") Integer groupId, @Param("userId") Integer userId);

    @Modifying
    @Query(value = "INSERT INTO options (topic_id, option, votes) VALUES (:topicId, :option, :votes)", nativeQuery = true)
    void insertOption(@Param("topicId") Integer topicId, @Param("option") String option, @Param("votes") Integer votes);

    @Modifying
    @Query(value = "INSERT INTO options (topic_id, image, option, votes) VALUES (:topicId, :image, :option, :votes)", nativeQuery = true)
    void insertOption(@Param("topicId") Integer topicId, @Param("image") String image, @Param("option") String option, @Param("votes") Integer votes);

    @Query(value = "SELECT * FROM topics " +
            "WHERE (author = :user OR (group_id IN :groupIds AND author != :user)) " +
            "ORDER BY id DESC " +
            "LIMIT :limit OFFSET :offset", nativeQuery = true)
    List<TopicsEntity> loadTopics(@Param("user") String user,
                                  @Param("groupIds") List<Integer> groupIds,
                                  @Param("limit") Integer limit,
                                  @Param("offset") Integer offset);

    @Query(value = "SELECT * FROM topics " +
            "WHERE author = :user " +
            "ORDER BY id DESC " +
            "LIMIT :limit OFFSET :offset", nativeQuery = true)
    List<TopicsEntity> loadTopicsByAuthor(@Param("user") String user,
                                          @Param("limit") Integer limit,
                                          @Param("offset") Integer offset);

    @Query(value = "SELECT * FROM topics " +
            "WHERE author = :user " +
            "AND status = :status " +
            "ORDER BY id DESC " +
            "LIMIT :limit OFFSET :offset", nativeQuery = true)
    List<TopicsEntity> loadTopicsByAuthorWithStatus(@Param("user") String user,
                                                    @Param("status") Integer status,
                                                    @Param("limit") Integer limit,
                                                    @Param("offset") Integer offset);

    @Query(value = "SELECT * FROM topics " +
            "WHERE (author = :user OR (group_id IN :groupIds AND author != :user)) " +
            "AND status = :status " +
            "ORDER BY id DESC " +
            "LIMIT :limit OFFSET :offset", nativeQuery = true)
    List<TopicsEntity> loadTopicsWithStatus(@Param("user") String user,
                                            @Param("groupIds") List<Integer> groupIds,
                                            @Param("status") Integer status,
                                            @Param("limit") Integer limit,
                                            @Param("offset") Integer offset);

    @Query(value = "SELECT t.* FROM topics t " +
            "WHERE (t.author = :user OR (t.group_id IN :groupIds AND t.author != :user)) " +
            "AND t.id NOT IN (SELECT v.topic_id FROM voted_by v WHERE v.user_id = :userId) " +
            "ORDER BY t.id DESC " +
            "LIMIT :limit OFFSET :offset", nativeQuery = true)
    List<TopicsEntity> loadVotableTopics(@Param("user") String user,
                                         @Param("groupIds") List<Integer> groupIds,
                                         @Param("userId") Integer userId,
                                         @Param("limit") Integer limit,
                                         @Param("offset") Integer offset);

    @Query(value = "SELECT t.* FROM topics t " +
            "WHERE t.author = :user " +
            "AND t.status = 1 " +
            "AND t.id NOT IN (SELECT v.topic_id FROM voted_by v WHERE v.user_id = :userId) " +
            "ORDER BY t.id DESC " +
            "LIMIT :limit OFFSET :offset", nativeQuery = true)
    List<TopicsEntity> loadVotableTopicsByAuthorWithStatus(@Param("user") String user,
                                                           @Param("userId") Integer userId,
                                                           @Param("limit") Integer limit,
                                                           @Param("offset") Integer offset);

    @Query(value = "SELECT t.* FROM topics t " +
            "WHERE (t.author = :user OR (t.group_id IN :groupIds AND t.author != :user)) " +
            "AND t.status = 1 " +
            "AND t.id NOT IN (SELECT v.topic_id FROM voted_by v WHERE v.user_id = :userId) " +
            "ORDER BY t.id DESC " +
            "LIMIT :limit OFFSET :offset", nativeQuery = true)
    List<TopicsEntity> loadVotableTopicsWithStatus(@Param("user") String user,
                                                   @Param("groupIds") List<Integer> groupIds,
                                                   @Param("userId") Integer userId,
                                                   @Param("limit") Integer limit,
                                                   @Param("offset") Integer offset);

    @Modifying
    @Query(value = "INSERT INTO voted_by (topic_id, user_id) VALUES (:topicId, :userId)", nativeQuery = true)
    void registerUserVoted(@Param("topicId") Integer topicId, @Param("userId") Integer userId);

    @Modifying
    @Query("UPDATE OptionsEntity o SET o.image = :newImage WHERE o.id = :optionId")
    void updateOptionImage(@Param("optionId") Integer optionId, @Param("newImage") String newImage);

    @Modifying
    @Query("UPDATE OptionsEntity o SET o.votes = o.votes + 1 WHERE o.id = :optionId")
    void updateVotes(@Param("optionId") Integer optionId);
}
