package com.capgemini.ailabar.topics.infraestructure.repositories;

import com.capgemini.ailabar.topics.infraestructure.entities.TopicsEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface TopicsRepository extends JpaRepository<TopicsEntity, Integer> {
    @Query("SELECT t FROM TopicsEntity t WHERE t.author = :user OR t.members LIKE CONCAT('%', :user, '%')")
    List<TopicsEntity> loadTopicsByUser(@Param("user") String user);

    @Query("SELECT t FROM TopicsEntity t WHERE t.id = :id")
    TopicsEntity getTopicsEntityById(@Param("id") Integer id);

    @Query("SELECT CASE WHEN COUNT(u) > 0 THEN TRUE ELSE FALSE END FROM UsersEntity u WHERE u.user = :user AND u.token = :token")
    boolean checkAuthorization(@Param("user") String user, @Param("token") String token);

    @Query("SELECT CASE WHEN COUNT(t) > 0 THEN true ELSE false END FROM TopicsEntity t WHERE t.title = :title AND t.author = :user")
    boolean checkByTitleAndAuthor(@Param("title") String title, @Param("user") String user);





    @Query("SELECT t FROM TopicsEntity t WHERE t.id = :id")
    TopicsEntity findByIdIfExists(@Param("id") Integer id);

    @Query("SELECT t FROM TopicsEntity t WHERE t.id = :id AND (t.author = :user OR :user IN (t.members))")
    TopicsEntity findTopicByIdAndUser(@Param("id") Integer id, @Param("user") String user);

    @Query("SELECT t FROM TopicsEntity t WHERE t.status = :status AND t.closeDate <= :date")
    List<TopicsEntity> findByStatusAndCloseDateLessThanEqual(@Param("status") String status, @Param("date") String date);
}
