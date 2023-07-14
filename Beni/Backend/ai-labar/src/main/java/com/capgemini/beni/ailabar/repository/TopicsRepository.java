package com.capgemini.beni.ailabar.repository;

import com.capgemini.beni.ailabar.entity.TopicsEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface TopicsRepository extends JpaRepository<TopicsEntity, Integer> {
    @Query("SELECT CASE WHEN COUNT(u) > 0 THEN TRUE ELSE FALSE END FROM UsersEntity u WHERE u.user = :user AND u.password = :password")
    boolean existsByUserAndPassword(@Param("user") String user, @Param("password") String password);

    @Query("SELECT t FROM TopicsEntity t WHERE t.author = :user OR t.members LIKE CONCAT('%', :user, '%')")
    List<TopicsEntity> findByUser(@Param("user") String user);

    @Query("SELECT t FROM TopicsEntity t WHERE t.id = :id")
    TopicsEntity findTopicsEntityById(@Param("id") Integer id);

    @Query("SELECT CASE WHEN COUNT(t) > 0 THEN true ELSE false END FROM TopicsEntity t WHERE t.title = :title AND t.author = :author")
    Boolean existsByTitleAndAuthor(@Param("title") String title, @Param("author") String author);

    @Query("SELECT t FROM TopicsEntity t WHERE t.id = :id")
    TopicsEntity findByIdIfExists(@Param("id") Integer id);

    @Query("SELECT t FROM TopicsEntity t WHERE t.id = :id AND (t.author = :user OR :user IN (t.members))")
    TopicsEntity findTopicByIdAndUser(@Param("id") Integer id, @Param("user") String user);
}