package com.capgemini.beni.ailabar.repository;

import com.capgemini.beni.ailabar.entity.VotingEntity;
import org.springframework.data.jpa.repository.JpaRepository;

public interface VotingRepository extends JpaRepository<VotingEntity, Integer> {
    // Implementación del repositorio de votación
}
