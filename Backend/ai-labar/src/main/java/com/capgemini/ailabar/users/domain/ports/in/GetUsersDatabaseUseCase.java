package com.capgemini.ailabar.users.domain.ports.in;

import com.capgemini.ailabar.users.infraestructure.entities.UsersEntity;

import java.util.List;

public interface GetUsersDatabaseUseCase {
    List<UsersEntity> getUsersDatabase();
}
