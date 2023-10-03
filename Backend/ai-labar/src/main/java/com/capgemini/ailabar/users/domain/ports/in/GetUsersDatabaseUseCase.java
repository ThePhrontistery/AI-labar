package com.capgemini.ailabar.users.domain.ports.in;

import com.capgemini.ailabar.users.domain.models.UsersModel;

import java.util.List;

public interface GetUsersDatabaseUseCase {
    List<UsersModel> getUsersDatabase();
}
