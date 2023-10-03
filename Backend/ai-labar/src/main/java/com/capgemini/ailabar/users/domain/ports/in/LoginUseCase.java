package com.capgemini.ailabar.users.domain.ports.in;

import com.capgemini.ailabar.users.domain.models.UsersModel;

import java.security.PrivateKey;
import java.util.List;

public interface LoginUseCase {
    List<String> login(UsersModel usersModel, PrivateKey privateKey);
}
