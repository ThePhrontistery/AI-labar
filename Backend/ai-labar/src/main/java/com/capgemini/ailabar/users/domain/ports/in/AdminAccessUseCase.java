package com.capgemini.ailabar.users.domain.ports.in;

import com.capgemini.ailabar.users.domain.models.UsersModel;

public interface AdminAccessUseCase {
    void adminAccess(UsersModel usersModel);
}
