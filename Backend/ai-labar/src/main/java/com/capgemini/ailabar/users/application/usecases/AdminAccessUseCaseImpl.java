package com.capgemini.ailabar.users.application.usecases;

import com.capgemini.ailabar.users.domain.exceptions.AdminAccessException;
import com.capgemini.ailabar.users.domain.models.UsersModel;
import com.capgemini.ailabar.users.domain.ports.in.AdminAccessUseCase;
import com.capgemini.ailabar.users.domain.ports.out.UsersRepositoryPort;
import org.apache.commons.codec.digest.DigestUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

@Service
public class AdminAccessUseCaseImpl implements AdminAccessUseCase {
    @Value("${admin.access}")
    private String adminPass;
    private final UsersRepositoryPort usersRepositoryPort;

    public AdminAccessUseCaseImpl(UsersRepositoryPort usersRepositoryPort) {
        this.usersRepositoryPort = usersRepositoryPort;
    }

    @Override
    public void adminAccess(UsersModel usersModel) {
        if(usersModel.getPassword().isBlank()) {
            throw new AdminAccessException("Password is required");
        }

        if (Boolean.FALSE.equals(usersModel.getPassword().equals(DigestUtils.sha256Hex(adminPass)))) {
            throw new AdminAccessException("Wrong Password");
        }
    }
}
