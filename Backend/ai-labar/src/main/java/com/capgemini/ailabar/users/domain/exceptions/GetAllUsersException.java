package com.capgemini.ailabar.users.domain.exceptions;

public class GetAllUsersException extends RuntimeException {
    public GetAllUsersException(String message) {
        super(message);
    }
}
