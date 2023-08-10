package com.capgemini.ailabar.users.domain.exceptions;

public class CreateUserException extends RuntimeException {
    public CreateUserException(String message) {
        super(message);
    }
}
