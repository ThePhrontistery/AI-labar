package com.capgemini.ailabar.users.domain.exceptions;

public class DeleteUserException extends RuntimeException {
    public DeleteUserException(String message) {
        super(message);
    }
}
