package com.capgemini.ailabar.users.domain.exceptions;

public class AdminAccessException extends RuntimeException {
    public AdminAccessException(String message) {
        super(message);
    }
}
