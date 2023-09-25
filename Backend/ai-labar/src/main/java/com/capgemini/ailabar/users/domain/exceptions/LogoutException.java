package com.capgemini.ailabar.users.domain.exceptions;

public class LogoutException extends RuntimeException {
    public LogoutException(String message) {
        super(message);
    }
}
