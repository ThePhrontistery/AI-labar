package com.capgemini.ailabar.users.domain.exceptions;

public class GetUsersByMatchException extends RuntimeException {
    public GetUsersByMatchException(String message) {
        super(message);
    }
}
