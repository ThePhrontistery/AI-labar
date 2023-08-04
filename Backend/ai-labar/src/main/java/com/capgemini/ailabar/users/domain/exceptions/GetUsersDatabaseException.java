package com.capgemini.ailabar.users.domain.exceptions;

public class GetUsersDatabaseException extends RuntimeException {
    public GetUsersDatabaseException(String message) {
        super(message);
    }
}
