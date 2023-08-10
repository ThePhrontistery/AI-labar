package com.capgemini.ailabar.topics.domain.exceptions;

public class MailServiceException extends RuntimeException {
    public MailServiceException(String message) {
        super(message);
    }
}
