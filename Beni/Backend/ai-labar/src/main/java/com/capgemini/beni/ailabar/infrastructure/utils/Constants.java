package com.capgemini.beni.ailabar.infrastructure.utils;

public class Constants {
    private Constants() {}
    public static final String STATUS_OPENED = "Abierto";
    public static final String STATUS_CLOSED = "Cerrado";

    public enum TopicType {
        TEXT_SINGLE,
        TEXT_MULTIPLE,
        IMAGE_SINGLE,
        IMAGE_MULTIPLE,
        AS,
        RATING
    }
}
