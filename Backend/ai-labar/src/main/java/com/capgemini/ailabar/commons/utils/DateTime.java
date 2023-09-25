package com.capgemini.ailabar.commons.utils;

import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.time.Instant;
import java.util.Date;

public final class DateTime {
    private DateTime() {}

    public static Timestamp actualDateAndTime() {
        Instant instant = Instant.now();
        return Timestamp.from(instant);
    }

    public static String timestampToString(Timestamp timestamp) {
        if (timestamp==null) {return null;}
        Date date = new Date(timestamp.getTime());
        SimpleDateFormat dateFormat = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss");
        return dateFormat.format(date);
    }

    public static String timestampToStringWithoutTime(Timestamp timestamp) {
        if (timestamp==null) {return null;}
        Date date = new Date(timestamp.getTime());
        SimpleDateFormat dateFormat = new SimpleDateFormat("dd/MM/yyyy");
        return dateFormat.format(date);
    }
}
