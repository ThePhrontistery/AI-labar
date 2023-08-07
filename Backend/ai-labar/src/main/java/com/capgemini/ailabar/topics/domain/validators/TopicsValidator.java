package com.capgemini.ailabar.topics.domain.validators;

import com.capgemini.ailabar.commons.utils.Constants;
import com.capgemini.ailabar.commons.utils.OptionsData;
import com.google.gson.Gson;
import com.google.gson.JsonElement;
import com.google.gson.JsonParser;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

public final class TopicsValidator {
    private TopicsValidator() {}

    public static String checkTopicType(String type) {
        try {
            return String.valueOf(Constants.TopicType.valueOf(type));
        } catch (IllegalArgumentException e) {
            return "KO";
        }
    }

    public static boolean validateOptionsDataList(List<OptionsData> list) {
        return list.stream()
                .allMatch(optionsData -> optionsData.getImage() != null
                        && !optionsData.getImage().isEmpty()
                        && optionsData.getOption() != null
                        && !optionsData.getOption().isEmpty());
    }

    public static String validateFormatDate(String inputDate) {
        String cleanedDate = inputDate.replaceAll("\\s+", "");

        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("[yyyy-MM-dd][yyyy/MM/dd][dd-MM-yyyy][dd/MM/yyyy][MM/dd/yyyy][yyyyMMdd]");
        try {
            LocalDate localDate = LocalDate.parse(cleanedDate, formatter);
            return localDate.format(DateTimeFormatter.ofPattern("yyyyMMdd"));
        } catch (DateTimeParseException e) {
            return "KO - Error while parsing the date: " + e.getMessage();
        }
    }

    /* Esto cambiará con la nueva versión */
    public static String initiateVoting(String type, List<OptionsData> list) {
        List<OptionsData> optionsDataList = list.stream()
                .map(element -> {
                    if (type.equals(Constants.TopicType.IMAGE_SINGLE.toString()) || type.equals(Constants.TopicType.IMAGE_MULTIPLE.toString())) {
                        return new OptionsData(element.getImage(), element.getOption(), 0);
                    } else {
                        return new OptionsData(element.getOption(), 0);
                    }
                })
                .collect(Collectors.toList());

        Gson gson = new Gson();
        return gson.toJson(optionsDataList);
    }

        public static List<OptionsData> getOptionsWithoutVotes(String input) {
        Gson gson = new Gson();
        JsonElement jsonElement = JsonParser.parseString(input);

        return StreamSupport.stream(jsonElement.getAsJsonArray().spliterator(), false)
                .map(JsonElement::getAsJsonObject)
                .map(jsonObject -> {
                    jsonObject.remove("votes");
                    return gson.fromJson(jsonObject, OptionsData.class);
                })
                .collect(Collectors.toList());
    }
}
