package com.capgemini.ailabar.topics.infraestructure.utils;

import com.capgemini.ailabar.commons.utils.Constants;
import com.capgemini.ailabar.options.domain.models.OptionsModel;
import com.capgemini.ailabar.options.infraestructure.entities.OptionsEntity;
import com.capgemini.ailabar.votedby.domain.models.VotedByModel;
import com.capgemini.ailabar.votedby.infraestructure.entities.VotedByEntity;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public final class TopicsUtils {
    private TopicsUtils() {}

    public static String checkTopicType(String type) {
        try {
            return String.valueOf(Constants.TopicType.valueOf(type));
        } catch (IllegalArgumentException e) {
            return "KO";
        }
    }

    public static boolean validateOptionsDataList(List<OptionsModel> list) {
        return !list.stream()
                .allMatch(optionsModel -> optionsModel.getImage() != null
                        && !optionsModel.getImage().isEmpty()
                        && optionsModel.getOption() != null
                        && !optionsModel.getOption().isEmpty());
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


    public static List<OptionsModel> initiateVoting(String type, List<OptionsModel> list) {
        return list.stream()
                .map(element -> {
                    if (type.equals(Constants.TopicType.IMAGE_SINGLE.toString()) || type.equals(Constants.TopicType.IMAGE_MULTIPLE.toString())) {
                        return new OptionsModel(element.getImage(), element.getOption(), 0);
                    } else {
                        return new OptionsModel(element.getOption(), 0);
                    }
                })
                .collect(Collectors.toList());
    }

    public static List<OptionsModel> transformToOptionsModelList(List<OptionsEntity> optionsEntityList) {
        return optionsEntityList.stream()
                .map(entity -> {
                    OptionsModel optionsModel;
                    if (entity.getImage() != null) {
                        optionsModel = new OptionsModel(entity.getImage(), entity.getOption(), entity.getVotes());
                    } else {
                        optionsModel = new OptionsModel(entity.getOption(), entity.getVotes());
                    }
                    return optionsModel;
                })
                .collect(Collectors.toList());
    }

    public static boolean checkIfOptionsChanged(List<OptionsModel> actualOptions, List<OptionsModel> newOptions) {
        List<OptionsModel> sortedActualOptions = actualOptions.stream()
                .sorted(Comparator.comparing(OptionsModel::getOption))
                .collect(Collectors.toList());

        List<OptionsModel> sortedNewOptions = newOptions.stream()
                .sorted(Comparator.comparing(OptionsModel::getOption))
                .collect(Collectors.toList());

        if (sortedActualOptions.size() != sortedNewOptions.size()) {
            return true;
        }

        return IntStream.range(0, sortedActualOptions.size())
                .anyMatch(i -> !sortedActualOptions.get(i).getOption().equals(sortedNewOptions.get(i).getOption()));
    }


    public static List<OptionsModel> checkIfImagesChanged(List<OptionsModel> actualOptions, List<OptionsModel> newOptions) {
        List<OptionsModel> sortedActualOptions = actualOptions.stream()
                .sorted(Comparator.comparing(OptionsModel::getImage))
                .collect(Collectors.toList());

        List<OptionsModel> sortedNewOptions = newOptions.stream()
                .sorted(Comparator.comparing(OptionsModel::getImage))
                .collect(Collectors.toList());

        return IntStream.range(0, sortedActualOptions.size())
                .mapToObj(i -> {
                    OptionsModel actualOption = sortedActualOptions.get(i);
                    OptionsModel newOption = sortedNewOptions.get(i);

                    if (!actualOption.getImage().equals(newOption.getImage())) {
                        return new OptionsModel(actualOption.getId(), newOption.getImage());
                    } else {
                        return actualOption;
                    }
                })
                .collect(Collectors.toList());
    }

}
