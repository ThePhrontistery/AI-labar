package com.capgemini.ailabar;

import com.capgemini.ailabar.commons.utils.AccessPom;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.scheduling.annotation.EnableScheduling;

@SpringBootApplication
@EnableScheduling
public class AiLabarApplication {
	private static final Logger logger = LogManager.getLogger(AiLabarApplication.class);

	public static void main(String[] args) {
		SpringApplication.run(AiLabarApplication.class, args);
		logger.info(AccessPom.showAppVersion());
	}
}
