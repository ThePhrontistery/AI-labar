package com.capgemini.beni.ailabar;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.scheduling.annotation.EnableScheduling;

@SpringBootApplication
@EnableScheduling
public class AiLabarApplication {
	public static void main(String[] args) {
		SpringApplication.run(AiLabarApplication.class, args);
	}

}
