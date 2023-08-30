package com.capgemini.ailabar.commons.configurations;

import com.fasterxml.jackson.annotation.JsonInclude;
import org.springframework.beans.factory.annotation.Value;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.converter.HttpMessageConverter;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.web.servlet.config.annotation.CorsRegistry;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;
import springfox.documentation.swagger2.annotations.EnableSwagger2;

import java.util.List;

@Configuration
@EnableWebMvc
@EnableSwagger2
public class CorsConfiguration implements WebMvcConfigurer {
    @Value("${allowed.origins}")
    private String allowedOrigins;

    @Value("${allowed.port}")
    private String allowedPort;
    @Override
    public void addCorsMappings(CorsRegistry registry) {
        String allowedOrigin = allowedOrigins + ":" + allowedPort;
        registry.addMapping("/**")
                .allowedOrigins(allowedOrigin)
                .allowedMethods("GET", "POST", "PUT", "DELETE", "OPTIONS");
    }

    @Override
    public void configureMessageConverters(List<HttpMessageConverter<?>> converters) {
        MappingJackson2HttpMessageConverter converter = new MappingJackson2HttpMessageConverter();
        ObjectMapper objectMapper = converter.getObjectMapper();
        objectMapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
        converters.add(converter);
    }
}
