package com.capgemini.beni.ailabar.config;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.context.SpringBootTest;
import springfox.documentation.spring.web.plugins.Docket;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;

@ExtendWith(MockitoExtension.class)
@SpringBootTest
class SwaggerConfigTest {
    @Test
    void testSwaggerConfig() {
        SwaggerConfig swaggerConfig = new SwaggerConfig();

        Docket result = swaggerConfig.api();

        assertThat(result).isNotNull();
    }
}
