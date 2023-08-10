package com.capgemini.ailabar.commons.configurations;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import springfox.documentation.spring.web.plugins.Docket;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;

@ExtendWith(SpringExtension.class)
@SpringBootTest
class SwaggerConfigTest {
    @Test
    void testSwaggerConfig() {
        SwaggerConfiguration swaggerConfig = new SwaggerConfiguration();

        Docket result = swaggerConfig.api();

        assertThat(result).isNotNull();
    }
}
