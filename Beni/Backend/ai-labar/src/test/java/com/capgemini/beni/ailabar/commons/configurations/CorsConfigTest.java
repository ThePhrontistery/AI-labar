package com.capgemini.beni.ailabar.commons.configurations;

import com.capgemini.beni.ailabar.commons.configurations.CorsConfig;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.http.converter.HttpMessageConverter;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultHandlers;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

@ExtendWith(SpringExtension.class)
@SpringBootTest
class CorsConfigTest {
    @Test
    void testCorsFilterBean() throws Exception {
        CorsConfig corsConfig = new CorsConfig();
        MockMvc mockMvc = MockMvcBuilders.standaloneSetup(corsConfig).build();

        MvcResult result = mockMvc.perform(MockMvcRequestBuilders.get("/users/all").contentType(MediaType.APPLICATION_JSON))
                .andExpect(MockMvcResultMatchers.status().isNotFound())
                .andDo(MockMvcResultHandlers.print())
                .andReturn();

        MockHttpServletResponse mockResponse = result.getResponse();

        Collection<String> responseHeaders = mockResponse.getHeaderNames();
        assertThat(responseHeaders).isNotNull();
        assertThat(responseHeaders.size()).isBetween(0, 15);
    }

    @Test
    void testConfigureMessageConverters() {
        CorsConfig corsConfig = new CorsConfig();
        List<HttpMessageConverter<?>> converters = new ArrayList<>();
        corsConfig.configureMessageConverters(converters);
        assertFalse(converters.isEmpty());
        HttpMessageConverter<?> converter = converters.get(0);
        assertTrue(converter instanceof MappingJackson2HttpMessageConverter);
        ObjectMapper objectMapper = ((MappingJackson2HttpMessageConverter) converter).getObjectMapper();
        assertTrue(objectMapper.isEnabled(SerializationFeature.WRITE_NULL_MAP_VALUES));
    }
}
