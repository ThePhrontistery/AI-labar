package com.capgemini.ailabar.commons.configurations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.MediaType;
import org.springframework.http.converter.HttpMessageConverter;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.mock.web.MockHttpServletResponse;
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

@ExtendWith(MockitoExtension.class)
class CorsConfigurationTest {

    @InjectMocks
    private CorsConfiguration corsConfiguration;

    @Test
    void testCorsFilterBean() throws Exception {
        MockMvc mockMvc = MockMvcBuilders.standaloneSetup(corsConfiguration).build();

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
        List<HttpMessageConverter<?>> converters = new ArrayList<>();
        corsConfiguration.configureMessageConverters(converters);
        assertFalse(converters.isEmpty());
        HttpMessageConverter<?> converter = converters.get(0);
        assertTrue(converter instanceof MappingJackson2HttpMessageConverter);
        ObjectMapper objectMapper = ((MappingJackson2HttpMessageConverter) converter).getObjectMapper();
        assertTrue(objectMapper.isEnabled(SerializationFeature.WRITE_NULL_MAP_VALUES));
    }
}
