package com.capgemini.beni.ailabar;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.context.SpringBootTest;

@ExtendWith(MockitoExtension.class)
@SpringBootTest
class AiLabarApplicationTest {
	@Test
	void testMainMethod() {
		AiLabarApplication.main(new String[]{});
		Assertions.assertTrue(true);
	}
}
