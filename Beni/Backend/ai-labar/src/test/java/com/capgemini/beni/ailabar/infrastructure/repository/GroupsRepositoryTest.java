package com.capgemini.beni.ailabar.infrastructure.repository;

import com.capgemini.beni.ailabar.infrastructure.entity.GroupsEntity;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.*;

@ExtendWith(SpringExtension.class)
@SpringBootTest
class GroupsRepositoryTest {
    @Mock
    private GroupsRepository groupsRepository;

    @BeforeEach
    void setUp() {
        Mockito.reset(groupsRepository);
    }

    @Test
    void testFindByGroupNameAndAdmin() {
        String groupName = "groupName";
        String admin = "admin";
        GroupsEntity expectedEntity = new GroupsEntity();

        when(groupsRepository.findByGroupNameAndAdmin(groupName, admin)).thenReturn(expectedEntity);

        GroupsEntity result = groupsRepository.findByGroupNameAndAdmin(groupName, admin);

        assertEquals(expectedEntity, result);
        verify(groupsRepository, times(1)).findByGroupNameAndAdmin(groupName, admin);
        verifyNoMoreInteractions(groupsRepository);
    }

    @Test
    void testFindAllGroupNamesByAdmin() {
        String admin = "admin";
        List<String> expectedGroupNames = Arrays.asList("Group1", "Group2");

        when(groupsRepository.findAllGroupNamesByAdmin(admin)).thenReturn(expectedGroupNames);

        List<String> result = groupsRepository.findAllGroupNamesByAdmin(admin);

        assertEquals(expectedGroupNames, result);
        verify(groupsRepository, times(1)).findAllGroupNamesByAdmin(admin);
        verifyNoMoreInteractions(groupsRepository);
    }

    @Test
    void testExistsByGroupNameAndAdmin() {
        String groupName = "groupName";
        String admin = "admin";
        Boolean expectedExists = true;

        when(groupsRepository.existsByGroupNameAndAdmin(groupName, admin)).thenReturn(expectedExists);

        Boolean result = groupsRepository.existsByGroupNameAndAdmin(groupName, admin);

        assertEquals(expectedExists, result);
        verify(groupsRepository, times(1)).existsByGroupNameAndAdmin(groupName, admin);
        verifyNoMoreInteractions(groupsRepository);
    }

    @Test
    void testFindGroupsEntityById() {
        String id = "1";
        GroupsEntity expectedEntity = new GroupsEntity();

        when(groupsRepository.findById(id)).thenReturn(Optional.of(expectedEntity));

        Optional<GroupsEntity> result = groupsRepository.findById(id);

        assertTrue(result.isPresent());
        assertEquals(expectedEntity, result.get());
        verify(groupsRepository, times(1)).findById(id);
        verifyNoMoreInteractions(groupsRepository);
    }

    @Test
    void testDeleteByGroupNameAndAdmin() {
        String groupName = "group1";
        String admin = "admin1";

        groupsRepository.deleteByGroupNameAndAdmin(groupName, admin);

        verify(groupsRepository, times(1)).deleteByGroupNameAndAdmin(groupName, admin);
        verifyNoMoreInteractions(groupsRepository);
    }
}
