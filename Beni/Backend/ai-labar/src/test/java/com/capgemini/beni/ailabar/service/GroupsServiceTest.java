package com.capgemini.beni.ailabar.service;

import com.capgemini.beni.ailabar.entity.GroupsEntity;
import com.capgemini.beni.ailabar.repository.GroupsRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(SpringExtension.class)
@SpringBootTest
class GroupsServiceTest {
    @Mock
    private GroupsRepository groupsRepository;

    @InjectMocks
    private GroupsService groupsService;

    @BeforeEach
    void setUp() {
        Mockito.reset(groupsRepository);
    }

    @Test
    void testExistsByGroupNameAndAdmin() {
        String groupName = "Group A";
        String admin = "John";
        boolean expectedResult = true;

        when(groupsRepository.existsByGroupNameAndAdmin(groupName, admin)).thenReturn(expectedResult);

        boolean result = groupsService.existsByGroupNameAndAdmin(groupName, admin);

        assertEquals(expectedResult, result);
        verify(groupsRepository, times(1)).existsByGroupNameAndAdmin(groupName, admin);
        verifyNoMoreInteractions(groupsRepository);
    }

    @Test
    void testSaveGroup() {
        GroupsEntity groupEntity = new GroupsEntity();

        groupsService.saveGroup(groupEntity);

        verify(groupsRepository, times(1)).save(groupEntity);
        verifyNoMoreInteractions(groupsRepository);
    }

    @Test
    void testGetGroup() {
        String groupName = "Group A";
        String admin = "John";
        GroupsEntity expectedGroup = new GroupsEntity();

        when(groupsRepository.findByGroupNameAndAdmin(groupName, admin)).thenReturn(expectedGroup);

        GroupsEntity result = groupsService.getGroup(groupName, admin);

        assertEquals(expectedGroup, result);
        verify(groupsRepository, times(1)).findByGroupNameAndAdmin(groupName, admin);
        verifyNoMoreInteractions(groupsRepository);
    }

    @Test
    void testGetGroupForEdit() {
        String admin = "John";
        List<String> expectedGroupNames = Arrays.asList("Group A", "Group B");

        when(groupsRepository.findAllGroupNamesByAdmin(admin)).thenReturn(expectedGroupNames);

        List<String> result = groupsService.getGroupForEdit(admin);

        assertEquals(expectedGroupNames, result);
        verify(groupsRepository, times(1)).findAllGroupNamesByAdmin(admin);
        verifyNoMoreInteractions(groupsRepository);
    }

    @Test
    void testDeleteGroup() {
        String groupName = "Group A";
        String admin = "John";

        groupsService.deleteGroup(groupName, admin);

        verify(groupsRepository, times(1)).deleteByGroupNameAndAdmin(groupName, admin);
        verifyNoMoreInteractions(groupsRepository);
    }

    @Test
    void testFindGroupsEntityById() {
        Integer id = 1;
        GroupsEntity expectedGroup = new GroupsEntity();

        when(groupsRepository.findGroupsEntityById(id)).thenReturn(expectedGroup);

        GroupsEntity result = groupsService.findGroupsEntityById(id);

        assertEquals(expectedGroup, result);
        verify(groupsRepository, times(1)).findGroupsEntityById(id);
        verifyNoMoreInteractions(groupsRepository);
    }

    @Test
    void testGetAllGroupsData() {
        GroupsEntity groupEntity1 = new GroupsEntity();
        groupEntity1.setId(1);
        groupEntity1.setGroupName("Group 1");
        groupEntity1.setAdmin("admin1");

        GroupsEntity groupEntity2 = new GroupsEntity();
        groupEntity2.setId(2);
        groupEntity2.setGroupName("Group 2");
        groupEntity2.setAdmin("admin2");

        GroupsEntity groupEntity3 = new GroupsEntity();
        groupEntity3.setId(3);
        groupEntity3.setGroupName("Group 3");
        groupEntity3.setAdmin("admin3");

        List<GroupsEntity> expectedEntities = Arrays.asList(groupEntity1, groupEntity2, groupEntity3);

        when(groupsRepository.findAll()).thenReturn(expectedEntities);

        List<GroupsEntity> result = groupsService.getAllGroupsData();

        assertEquals(expectedEntities, result);
        verify(groupsRepository, times(1)).findAll();
        verifyNoMoreInteractions(groupsRepository);
    }

}

