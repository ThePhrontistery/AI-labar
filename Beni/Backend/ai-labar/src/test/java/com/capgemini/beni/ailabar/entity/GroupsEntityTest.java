package com.capgemini.beni.ailabar.entity;

import com.capgemini.beni.ailabar.dto.GroupsDto;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import static org.junit.jupiter.api.Assertions.assertEquals;

@ExtendWith(SpringExtension.class)
@SpringBootTest
class GroupsEntityTest {
    @Test
    void testIdProperty() {
        GroupsEntity entity = new GroupsEntity();
        entity.setId(1);
        assertEquals(1, entity.getId());
    }

    @Test
    void testGroupNameProperty() {
        GroupsEntity entity = new GroupsEntity();
        entity.setGroupName("Group 1");
        assertEquals("Group 1", entity.getGroupName());
    }

    @Test
    void testMembersProperty() {
        GroupsEntity entity = new GroupsEntity();
        entity.setMembers("[member1, member2]");
        assertEquals("[member1, member2]", entity.getMembers());
    }

    @Test
    void testAdminProperty() {
        GroupsEntity entity = new GroupsEntity();
        entity.setAdmin("admin");
        assertEquals("admin", entity.getAdmin());
    }

    @Test
    void testConstructorWithGroupsDto() {
        GroupsDto dto = new GroupsDto();
        dto.setGroupName("Group 1");
        dto.getMembers().add("member1");
        dto.getMembers().add("member2");
        dto.setAdmin("admin");

        GroupsEntity entity = new GroupsEntity(dto);

        assertEquals("Group 1", entity.getGroupName());
        assertEquals("[member1, member2]", entity.getMembers());
        assertEquals("admin", entity.getAdmin());
    }
}

