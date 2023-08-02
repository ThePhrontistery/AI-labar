package com.capgemini.beni.ailabar.commons.configurations;

import com.capgemini.beni.ailabar.commons.configurations.SQLiteDialect;
import org.junit.jupiter.api.Test;

import java.sql.Types;

import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import static org.junit.jupiter.api.Assertions.*;

@ExtendWith(SpringExtension.class)
@SpringBootTest
class SQLiteDialectTest {
    @Test
    void testSQLiteDialect() {
        SQLiteDialect sqliteDialect = new SQLiteDialect();

        assertEquals("integer", sqliteDialect.getTypeName(Types.BIT));
        assertEquals("tinyint", sqliteDialect.getTypeName(Types.TINYINT));
        assertEquals("smallint", sqliteDialect.getTypeName(Types.SMALLINT));
        assertEquals("integer", sqliteDialect.getTypeName(Types.INTEGER));
        assertEquals("bigint", sqliteDialect.getTypeName(Types.BIGINT));
        assertEquals("float", sqliteDialect.getTypeName(Types.FLOAT));
        assertEquals("real", sqliteDialect.getTypeName(Types.REAL));
        assertEquals("double", sqliteDialect.getTypeName(Types.DOUBLE));
        assertEquals("numeric", sqliteDialect.getTypeName(Types.NUMERIC));
        assertEquals("decimal", sqliteDialect.getTypeName(Types.DECIMAL));
        assertEquals("char", sqliteDialect.getTypeName(Types.CHAR));
        assertEquals("varchar", sqliteDialect.getTypeName(Types.VARCHAR));
        assertEquals("longvarchar", sqliteDialect.getTypeName(Types.LONGVARCHAR));
        assertEquals("date", sqliteDialect.getTypeName(Types.DATE));
        assertEquals("time", sqliteDialect.getTypeName(Types.TIME));
        assertEquals("timestamp", sqliteDialect.getTypeName(Types.TIMESTAMP));
        assertEquals("blob", sqliteDialect.getTypeName(Types.BINARY));
        assertEquals("blob", sqliteDialect.getTypeName(Types.VARBINARY));
        assertEquals("blob", sqliteDialect.getTypeName(Types.LONGVARBINARY));
        assertEquals("blob", sqliteDialect.getTypeName(Types.BLOB));
        assertEquals("clob", sqliteDialect.getTypeName(Types.CLOB));
        assertEquals("integer", sqliteDialect.getTypeName(Types.BOOLEAN));

        assertTrue(sqliteDialect.supportsIdentityColumns());
        assertFalse(sqliteDialect.hasDataTypeInIdentityColumn());
        assertEquals("integer", sqliteDialect.getIdentityColumnString());
        assertEquals("select last_insert_rowid()", sqliteDialect.getIdentitySelectString());
        assertTrue(sqliteDialect.supportsLimit());
        assertEquals("create temporary table if not exists", sqliteDialect.getCreateTemporaryTableString());
        assertFalse(sqliteDialect.dropTemporaryTableAfterUse());
        assertTrue(sqliteDialect.supportsCurrentTimestampSelection());
        assertFalse(sqliteDialect.isCurrentTimestampSelectStringCallable());
        assertEquals("select current_timestamp", sqliteDialect.getCurrentTimestampSelectString());
        assertTrue(sqliteDialect.supportsUnionAll());
        assertFalse(sqliteDialect.hasAlterTable());
        assertFalse(sqliteDialect.dropConstraints());
        assertEquals("add column", sqliteDialect.getAddColumnString());
        assertEquals("", sqliteDialect.getForUpdateString());
        assertFalse(sqliteDialect.supportsOuterJoinForUpdate());
        assertThrows(UnsupportedOperationException.class, sqliteDialect::getDropForeignKeyString);
        assertThrows(UnsupportedOperationException.class, () -> sqliteDialect.getAddForeignKeyConstraintString("", new String[0], "", new String[0], false));
        assertThrows(UnsupportedOperationException.class, () -> sqliteDialect.getAddPrimaryKeyConstraintString(""));
        assertTrue(sqliteDialect.supportsIfExistsBeforeTableName());
        assertFalse(sqliteDialect.supportsCascadeDelete());
    }

    @Test
    void testGetLimitString() {
        SQLiteDialect sqliteDialect = new SQLiteDialect();
        String query = "SELECT * FROM table";
        boolean hasOffset = true;

        String limitString = sqliteDialect.getLimitString(query, hasOffset);

        assertEquals("SELECT * FROM table limit ? offset ?", limitString);
    }

    @Test
    void testSupportsTemporaryTables() {
        SQLiteDialect sqliteDialect = new SQLiteDialect();

        boolean supportsTemporaryTables = sqliteDialect.supportsTemporaryTables();

        assertTrue(supportsTemporaryTables);
    }
}
