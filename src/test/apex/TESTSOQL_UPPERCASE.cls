PUBLIC CLASS TESTSOQL_UPPERCASE {
    PUBLIC VOID TESTSTATEMENT() {
        ID VARID = NULL;
        INTEGER VARINTEGER = NULL;
        
        LIST<SOBJECT> VAR01 = [SELECT ID FROM ACCOUNT];
        LIST<SOBJECT> VAR02 = [SELECT ID, NAME FROM ACCOUNT];
        LIST<SOBJECT> VAR03 = [SELECT PARENT.NAME FROM ACCOUNT];
        LIST<SOBJECT> VAR04 = [SELECT ID FROM ACCOUNT WHERE ID = ''];
        LIST<SOBJECT> VAR05 = [SELECT ID FROM ACCOUNT WHERE ID = NULL];
        LIST<SOBJECT> VAR06 = [SELECT ID FROM ACCOUNT WHERE ID = :VARID];
        LIST<SOBJECT> VAR07 = [SELECT ID FROM ACCOUNT WHERE NUMBEROFEMPLOYEES = 0];
        LIST<SOBJECT> VAR08 = [SELECT ID FROM ACCOUNT WHERE ANNUALREVENUE = 99.44];
        LIST<SOBJECT> VAR09 = [SELECT ID FROM ACCOUNT WHERE CREATEDDATE = 2014-01-01T00:00:00.000Z];
        LIST<SOBJECT> VAR10 = [SELECT ID FROM ACCOUNT WHERE CREATEDDATE = TODAY];
        LIST<SOBJECT> VAR11 = [SELECT ID FROM OPPORTUNITY WHERE ISPRIVATE = TRUE];

        LIST<SOBJECT> VAR21 = [SELECT ID FROM ACCOUNT WHERE NUMBEROFEMPLOYEES != 0];
        LIST<SOBJECT> VAR22 = [SELECT ID FROM ACCOUNT WHERE NUMBEROFEMPLOYEES <> 0];
        LIST<SOBJECT> VAR23 = [SELECT ID FROM ACCOUNT WHERE NUMBEROFEMPLOYEES > 0];
        LIST<SOBJECT> VAR24 = [SELECT ID FROM ACCOUNT WHERE NUMBEROFEMPLOYEES >= 0];
        LIST<SOBJECT> VAR25 = [SELECT ID FROM ACCOUNT WHERE NUMBEROFEMPLOYEES < 0];
        LIST<SOBJECT> VAR26 = [SELECT ID FROM ACCOUNT WHERE NUMBEROFEMPLOYEES <= 0];
        LIST<SOBJECT> VAR27 = [SELECT ID FROM ACCOUNT WHERE NUMBEROFEMPLOYEES IN (0)];
        LIST<SOBJECT> VAR28 = [SELECT ID FROM ACCOUNT WHERE NUMBEROFEMPLOYEES IN (0, 1)];
        LIST<SOBJECT> VAR29 = [SELECT ID FROM ACCOUNT WHERE NUMBEROFEMPLOYEES NOT IN (0)];
        LIST<SOBJECT> VAR30 = [SELECT ID FROM ACCOUNT WHERE NUMBEROFEMPLOYEES NOT IN (0, 1)];
        
        LIST<SOBJECT> VAR40 = [SELECT ID FROM ACCOUNT WHERE ID = '' AND ID = ''];
        LIST<SOBJECT> VAR41 = [SELECT ID FROM ACCOUNT WHERE ID = '' OR ID = ''];
        LIST<SOBJECT> VAR42 = [SELECT ID FROM ACCOUNT WHERE NOT ID = ''];
        LIST<SOBJECT> VAR43 = [SELECT ID FROM ACCOUNT WHERE (ID = '') AND ID = ''];
        LIST<SOBJECT> VAR44 = [SELECT ID FROM ACCOUNT WHERE ID = '' OR (ID = '')];
        LIST<SOBJECT> VAR45 = [SELECT ID FROM ACCOUNT WHERE NOT (ID = '')];
        LIST<SOBJECT> VAR46 = [SELECT ID FROM ACCOUNT WHERE ID = '' AND ID = '' AND ID = ''];
        LIST<SOBJECT> VAR47 = [SELECT ID FROM ACCOUNT WHERE ID = '' OR ID = '' OR ID = ''];
        LIST<SOBJECT> VAR48 = [SELECT ID FROM ACCOUNT WHERE NOT (NOT ID = '')];
        LIST<SOBJECT> VAR49 = [SELECT ID FROM ACCOUNT WHERE (ID = '' AND ID = '') OR ID = ''];
        LIST<SOBJECT> VAR50 = [SELECT ID FROM ACCOUNT WHERE (ID = '' OR ID = '') AND ID = ''];
        LIST<SOBJECT> VAR51 = [SELECT ID FROM ACCOUNT WHERE NOT (ID = '' AND (ID = '' OR ID = ''))];
        
        LIST<SOBJECT> VAR60 = [SELECT ID FROM ACCOUNT LIMIT 1];
        LIST<SOBJECT> VAR61 = [SELECT ID FROM ACCOUNT LIMIT :VARINTEGER];
        LIST<SOBJECT> VAR62 = [SELECT ID FROM ACCOUNT OFFSET 1];
        LIST<SOBJECT> VAR63 = [SELECT ID FROM ACCOUNT OFFSET :VARINTEGER];
        LIST<SOBJECT> VAR64 = [SELECT ID FROM ACCOUNT FOR UPDATE];

        INTEGER VAR70 = [SELECT COUNT() FROM ACCOUNT];
        LIST<AGGREGATERESULT> VAR71 = [SELECT COUNT(ID) FROM ACCOUNT];
        LIST<AGGREGATERESULT> VAR72 = [SELECT COUNT(ID) SIZE FROM ACCOUNT];
        LIST<AGGREGATERESULT> VAR73 = [SELECT COUNT(ID) SIZE FROM ACCOUNT ACCT];
        LIST<AGGREGATERESULT> VAR74 = [SELECT COUNT(ID) SIZE FROM ACCOUNT AS ACCT];
        LIST<AGGREGATERESULT> VAR75 = [SELECT COUNT(ID), NAME FROM ACCOUNT GROUP BY NAME];
        LIST<AGGREGATERESULT> VAR76 = [SELECT COUNT(ID), NAME N FROM ACCOUNT GROUP BY NAME];
        LIST<AGGREGATERESULT> VAR77 = [SELECT COUNT(ID), NAME, PARENTID FROM ACCOUNT GROUP BY NAME, PARENTID];
        LIST<AGGREGATERESULT> VAR78 = [SELECT COUNT(ID), NAME FROM ACCOUNT GROUP BY NAME HAVING COUNT(ID) = 0];
    }

    PUBLIC VOID TESTOPERATIONS() {
        ACCOUNT VAR01 = NULL;
        INSERT VAR01;
        INSERT NEW LIST<ACCOUNT>{VAR01};
        DATABASE.INSERT(VAR01);
        DATABASE.INSERT(NEW LIST<ACCOUNT>{VAR01});

        UPDATE VAR01;
        UPDATE NEW LIST<ACCOUNT>{VAR01};
        DATABASE.UPDATE(VAR01);
        DATABASE.UPDATE(NEW LIST<ACCOUNT>{VAR01});

        UPSERT VAR01;
        UPSERT NEW LIST<ACCOUNT>{VAR01};
        DATABASE.UPSERT(VAR01);
        DATABASE.UPSERT(NEW LIST<ACCOUNT>{VAR01});

        DELETE VAR01;
        DELETE NEW LIST<ACCOUNT>{VAR01};
        DATABASE.DELETE(VAR01);
        DATABASE.DELETE(NEW LIST<ACCOUNT>{VAR01});

        UNDELETE VAR01;
        UNDELETE NEW LIST<ACCOUNT>{VAR01};
        DATABASE.UNDELETE(VAR01);
        DATABASE.UNDELETE(NEW LIST<ACCOUNT>{VAR01});

    }
}