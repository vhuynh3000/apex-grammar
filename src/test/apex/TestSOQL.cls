public class TestSOQL {
    public void testStatement() {
        Id varId = null;
        Integer varInteger = null;
        
        List<SObject> var01 = [SELECT Id FROM Account];
        List<SObject> var02 = [SELECT Id, Name FROM Account];
        List<SObject> var03 = [SELECT Parent.Name FROM Account];
        List<SObject> var04 = [SELECT Id FROM Account WHERE Id = ''];
        List<SObject> var05 = [SELECT Id FROM Account WHERE Id = null];
        List<SObject> var06 = [SELECT Id FROM Account WHERE Id = :varId];
        List<SObject> var07 = [SELECT Id FROM Account WHERE NumberOfEmployees = 0];
        List<SObject> var08 = [SELECT Id FROM Account WHERE AnnualRevenue = 99.44];
        List<SObject> var09 = [SELECT Id FROM Account WHERE CreatedDate = 2014-01-01T00:00:00.000Z];
        List<SObject> var10 = [SELECT Id FROM Account WHERE CreatedDate = today];
        List<SObject> var11 = [SELECT Id FROM Opportunity WHERE IsPrivate = true];

        List<SObject> var21 = [SELECT Id FROM Account WHERE NumberOfEmployees != 0];
        List<SObject> var22 = [SELECT Id FROM Account WHERE NumberOfEmployees <> 0];
        List<SObject> var23 = [SELECT Id FROM Account WHERE NumberOfEmployees > 0];
        List<SObject> var24 = [SELECT Id FROM Account WHERE NumberOfEmployees >= 0];
        List<SObject> var25 = [SELECT Id FROM Account WHERE NumberOfEmployees < 0];
        List<SObject> var26 = [SELECT Id FROM Account WHERE NumberOfEmployees <= 0];
        List<SObject> var27 = [SELECT Id FROM Account WHERE NumberOfEmployees IN (0)];
        List<SObject> var28 = [SELECT Id FROM Account WHERE NumberOfEmployees IN (0, 1)];
        List<SObject> var29 = [SELECT Id FROM Account WHERE NumberOfEmployees NOT IN (0)];
        List<SObject> var30 = [SELECT Id FROM Account WHERE NumberOfEmployees NOT IN (0, 1)];
        
        List<SObject> var40 = [SELECT Id FROM Account WHERE Id = '' AND Id = ''];
        List<SObject> var41 = [SELECT Id FROM Account WHERE Id = '' OR Id = ''];
        List<SObject> var42 = [SELECT Id FROM Account WHERE NOT Id = ''];
        List<SObject> var43 = [SELECT Id FROM Account WHERE (Id = '') AND Id = ''];
        List<SObject> var44 = [SELECT Id FROM Account WHERE Id = '' OR (Id = '')];
        List<SObject> var45 = [SELECT Id FROM Account WHERE NOT (Id = '')];
        List<SObject> var46 = [SELECT Id FROM Account WHERE Id = '' AND Id = '' AND Id = ''];
        List<SObject> var47 = [SELECT Id FROM Account WHERE Id = '' OR Id = '' OR Id = ''];
        List<SObject> var48 = [SELECT Id FROM Account WHERE NOT (NOT Id = '')];
        List<SObject> var49 = [SELECT Id FROM Account WHERE (Id = '' AND Id = '') OR Id = ''];
        List<SObject> var50 = [SELECT Id FROM Account WHERE (Id = '' OR Id = '') AND Id = ''];
        List<SObject> var51 = [SELECT Id FROM Account WHERE NOT (Id = '' AND (Id = '' OR Id = ''))];
        
        List<SObject> var60 = [SELECT Id FROM Account LIMIT 1];
        List<SObject> var61 = [SELECT Id FROM Account LIMIT :varInteger];
        List<SObject> var62 = [SELECT Id FROM Account OFFSET 1];
        List<SObject> var63 = [SELECT Id FROM Account OFFSET :varInteger];
        List<SObject> var64 = [SELECT Id FROM Account FOR UPDATE];
        List<SObject> var65 = [SELECT Id FROM KnowledgeArticle UPDATE TRACKING];
        List<SObject> var66 = [SELECT Id FROM KnowledgeArticle UPDATE VIEWSTAT];
        List<SObject> var67 = [SELECT Id FROM Account FOR VIEW];
        List<SObject> var68 = [SELECT Id FROM Account FOR REFERENCE];

        Integer var70 = [SELECT COUNT() FROM Account];
        List<AggregateResult> var71 = [SELECT COUNT(Id) FROM Account];
        List<AggregateResult> var72 = [SELECT COUNT(Id) size FROM Account];
        List<AggregateResult> var73 = [SELECT COUNT(Id) size FROM Account Acct];
        List<AggregateResult> var74 = [SELECT COUNT(Id) size FROM Account AS Acct];
        List<AggregateResult> var75 = [SELECT COUNT(Id), Name FROM Account GROUP BY Name];
        List<AggregateResult> var76 = [SELECT COUNT(Id), Name n FROM Account GROUP BY Name];
        List<AggregateResult> var77 = [SELECT COUNT(Id), Name, ParentId FROM Account GROUP BY Name, ParentId];
        List<AggregateResult> var78 = [SELECT COUNT(Id), Name FROM Account GROUP BY Name HAVING COUNT(Id) = 0];

        List<SObject> var80 = [SELECT Id FROM Account USING SCOPE mine];
        List<SObject> var81 = [SELECT Id FROM UserProfileFeed WITH Id=''];
    }

    public void testOperations() {
        Account var01 = null;
        insert var01;
        insert new List<Account>{var01};
        Database.insert(var01);
        Database.insert(new List<Account>{var01});

        update var01;
        update new List<Account>{var01};
        Database.update(var01);
        Database.update(new List<Account>{var01});

        upsert var01;
        upsert new List<Account>{var01};
        Database.upsert(var01);
        Database.upsert(new List<Account>{var01});

        delete var01;
        delete new List<Account>{var01};
        Database.delete(var01);
        Database.delete(new List<Account>{var01});

        undelete var01;
        undelete new List<Account>{var01};
        Database.undelete(var01);
        Database.undelete(new List<Account>{var01});

    }
}