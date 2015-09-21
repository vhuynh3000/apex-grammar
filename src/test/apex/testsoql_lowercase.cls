public class testsoql_lowercase {
    public void teststatement() {
        id varid = null;
        integer varinteger = null;
        
        list<sobject> var01 = [select id from account];
        list<sobject> var02 = [select id, name from account];
        list<sobject> var03 = [select parent.name from account];
        list<sobject> var04 = [select id from account where id = ''];
        list<sobject> var05 = [select id from account where id = null];
        list<sobject> var06 = [select id from account where id = :varid];
        list<sobject> var07 = [select id from account where numberofemployees = 0];
        list<sobject> var08 = [select id from account where annualrevenue = 99.44];
        list<sobject> var09 = [select id from account where createddate = 2014-01-01t00:00:00.000z];
        list<sobject> var10 = [select id from account where createddate = today];
        list<sobject> var11 = [select id from opportunity where isprivate = true];

        list<sobject> var21 = [select id from account where numberofemployees != 0];
        list<sobject> var22 = [select id from account where numberofemployees <> 0];
        list<sobject> var23 = [select id from account where numberofemployees > 0];
        list<sobject> var24 = [select id from account where numberofemployees >= 0];
        list<sobject> var25 = [select id from account where numberofemployees < 0];
        list<sobject> var26 = [select id from account where numberofemployees <= 0];
        list<sobject> var27 = [select id from account where numberofemployees in (0)];
        list<sobject> var28 = [select id from account where numberofemployees in (0, 1)];
        list<sobject> var29 = [select id from account where numberofemployees not in (0)];
        list<sobject> var30 = [select id from account where numberofemployees not in (0, 1)];
        
        list<sobject> var40 = [select id from account where id = '' and id = ''];
        list<sobject> var41 = [select id from account where id = '' or id = ''];
        list<sobject> var42 = [select id from account where not id = ''];
        list<sobject> var43 = [select id from account where (id = '') and id = ''];
        list<sobject> var44 = [select id from account where id = '' or (id = '')];
        list<sobject> var45 = [select id from account where not (id = '')];
        list<sobject> var46 = [select id from account where id = '' and id = '' and id = ''];
        list<sobject> var47 = [select id from account where id = '' or id = '' or id = ''];
        list<sobject> var48 = [select id from account where not (not id = '')];
        list<sobject> var49 = [select id from account where (id = '' and id = '') or id = ''];
        list<sobject> var50 = [select id from account where (id = '' or id = '') and id = ''];
        list<sobject> var51 = [select id from account where not (id = '' and (id = '' or id = ''))];
        
        list<sobject> var60 = [select id from account limit 1];
        list<sobject> var61 = [select id from account limit :varinteger];
        list<sobject> var62 = [select id from account offset 1];
        list<sobject> var63 = [select id from account offset :varinteger];
        list<sobject> var64 = [select id from account for update];

        integer var70 = [select count() from account];
        list<aggregateresult> var71 = [select count(id) from account];
        list<aggregateresult> var72 = [select count(id) size from account];
        list<aggregateresult> var73 = [select count(id) size from account acct];
        list<aggregateresult> var74 = [select count(id) size from account as acct];
        list<aggregateresult> var75 = [select count(id), name from account group by name];
        list<aggregateresult> var76 = [select count(id), name n from account group by name];
        list<aggregateresult> var77 = [select count(id), name, parentid from account group by name, parentid];
        list<aggregateresult> var78 = [select count(id), name from account group by name having count(id) = 0];
    }

    public void testoperations() {
        account var01 = null;
        insert var01;
        insert new list<account>{var01};
        database.insert(var01);
        database.insert(new list<account>{var01});

        update var01;
        update new list<account>{var01};
        database.update(var01);
        database.update(new list<account>{var01});

        upsert var01;
        upsert new list<account>{var01};
        database.upsert(var01);
        database.upsert(new list<account>{var01});

        delete var01;
        delete new list<account>{var01};
        database.delete(var01);
        database.delete(new list<account>{var01});

        undelete var01;
        undelete new list<account>{var01};
        database.undelete(var01);
        database.undelete(new list<account>{var01});

    }
}
