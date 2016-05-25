PUBLIC CLASS TESTCONTROL_UPPERCASE {
    PUBLIC VOID METHOD() {
        STRING VAR01 = '';
        IF (VAR01 == '') {
        }
        
        IF (VAR01 == '') {
        } ELSE {
        }
        
        FOR (STRING VAR02 : NEW STRING[] {}) {
        }
        
        FOR (;;) {
        }

        FOR (INTEGER VAR03 = 0;;) {
        }

        FOR (; VAR01 == '';) {
        }
        
        FOR (;; VAR01 = '') {
        }
       
        WHILE (VAR01 == '') SYSTEM.DEBUG('DO NOTHING');
        WHILE (VAR01 == '') {
            SYSTEM.DEBUG('DO NOTHING');
        }
        
        DO {} WHILE (VAR01 == '');
        DO {
            SYSTEM.DEBUG('DO NOTHING');
        } 
        WHILE (VAR01 == '');
        
        TRY {
            SYSTEM.DEBUG('DO NOTHING');
        } CATCH (EXCEPTION E) {
            THROW E;
        }
        
        FOR (;;) {
           FOR (;;) {
               BREAK;
               CONTINUE;
           }
        }
    }
}
