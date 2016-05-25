public class TestControl {
    public void method() {
        String var01 = '';
        if (var01 == '') {
        }
        
        if (var01 == '') {
        } else {
        }
        
        for (String var02 : new String[] {}) {
        }
        
        for (;;) {
        }

        for (Integer var03 = 0;;) {
        }

        for (; var01 == '';) {
        }
        
        for (;; var01 = '') {
        }
       
        while (var01 == '') System.debug('do nothing');
        while (var01 == '') {
            System.debug('do nothing');
        }
        
        do {} while (var01 == '');
        do {
            System.debug('do nothing');
        } 
        while (var01 == '');
        
        try {
            System.debug('do nothing');
        } catch (Exception e) {
            throw e;
        }
        
        for (;;) {
           for (;;) {
               break;
               continue;
           }
        }
    }
}
