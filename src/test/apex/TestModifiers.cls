global class TestModifiers {
    //variable modifiers
    //global, protected, private
    //static, final, transient
    String var01 = null;
    global String var02 = null;
    protected String var03 = null;
    private String var04 = null;

    static String var11 = null;
    global static String var12 = null;
    //protected static String var13 = null;
    private static String var14 = null;

    static final String var21 = null;
    global static final String var22 = null;
    //protected static final String var23 = null;
    private static final String var24 = null;

    transient final String var31 = null;
    global transient final String var32 = null;
    protected transient final String var33 = null;
    private transient final String var34 = null;

    //method modifiers
    //global, protected, private
    //virtual, abstract, testmethod, webservice, static, override
    String method01() { return null;}
    global String method02() { return null;}
    //protected String method03() { return null;}
    private String method04() { return null;}

    static String method11() { return null;}
    global static String method12() { return null;}
    //protected static String method13() { return null;}
    private static String method14() { return null;}

    abstract class class01 {
        abstract String method21();
        //global abstract String method22();
        //protected abstract String method23();
        private abstract String method24();
    }

    static webservice String method31() { return null;}
    //global webservice static String method32() { return null;}
    //protected static webservice String method33() { return null;}
    //private static webservice String method34() { return null;}

    virtual String method41() { return null;}
    global virtual String method42() { return null;}
    //protected virtual String method43() { return null;}
    private virtual String method44() { return null;}

    class class02 extends class01 {
        override String method21() { return null;}
        //global override String method22() { return null;}
        //protected override String method23() { return null;}
        private override String method24() { return null;}
    }

    //class modifiers
    //global, public, private
    //abstract
    class class11 {}
    global class class12 {}
    //protected class class13 {}
    public class class14 {}
    
    abstract class class21 {}
    global abstract class class22 {}
    //protected abstract class class23 {}
    public abstract class class24 {}
    
}