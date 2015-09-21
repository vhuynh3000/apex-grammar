global class testmodifiers_lowercase {
    //variable modifiers
    //global, protected, private
    //static, final, transient
    string var01 = null;
    global string var02 = null;
    protected string var03 = null;
    private string var04 = null;

    static string var11 = null;
    global static string var12 = null;
    //protected static string var13 = null;
    private static string var14 = null;

    static final string var21 = null;
    global static final string var22 = null;
    //protected static final string var23 = null;
    private static final string var24 = null;

    transient final string var31 = null;
    global transient final string var32 = null;
    protected transient final string var33 = null;
    private transient final string var34 = null;

    //method modifiers
    //global, protected, private
    //virtual, abstract, testmethod, webservice, static, override
    string method01() { return null;}
    global string method02() { return null;}
    //protected string method03() { return null;}
    private string method04() { return null;}

    static string method11() { return null;}
    global static string method12() { return null;}
    //protected static string method13() { return null;}
    private static string method14() { return null;}

    abstract class class01 {
        abstract string method21();
        //global abstract string method22();
        //protected abstract string method23();
        private abstract string method24();
    }

    static webservice string method31() { return null;}
    //global webservice static string method32() { return null;}
    //protected static webservice string method33() { return null;}
    //private static webservice string method34() { return null;}

    virtual string method41() { return null;}
    global virtual string method42() { return null;}
    //protected virtual string method43() { return null;}
    private virtual string method44() { return null;}

    class class02 extends class01 {
        override string method21() { return null;}
        //global override string method22() { return null;}
        //protected override string method23() { return null;}
        private override string method24() { return null;}
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