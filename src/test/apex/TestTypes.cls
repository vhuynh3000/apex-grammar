public class TestTypes {
    //int var01 = 1;
    boolean var02 = true;
    //char var03 = 'a';
    //float var04= 1.1;
    double var05 = 2.2;
    //byte var06 = 1;
    long var07 = 1;
    //short var08 = 1;
    Integer var09 = 1;
    Decimal var10 = 1.1;
    String var11 = 'a';

    enum enum01 {}
    enum enum02 {one}
    enum enum03 {one, two}

    String[] arr01= null;
    String[] arr02= new String[0];
    String[] arr03= new String[]{};
    String[] arr04= new String[]{''};
    String[] arr05= new String[]{'',''};
    String[] arr06= new String[]{'','',''};
    //String[] arr07= new String[]{'','',,''};
    //String[] arr08= new String[]{'','','',};
    
    List<String> arr11= null;
    List<String> arr12= new String[0];
    List<String> arr13= new String[]{};
    List<String> arr14= new String[]{''};
    List<String> arr15= new String[]{'',''};
    List<String> arr16= new String[]{'','',''};
    //List<String> arr17= new String[]{'','',,''};
    //List<String> arr18= new String[]{'','','',};
    
    List<String> arr21= null;
    List<String> arr22= new List<String>();
    List<String> arr23= new List<String>{};
    List<String> arr24= new List<String>{''};
    List<String> arr25= new List<String>{'',''};
    List<String> arr26= new List<String>{'','',''};
    //List<String> arr27= new List<String>{'','',,''};
    //List<String> arr28= new List<String>{'','','',};
    
    Map<String, String> map01= null;
    Map<String, String> map02= new Map<String, String>();
    Map<String, String> map03= new Map<String, String>{};
    Map<String, String> map04= new Map<String, String>{''=>''};
    Map<String, String> map05= new Map<String, String>{''=>'',''=>''};
    Map<String, String> map06= new Map<String, String>{''=>'',''=>'',''=>''};
    //Map<String, String> map07= new Map<String, String>{''=>'',''=>'',,''=>''};
    //Map<String, String> map08= new Map<String, String>{''=>'',''=>'',''=>'',};

    String prop01 {get;}
    String prop02 {set;}
    String prop03 {get; set;}
    String prop04 {set; get;}
    public String prop05 {public get;}
    public String prop06 {public set;}
    public String prop07 {public get; public set;}
    public String prop08 {public set; public get;}
    
    String prop11 {get {return null;}}
    String prop12 {set {}}
    String prop13 {get {return null;} set {}}
    String prop14 {set; get {return null;}}
    public String prop15 {public get {return null;}}
    public String prop16 {public set;}
    public String prop17 {public get {return null;} public set;}
    public String prop18 {public set; public get {return null;}}
}
