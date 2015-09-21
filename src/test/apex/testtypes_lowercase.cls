public class testtypes_lowercase {
    //int var01 = 1;
    boolean var02 = true;
    //char var03 = 'a';
    //float var04= 1.1;
    double var05 = 2.2;
    //byte var06 = 1;
    long var07 = 1;
    //short var08 = 1;
    integer var09 = 1;
    decimal var10 = 1.1;
    string var11 = 'a';

    enum enum01 {}
    enum enum02 {one}
    enum enum03 {one, two}

    string[] arr01= null;
    string[] arr02= new string[0];
    string[] arr03= new string[]{};
    string[] arr04= new string[]{''};
    string[] arr05= new string[]{'',''};
    string[] arr06= new string[]{'','',''};
    //string[] arr07= new string[]{'','',,''};
    //string[] arr08= new string[]{'','','',};
    
    list<string> arr11= null;
    list<string> arr12= new string[0];
    list<string> arr13= new string[]{};
    list<string> arr14= new string[]{''};
    list<string> arr15= new string[]{'',''};
    list<string> arr16= new string[]{'','',''};
    //list<string> arr17= new string[]{'','',,''};
    //list<string> arr18= new string[]{'','','',};
    
    list<string> arr21= null;
    list<string> arr22= new list<string>();
    list<string> arr23= new list<string>{};
    list<string> arr24= new list<string>{''};
    list<string> arr25= new list<string>{'',''};
    list<string> arr26= new list<string>{'','',''};
    //list<string> arr27= new list<string>{'','',,''};
    //list<string> arr28= new list<string>{'','','',};
    
    map<string, string> map01= null;
    map<string, string> map02= new map<string, string>();
    map<string, string> map03= new map<string, string>{};
    map<string, string> map04= new map<string, string>{''=>''};
    map<string, string> map05= new map<string, string>{''=>'',''=>''};
    map<string, string> map06= new map<string, string>{''=>'',''=>'',''=>''};
    //map<string, string> map07= new map<string, string>{''=>'',''=>'',,''=>''};
    //map<string, string> map08= new map<string, string>{''=>'',''=>'',''=>'',};

    string prop01 {get;}
    string prop02 {set;}
    string prop03 {get; set;}
    string prop04 {set; get;}
    public string prop05 {public get;}
    public string prop06 {public set;}
    public string prop07 {public get; public set;}
    public string prop08 {public set; public get;}
    
    string prop11 {get {return null;}}
    string prop12 {set {}}
    string prop13 {get {return null;} set {}}
    string prop14 {set; get {return null;}}
    public string prop15 {public get {return null;}}
    public string prop16 {public set;}
    public string prop17 {public get {return null;} public set;}
    public string prop18 {public set; public get {return null;}}
}
