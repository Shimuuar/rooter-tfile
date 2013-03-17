
void test () {
    TFile f("tst.root", "RECREATE");
    f.cd();
   
    TNamed nm("ZZZZ","XXXX");
    nm.Write("named");

    TNamed nm2("ZAAZZZ","XXAAXX");
    nm2.Write("named2");

    TNamed nm3("ZAAZZZ","XXAAXX");
    nm3.Write("named3");
    std::cout << nm3.GetUniqueID() << std::endl;
}
