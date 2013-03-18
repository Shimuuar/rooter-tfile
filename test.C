
void test () {
    TFile f("tst.root", "RECREATE");
    f.cd();
   
    TH1I h("HIST","hist", 10,0, 10);
    for( int i = 0; i < 10; i++) {
        h.Fill( i+0.1, 256 + i );
    }
    h.Write("hist");
}
