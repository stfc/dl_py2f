extern "C" {
    F2PY* Foo_new(){ return new F2PY(); }
    Foo_bar(Foo* foo){ foo->parse_dlso(); }
}
