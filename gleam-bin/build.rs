fn main() {
    #[cfg(windows)]
    static_vcruntime::metabuild();
}
