#![cfg_attr(test, deny(warnings))]

extern crate clap;

fn main() {
    let options = clap::App::new("The aro-> Compiler")
        .version("0.1.0")
        .author("Zander Otavka <otavkaal@grinnell.edu>")
        .arg(
            clap::Arg::with_name("length")
                .help("Print out the lengths of each arg.")
                .long("length"),
        )
        .arg(
            clap::Arg::with_name("args")
                .help("A mysterious list of arguments to be printed back.")
                .multiple(true),
        )
        .get_matches();

    for arg in options.values_of("args").unwrap_or_default() {
        if options.is_present("length") {
            println!("{}", String::from(arg).len())
        } else {
            println!("{}", arg);
        }
    }
}
