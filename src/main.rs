// Citations:
//  https://doc.rust-lang.org
//  https://stackoverflow.com/questions/34969902/how-to-write-a-rust-function-that-takes-an-iterator
//  https://stackoverflow.com/questions/28370126/how-can-i-test-stdin-and-stdout

#![cfg_attr(feature = "ci", deny(warnings))]

extern crate clap;
mod lex;
mod parse;
mod eval;

use std::io::{self, Write};

fn write_output<'a, W, A>(mut output: W, show_length: bool, args: A)
where
    W: Write,
    A: IntoIterator<Item = &'a str>,
{
    for arg in args.into_iter() {
        if show_length {
            write!(&mut output, "{}\n", arg.len()).unwrap();
        } else {
            write!(&mut output, "{}\n", arg).unwrap();
        }
    }
}

#[cfg(test)]
mod test_write_output {
    use super::*;

    #[test]
    fn it_repeats_the_args_back() {
        let mut output = Vec::new();
        write_output(&mut output, false, vec!["foo", "bar"]);

        assert_eq!("foo\nbar\n", String::from_utf8(output).unwrap());
    }

    #[test]
    fn it_sends_back_the_arg_lengths() {
        let mut output = Vec::new();
        write_output(&mut output, true, vec!["foo", "bazing"]);

        assert_eq!("3\n6\n", String::from_utf8(output).unwrap());
    }
}

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

    write_output(
        io::stdout(),
        options.is_present("length"),
        options.values_of("args").unwrap_or_default(),
    );
}
