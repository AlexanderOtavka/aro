use std::cmp::min;
use std::fmt::Display;

pub fn sequence_to_str<T: Display>(start: &str, sequence: &Vec<T>, end: &str) -> String {
    let mut string = String::new();

    if sequence.len() >= 1 {
        string += &format!("{}", sequence[0]);

        for element in &sequence[1..] {
            string += &format!(" {}", element);
        }
    }

    format!("{}{}{}", start, string, end)
}

#[derive(Debug)]
pub enum Error {
    LRLocated {
        message: String,
        left_loc: usize,
        right_loc: usize,
    },
    Located {
        message: String,
        loc: usize,
    },
    Unlocated {
        message: String,
    },
}

impl Error {
    fn underlined_at_loc(source: &str, left_loc: usize, right_loc: usize) -> String {
        let line_num = source[0..left_loc].matches("\n").count() + 1;
        let line_start = if let Some(newline_pos) = source[0..left_loc].rfind("\n") {
            newline_pos + 1
        } else {
            0
        };
        let line_end = if let Some(newline_pos) = source[left_loc..].find("\n") {
            newline_pos + left_loc
        } else {
            source.len()
        };

        let line = &source[line_start..line_end];

        let mut string = String::new();

        string += "      |\n";
        string += &format!("{:5} | {}\n", line_num, line);
        string += "      | ";

        for _ in 0..(left_loc - line_start) {
            string += " ";
        }

        for _ in 0..(min(right_loc, line_end + 1) - left_loc) {
            string += "^";
        }

        if right_loc > line_end && right_loc < source.len() {
            string += "...";
        }

        string
    }

    pub fn as_string(&self, source: &str) -> String {
        match self {
            &Error::LRLocated {
                left_loc,
                right_loc,
                ref message,
            } => format!(
                "{}\n{}",
                message,
                Error::underlined_at_loc(source, left_loc, right_loc)
            ),
            &Error::Located { loc, ref message } => format!(
                "{}\n{}",
                message,
                Error::underlined_at_loc(source, loc, loc + 1)
            ),
            &Error::Unlocated { ref message } => format!("{}", message),
        }
    }
}

#[cfg(test)]
mod underlined_at_loc {
    use super::*;

    #[test]
    fn underlines_all_of_a_one_line_source() {
        assert_eq!(
            Error::underlined_at_loc("foo bar", 0, 7),
            "      |\
             \n    1 | foo bar\
             \n      | ^^^^^^^"
        );
    }

    #[test]
    fn underlines_part_of_a_one_line_source() {
        assert_eq!(
            Error::underlined_at_loc("foo bar", 2, 5),
            "      |\
             \n    1 | foo bar\
             \n      |   ^^^"
        );
    }

    #[test]
    fn underlines_a_single_character_of_a_one_line_source() {
        assert_eq!(
            Error::underlined_at_loc("foo bar", 4, 5),
            "      |\
             \n    1 | foo bar\
             \n      |     ^"
        );
    }

    #[test]
    fn underlines_the_end_of_input() {
        assert_eq!(
            Error::underlined_at_loc("foo bar", 7, 8),
            "      |\
             \n    1 | foo bar\
             \n      |        ^"
        );
    }

    #[test]
    fn underlines_part_of_a_multi_line_source() {
        let input = "this is the prev line\
                     \nfoo 0bar1 baz\
                     \nthis is another line\
                     \n";
        assert_eq!(
            Error::underlined_at_loc(
                input,
                input.find("0").unwrap() + 1,
                input.find("1").unwrap()
            ),
            "      |\
             \n    2 | foo 0bar1 baz\
             \n      |      ^^^"
        );
    }

    #[test]
    fn underlines_multiline_part_of_a_multi_line_source() {
        let input = "this is the prev line\
                     \nfoo 0bar baz\
                     \nthis is1 another line\
                     \n";
        assert_eq!(
            Error::underlined_at_loc(
                input,
                input.find("0").unwrap() + 1,
                input.find("1").unwrap()
            ),
            "      |\
             \n    2 | foo 0bar baz\
             \n      |      ^^^^^^^^..."
        );
    }
}
