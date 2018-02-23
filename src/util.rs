use std::cmp::min;

#[derive(Debug)]
pub enum CompilerError {
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

impl CompilerError {
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

        for _ in 0..(min(right_loc, line_end) - left_loc) {
            string += "^";
        }

        if right_loc > line_end {
            string += "...";
        }

        string += "\n";

        string
    }

    pub fn as_string(&self, source: &str) -> String {
        match self {
            &CompilerError::LRLocated {
                left_loc,
                right_loc,
                ref message,
            } => format!(
                "{}\n{}",
                message,
                CompilerError::underlined_at_loc(source, left_loc, right_loc)
            ),
            &CompilerError::Located { loc, ref message } => format!(
                "{}\n{}",
                message,
                CompilerError::underlined_at_loc(source, loc, loc + 1)
            ),
            &CompilerError::Unlocated { ref message } => format!("{}\n", message),
        }
    }
}

#[cfg(test)]
mod underlined_at_loc {
    use super::*;

    #[test]
    fn underlines_all_of_a_one_line_source() {
        assert_eq!(
            CompilerError::underlined_at_loc("foo bar", 0, 7),
            "      |\
             \n    1 | foo bar\
             \n      | ^^^^^^^\
             \n"
        );
    }

    #[test]
    fn underlines_part_of_a_one_line_source() {
        assert_eq!(
            CompilerError::underlined_at_loc("foo bar", 2, 5),
            "      |\
             \n    1 | foo bar\
             \n      |   ^^^\
             \n"
        );
    }

    #[test]
    fn underlines_a_single_character_of_a_one_line_source() {
        assert_eq!(
            CompilerError::underlined_at_loc("foo bar", 6, 7),
            "      |\
             \n    1 | foo bar\
             \n      |       ^\
             \n"
        );
    }

    #[test]
    fn underlines_part_of_a_multi_line_source() {
        let input = "this is the prev line\
                     \nfoo 0bar1 baz\
                     \nthis is another line\
                     \n";
        assert_eq!(
            CompilerError::underlined_at_loc(
                input,
                input.find("0").unwrap() + 1,
                input.find("1").unwrap()
            ),
            "      |\
             \n    2 | foo 0bar1 baz\
             \n      |      ^^^\
             \n"
        );
    }

    #[test]
    fn underlines_multiline_part_of_a_multi_line_source() {
        let input = "this is the prev line\
                     \nfoo 0bar baz\
                     \nthis is1 another line\
                     \n";
        assert_eq!(
            CompilerError::underlined_at_loc(
                input,
                input.find("0").unwrap() + 1,
                input.find("1").unwrap()
            ),
            "      |\
             \n    2 | foo 0bar baz\
             \n      |      ^^^^^^^...\
             \n"
        );
    }
}
