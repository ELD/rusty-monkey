use crate::lexer::token::TokenSlice;
use nom::{
    error::{VerboseError, VerboseErrorKind},
    InputIter, Offset,
};
use std::fmt::Write;

pub fn convert_error_tokenslice(input: TokenSlice<'_>, e: VerboseError<TokenSlice<'_>>) -> String {
    let mut result = String::new();

    for (i, (substring, kind)) in e.errors.iter().enumerate() {
        let offset = input.offset(substring);

        if input.slice.is_empty() {
            match kind {
                VerboseErrorKind::Char(c) => {
                    write!(&mut result, "{}: expected '{}', got empty input\n\n", i, c)
                }
                VerboseErrorKind::Context(s) => {
                    write!(&mut result, "{}: in {}, got empty input\n\n", i, s)
                }
                VerboseErrorKind::Nom(e) => {
                    write!(&mut result, "{}: in {:?}, got empty input\n\n", i, e)
                }
            }
        } else {
            let _prefix = &input.slice[..offset];

            match kind {
                VerboseErrorKind::Char(c) => {
                    if let Some(actual) = substring.iter_elements().next() {
                        write!(
                            &mut result,
                            "{i}: expected '{expected}', found {actual:?}\n\n",
                            i = i,
                            expected = c,
                            actual = actual
                        )
                    } else {
                        write!(
                            &mut result,
                            "{i}: expected '{expected}', got end of input\n\n",
                            i = i,
                            expected = c
                        )
                    }
                }
                VerboseErrorKind::Context(s) => {
                    writeln!(&mut result, "{i}: in {context}:", i = i, context = s)
                }
                VerboseErrorKind::Nom(e) => {
                    writeln!(&mut result, "{i}: in {nom_err:?}:", i = i, nom_err = e)
                }
            }
        }
        .unwrap()
    }

    result
}
