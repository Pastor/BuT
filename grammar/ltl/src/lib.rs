extern crate pest;
#[macro_use]
extern crate pest_derive;

pub use pest::Parser;

#[derive(Parser)]
#[grammar = "ltl.pest"]
pub struct LtlParser;

#[cfg(test)]
mod tests {
    use crate::*;

    const LTL: &'static str = "(is(x1) W was(z1)) & [](was(z2) -> (is(x1) U was(z1)) & was(z1) -> (!is(x1) R was(z2)))";

    #[test]
    fn test_parse_ltl() {
        let pairs = LtlParser::parse(Rule::grammar, LTL).unwrap_or_else(|e| panic!("{}", e));
        for pair in pairs {
            println!("Rule:    {:?}", pair.as_rule());
            println!("Span:    {:?}", pair.as_span());
            println!("Text:    {}", pair.as_str());
            //
            for inner_pair in pair.into_inner() {
                match inner_pair.as_rule() {
                    Rule::Name => println!("Name:  {}", inner_pair.as_str()),
                    _ => println!("{:?}: {}", inner_pair.as_rule(), inner_pair.as_str())
                };
            }
        }
    }
}