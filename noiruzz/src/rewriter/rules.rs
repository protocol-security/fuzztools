use super::parser::{
    MatchFunction, MatchParser, Parser, RewriteFunction, RewriteParser, RewriteUtil,
};
use crate::{config::RewriteRule as ConfigRewriteRule, nodes::Expression};
use std::collections::HashMap;

pub struct RewriteRule {
    pub name: String,
    pub match_pattern: String,
    pub rewrite_pattern: String,
    match_func: MatchFunction,
    rewrite_func: RewriteFunction,
}

impl RewriteRule {
    pub fn new(
        name: String,
        match_pattern: String,
        rewrite_pattern: String,
    ) -> Result<Self, String> {
        let match_parser = MatchParser::new();
        let rewrite_parser = RewriteParser::new();

        let match_func = match_parser.parse(&match_pattern)?;
        let rewrite_func = rewrite_parser.parse(&rewrite_pattern)?;

        Ok(Self { name, match_pattern, rewrite_pattern, match_func, rewrite_func })
    }

    pub fn from_config(config: &ConfigRewriteRule) -> Result<Self, String> {
        Self::new(config.name.clone(), config.match_pattern.clone(), config.rewrite.clone())
    }

    pub fn is_applicable(&self, node: &Expression) -> bool {
        let mut lookup = HashMap::new();
        (self.match_func)(&mut lookup, node)
    }

    pub fn rewrite(&self, node: &Expression, rewrite_util: &mut RewriteUtil) -> Option<Expression> {
        let mut lookup = HashMap::new();
        if (self.match_func)(&mut lookup, node) {
            let rewritten = (self.rewrite_func)(&mut lookup, rewrite_util);
            Some(rewritten)
        } else {
            None
        }
    }
}

impl PartialEq for RewriteRule {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
            && self.match_pattern == other.match_pattern
            && self.rewrite_pattern == other.rewrite_pattern
    }
}

impl Eq for RewriteRule {}

impl std::hash::Hash for RewriteRule {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}
