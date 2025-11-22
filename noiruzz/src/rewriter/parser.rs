use super::tokenizer::{Token, TokenKind, TokenValue, Tokenizer};
use crate::{nodes::*, types::Operator};
use alloy::primitives::U256;
use rand::{rngs::SmallRng, Rng};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum TypeHint {
    Int,
    Bool,
}

impl TypeHint {
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "int" => Some(TypeHint::Int),
            "bool" => Some(TypeHint::Bool),
            _ => None,
        }
    }
}

pub struct RewriteUtil {
    min_integer: U256,
    max_integer: U256,
    rng: SmallRng,
}

impl RewriteUtil {
    pub fn new(min_integer: U256, max_integer: U256, rng: SmallRng) -> Self {
        Self { min_integer, max_integer, rng }
    }

    pub fn get_random_int(&mut self) -> Expression {
        let value = if self.max_integer > self.min_integer {
            let range = (self.max_integer - self.min_integer).to::<u64>();
            let random_offset = self.rng.random_range(0..=range);
            self.min_integer + U256::from(random_offset)
        } else {
            self.min_integer
        };
        Expression::IntegerLiteral(IntegerLiteral { value })
    }

    pub fn get_random_bool(&mut self) -> Expression {
        let value = self.rng.random_bool(0.5);
        Expression::BooleanLiteral(BooleanLiteral { value })
    }
}

pub type MatchFunction = Box<dyn Fn(&mut HashMap<String, Expression>, &Expression) -> bool>;
pub type RewriteFunction =
    Box<dyn Fn(&mut HashMap<String, Expression>, &mut RewriteUtil) -> Expression>;

pub trait Parser<T> {
    fn process_number(&self, number: i64) -> T;
    fn process_boolean(&self, boolean: bool) -> T;
    fn process_placeholder(&self, identifier: String) -> T;
    fn process_random(&self, hint: TypeHint, identifier: String) -> T;
    fn process_unary_expression(&self, op: Operator, value: T) -> T;
    fn process_binary_expression(&self, op: Operator, lhs: T, rhs: T) -> T;
    fn process_type_hint(&self, hint: TypeHint, value: T) -> T;
    fn process_assert(&self, value: T) -> T;

    fn parse(&self, pattern: &str) -> Result<T, String> {
        let mut tokenizer = Tokenizer::new();
        let tokens = tokenizer.tokenize(pattern)?;

        let (ptr, result) = self.consume_node(&tokens, 0)?;

        if ptr != tokens.len() {
            return Err(format!("Unexpected remaining tokens at position {}", ptr));
        }

        Ok(result)
    }

    fn is_unary_operator_lookahead(&self, tokens: &[Token], ptr: usize) -> bool {
        if ptr >= tokens.len() {
            return false;
        }
        matches!(tokens[ptr].kind, TokenKind::Operator) && {
            if let TokenValue::String(ref op) = tokens[ptr].value {
                matches!(op.as_str(), "!" | "-" | "~")
            } else {
                false
            }
        }
    }

    fn is_binary_operator_lookahead(&self, tokens: &[Token], ptr: usize) -> bool {
        if ptr >= tokens.len() {
            return false;
        }
        matches!(tokens[ptr].kind, TokenKind::Operator) && {
            if let TokenValue::String(ref op) = tokens[ptr].value {
                matches!(
                    op.as_str(),
                    "+" | "-"
                        | "*"
                        | "/"
                        | "%"
                        | "&"
                        | "|"
                        | "^"
                        | "<<"
                        | ">>"
                        | "=="
                        | "!="
                        | "<"
                        | ">"
                        | "<="
                        | ">="
                        | "&&"
                        | "||"
                        | "^^"
                        | "**"
                )
            } else {
                false
            }
        }
    }

    fn is_type_hint_lookahead(&self, tokens: &[Token], ptr: usize) -> bool {
        ptr < tokens.len() && matches!(tokens[ptr].kind, TokenKind::Colon)
    }

    fn consume_token(
        &self,
        kind: TokenKind,
        tokens: &[Token],
        ptr: usize,
    ) -> Result<usize, String> {
        if ptr >= tokens.len() {
            return Err(format!("Expected {:?} but reached end of tokens", kind));
        }
        if tokens[ptr].kind != kind {
            return Err(format!(
                "Expected {:?} but got {:?} at position {}",
                kind, tokens[ptr].kind, ptr
            ));
        }
        Ok(ptr + 1)
    }

    fn consume_type_hint(&self, tokens: &[Token], ptr: usize) -> Result<(usize, TypeHint), String> {
        if ptr >= tokens.len() {
            return Err("Expected type hint but reached end of tokens".to_string());
        }

        if let TokenValue::String(ref type_str) = tokens[ptr].value {
            if let Some(hint) = TypeHint::from_str(type_str) {
                return Ok((ptr + 1, hint));
            }
            return Err(format!("Unsupported type hint '{}'", type_str));
        }

        Err(format!("Expected type hint identifier but got {:?}", tokens[ptr]))
    }

    fn consume_number(&self, tokens: &[Token], ptr: usize) -> Result<(usize, T), String> {
        if ptr >= tokens.len() {
            return Err("Expected number but reached end of tokens".to_string());
        }

        if let TokenValue::Number(num) = tokens[ptr].value {
            Ok((ptr + 1, self.process_number(num)))
        } else {
            Err(format!("Expected number but got {:?}", tokens[ptr]))
        }
    }

    fn consume_boolean(&self, tokens: &[Token], ptr: usize) -> Result<(usize, T), String> {
        if ptr >= tokens.len() {
            return Err("Expected boolean but reached end of tokens".to_string());
        }

        if let TokenValue::Boolean(b) = tokens[ptr].value {
            Ok((ptr + 1, self.process_boolean(b)))
        } else {
            Err(format!("Expected boolean but got {:?}", tokens[ptr]))
        }
    }

    fn consume_placeholder(&self, tokens: &[Token], ptr: usize) -> Result<(usize, T), String> {
        if ptr >= tokens.len() {
            return Err("Expected identifier but reached end of tokens".to_string());
        }

        if let TokenValue::String(ref identifier) = tokens[ptr].value {
            let mut ptr = ptr + 1;
            let func = self.process_placeholder(identifier.clone());

            if self.is_type_hint_lookahead(tokens, ptr) {
                ptr = self.consume_token(TokenKind::Colon, tokens, ptr)?;
                let (new_ptr, type_hint) = self.consume_type_hint(tokens, ptr)?;
                ptr = new_ptr;
                Ok((ptr, self.process_type_hint(type_hint, func)))
            } else {
                Ok((ptr, func))
            }
        } else {
            Err(format!("Expected identifier but got {:?}", tokens[ptr]))
        }
    }

    fn consume_random(&self, tokens: &[Token], ptr: usize) -> Result<(usize, T), String> {
        if ptr >= tokens.len() {
            return Err("Expected identifier but reached end of tokens".to_string());
        }

        if let TokenValue::String(ref identifier) = tokens[ptr].value {
            let mut ptr = ptr + 1;

            if self.is_type_hint_lookahead(tokens, ptr) {
                ptr = self.consume_token(TokenKind::Colon, tokens, ptr)?;
                let (new_ptr, type_hint) = self.consume_type_hint(tokens, ptr)?;
                ptr = new_ptr;
                Ok((ptr, self.process_random(type_hint, identifier.clone())))
            } else {
                Err("Random values require a type hint".to_string())
            }
        } else {
            Err(format!("Expected identifier but got {:?}", tokens[ptr]))
        }
    }

    fn consume_expression(&self, tokens: &[Token], ptr: usize) -> Result<(usize, T), String> {
        // Unary expression
        if self.is_unary_operator_lookahead(tokens, ptr) {
            let op = if let TokenValue::String(ref op_str) = tokens[ptr].value {
                Operator::from(op_str)
            } else {
                return Err("Invalid operator".to_string());
            };
            let ptr = ptr + 1;
            let (ptr, val_func) = self.consume_node(tokens, ptr)?;
            return Ok((ptr, self.process_unary_expression(op, val_func)));
        }

        // First node of binary expression
        let (mut ptr, lhs_func) = self.consume_node(tokens, ptr)?;

        // Binary expression
        if self.is_binary_operator_lookahead(tokens, ptr) {
            let op = if let TokenValue::String(ref op_str) = tokens[ptr].value {
                Operator::from(op_str)
            } else {
                return Err("Invalid operator".to_string());
            };
            ptr += 1;
            let (ptr, rhs_func) = self.consume_node(tokens, ptr)?;
            return Ok((ptr, self.process_binary_expression(op, lhs_func, rhs_func)));
        }

        Err(format!("Unexpected token at position {}", ptr))
    }

    fn consume_statement(&self, tokens: &[Token], ptr: usize) -> Result<(usize, T), String> {
        if ptr >= tokens.len() {
            return Err("Expected statement but reached end of tokens".to_string());
        }

        if let TokenValue::String(ref keyword) = tokens[ptr].value {
            if keyword == "assert" {
                let ptr = ptr + 1;
                let (ptr, val_func) = self.consume_node(tokens, ptr)?;
                return Ok((ptr, self.process_assert(val_func)));
            }
        }

        Err(format!("Unexpected token at position {}", ptr))
    }

    fn consume_node(&self, tokens: &[Token], ptr: usize) -> Result<(usize, T), String> {
        if ptr >= tokens.len() {
            return Err("Unexpected end of tokens".to_string());
        }

        match tokens[ptr].kind {
            TokenKind::Number => self.consume_number(tokens, ptr),
            TokenKind::Boolean => self.consume_boolean(tokens, ptr),
            TokenKind::QuestionMark => {
                let ptr = self.consume_token(TokenKind::QuestionMark, tokens, ptr)?;
                self.consume_placeholder(tokens, ptr)
            },
            TokenKind::Dollar => {
                let ptr = self.consume_token(TokenKind::Dollar, tokens, ptr)?;
                self.consume_random(tokens, ptr)
            },
            TokenKind::ParenthesisLeft => {
                let ptr = self.consume_token(TokenKind::ParenthesisLeft, tokens, ptr)?;
                let (ptr, func) = self.consume_expression(tokens, ptr)?;
                let ptr = self.consume_token(TokenKind::ParenthesisRight, tokens, ptr)?;
                Ok((ptr, func))
            },
            TokenKind::CurlyParenthesisLeft => {
                let ptr = self.consume_token(TokenKind::CurlyParenthesisLeft, tokens, ptr)?;
                let (ptr, func) = self.consume_statement(tokens, ptr)?;
                let ptr = self.consume_token(TokenKind::CurlyParenthesisRight, tokens, ptr)?;
                Ok((ptr, func))
            },
            _ => Err(format!("Unexpected token {:?} at position {}", tokens[ptr].kind, ptr)),
        }
    }
}

pub struct MatchParser;

impl MatchParser {
    pub fn new() -> Self {
        MatchParser
    }
}

impl Parser<MatchFunction> for MatchParser {
    fn process_number(&self, number: i64) -> MatchFunction {
        Box::new(
            move |_lookup: &mut HashMap<String, Expression>, node: &Expression| matches!(node, Expression::IntegerLiteral(IntegerLiteral { value }) if *value == U256::from(number.abs() as u64)),
        )
    }

    fn process_boolean(&self, boolean: bool) -> MatchFunction {
        Box::new(
            move |_lookup: &mut HashMap<String, Expression>, node: &Expression| matches!(node, Expression::BooleanLiteral(BooleanLiteral { value }) if *value == boolean),
        )
    }

    fn process_placeholder(&self, identifier: String) -> MatchFunction {
        Box::new(move |lookup: &mut HashMap<String, Expression>, node: &Expression| {
            if let Some(existing) = lookup.get(&identifier) {
                // Check structural equality
                format!("{:?}", existing) == format!("{:?}", node)
            } else {
                lookup.insert(identifier.clone(), node.clone());
                true
            }
        })
    }

    fn process_random(&self, _hint: TypeHint, _identifier: String) -> MatchFunction {
        panic!("Cannot match on random values");
    }

    fn process_unary_expression(&self, op: Operator, value: MatchFunction) -> MatchFunction {
        Box::new(move |lookup: &mut HashMap<String, Expression>, node: &Expression| {
            if let Expression::UnaryExpression(UnaryExpression { operator, value: expr_value }) =
                node
            {
                format!("{:?}", operator) == format!("{:?}", op) && value(lookup, expr_value)
            } else {
                false
            }
        })
    }

    fn process_binary_expression(
        &self,
        op: Operator,
        lhs: MatchFunction,
        rhs: MatchFunction,
    ) -> MatchFunction {
        Box::new(move |lookup: &mut HashMap<String, Expression>, node: &Expression| {
            if let Expression::BinaryExpression(BinaryExpression {
                operator,
                lhs: expr_lhs,
                rhs: expr_rhs,
            }) = node
            {
                format!("{:?}", operator) == format!("{:?}", op)
                    && lhs(lookup, expr_lhs)
                    && rhs(lookup, expr_rhs)
            } else {
                false
            }
        })
    }

    fn process_type_hint(&self, hint: TypeHint, value: MatchFunction) -> MatchFunction {
        Box::new(move |lookup: &mut HashMap<String, Expression>, node: &Expression| {
            let type_matches = match hint {
                TypeHint::Int => !is_boolean_expression(node),
                TypeHint::Bool => is_boolean_expression(node),
            };
            type_matches && value(lookup, node)
        })
    }

    fn process_assert(&self, value: MatchFunction) -> MatchFunction {
        Box::new(move |lookup: &mut HashMap<String, Expression>, node: &Expression| {
            // For assertions, we need to match against Statement, but this is simplified
            // In a full implementation, we'd need to handle Statement matching differently
            value(lookup, node)
        })
    }
}

pub struct RewriteParser;

impl RewriteParser {
    pub fn new() -> Self {
        RewriteParser
    }
}

impl Parser<RewriteFunction> for RewriteParser {
    fn process_number(&self, number: i64) -> RewriteFunction {
        Box::new(move |_lookup: &mut HashMap<String, Expression>, _util: &mut RewriteUtil| {
            Expression::IntegerLiteral(IntegerLiteral { value: U256::from(number.abs() as u64) })
        })
    }

    fn process_boolean(&self, boolean: bool) -> RewriteFunction {
        Box::new(move |_lookup: &mut HashMap<String, Expression>, _util: &mut RewriteUtil| {
            Expression::BooleanLiteral(BooleanLiteral { value: boolean })
        })
    }

    fn process_placeholder(&self, identifier: String) -> RewriteFunction {
        Box::new(move |lookup: &mut HashMap<String, Expression>, _util: &mut RewriteUtil| {
            lookup
                .get(&identifier)
                .cloned()
                .expect(&format!("Placeholder '{}' not found in lookup", identifier))
        })
    }

    fn process_random(&self, hint: TypeHint, identifier: String) -> RewriteFunction {
        Box::new(move |lookup: &mut HashMap<String, Expression>, util: &mut RewriteUtil| {
            if !lookup.contains_key(&identifier) {
                let random_value = match hint {
                    TypeHint::Int => util.get_random_int(),
                    TypeHint::Bool => util.get_random_bool(),
                };
                lookup.insert(identifier.clone(), random_value.clone());
                random_value
            } else {
                lookup.get(&identifier).cloned().unwrap()
            }
        })
    }

    fn process_unary_expression(&self, op: Operator, value: RewriteFunction) -> RewriteFunction {
        Box::new(move |lookup: &mut HashMap<String, Expression>, util: &mut RewriteUtil| {
            let expr_value = value(lookup, util);
            Expression::UnaryExpression(UnaryExpression {
                operator: op.clone(),
                value: Box::new(expr_value),
            })
        })
    }

    fn process_binary_expression(
        &self,
        op: Operator,
        lhs: RewriteFunction,
        rhs: RewriteFunction,
    ) -> RewriteFunction {
        Box::new(move |lookup: &mut HashMap<String, Expression>, util: &mut RewriteUtil| {
            let expr_lhs = lhs(lookup, util);
            let expr_rhs = rhs(lookup, util);
            Expression::BinaryExpression(BinaryExpression {
                operator: op.clone(),
                lhs: Box::new(expr_lhs),
                rhs: Box::new(expr_rhs),
            })
        })
    }

    fn process_type_hint(&self, _hint: TypeHint, value: RewriteFunction) -> RewriteFunction {
        value
    }

    fn process_assert(&self, value: RewriteFunction) -> RewriteFunction {
        value
    }
}

// Helper function to determine if an expression is boolean
fn is_boolean_expression(expr: &Expression) -> bool {
    match expr {
        Expression::BooleanLiteral(_) => true,
        Expression::BinaryExpression(BinaryExpression { operator, .. }) => {
            matches!(
                operator,
                Operator::Lt
                    | Operator::Lte
                    | Operator::Gt
                    | Operator::Gte
                    | Operator::Eq
                    | Operator::Neq
                    | Operator::And
                    | Operator::Or
            )
        },
        Expression::UnaryExpression(UnaryExpression { operator, .. }) => {
            matches!(operator, Operator::Not)
        },
        _ => false,
    }
}
