use super::{parser::RewriteUtil, rules::RewriteRule, visitor::NodeReplacer};
use crate::{config::Config, nodes::*};
use alloy::primitives::U256;
use rand::{rngs::SmallRng, Rng, SeedableRng};
use std::collections::HashMap;

#[derive(Clone)]
pub struct PointOfInterest {
    pub rule_name: String,
    pub target: Expression,
    pub parent: Option<Expression>,
    pub replacement: Option<Expression>,
}

impl PointOfInterest {
    pub fn new(rule_name: String, target: Expression, parent: Option<Expression>) -> Self {
        Self { rule_name, target, parent, replacement: None }
    }

    pub fn has_parent(&self) -> bool {
        self.parent.is_some()
    }

    pub fn apply_rule(&mut self, rule: &RewriteRule, rewrite_util: &mut RewriteUtil) -> Expression {
        self.replacement = rule.rewrite(&self.target, rewrite_util);
        self.replacement
            .clone()
            .expect(&format!("Unable to apply rule {} to target", self.rule_name))
    }
}

pub struct RuleBasedRewriter {
    config: Config,
    rules: Vec<RewriteRule>,
    points_of_interest: HashMap<String, Vec<PointOfInterest>>,
}

impl RuleBasedRewriter {
    pub fn new(config: Config, rules: Vec<RewriteRule>) -> Self {
        Self { config, rules, points_of_interest: HashMap::new() }
    }

    /// Run the rewriter on an expression, applying `amount` random rewrites
    pub fn run_on_expression(
        &mut self,
        expr: Expression,
        amount: Option<usize>,
        random: &mut impl Rng,
    ) -> (Vec<PointOfInterest>, Expression) {
        let mut root = expr;
        let mut applied_rules = Vec::new();
        let replacer = NodeReplacer::new();

        let amount = amount.unwrap_or_else(|| {
            random.random_range(
                self.config.rewrite.min_rewrites as usize
                    ..=self.config.rewrite.max_rewrites as usize,
            )
        });

        for _ in 0..amount {
            self.points_of_interest.clear();
            self.collect_rules(&root, None);

            if self.points_of_interest.is_empty() {
                break; // No more applicable rules
            }

            // Select a random rule
            let rule_names: Vec<String> = self.points_of_interest.keys().cloned().collect();
            let rule_name = &rule_names[random.random_range(0..rule_names.len())];
            let pois = self.points_of_interest.get(rule_name).unwrap();
            let poi_idx = random.random_range(0..pois.len());
            let mut poi = pois[poi_idx].clone();

            // Find the rule
            let rule = self.rules.iter().find(|r| &r.name == rule_name).unwrap();

            // Apply the rule with a seeded rewrite util
            let seed = random.random();
            let mut rewrite_util = RewriteUtil::new(
                U256::ZERO,
                U256::from(1000000u64), // Max field value
                SmallRng::seed_from_u64(seed),
            );

            // Check if we're targeting the root
            if format!("{:?}", poi.target) == format!("{:?}", root) {
                assert!(!poi.has_parent(), "Unexpected parent for starting node");
                root = poi.apply_rule(rule, &mut rewrite_util);
                applied_rules.push(poi);
                continue;
            }

            // We're targeting a sub-node
            assert!(poi.has_parent(), "Unexpected orphan target for point of interest");
            let replacement = poi.apply_rule(rule, &mut rewrite_util);

            if let Some(ref mut parent) = poi.parent {
                let is_replaced = replacer.replace(parent, &poi.target, replacement);
                assert!(is_replaced, "Unable to find origin node");
                // Update root with the modified parent
                root = parent.clone();
            }

            applied_rules.push(poi);
        }

        (applied_rules, root)
    }

    /// Collect all applicable rules for the given expression and its sub-expressions
    fn collect_rules(&mut self, expr: &Expression, parent: Option<Expression>) {
        // Check if any rules apply to this expression
        for rule in &self.rules {
            if rule.is_applicable(expr) {
                self.points_of_interest
                    .entry(rule.name.clone())
                    .or_insert_with(Vec::new)
                    .push(PointOfInterest::new(rule.name.clone(), expr.clone(), parent.clone()));
            }
        }

        // Recursively collect rules for sub-expressions
        match expr {
            Expression::BinaryExpression(bin_expr) => {
                self.collect_rules(&bin_expr.lhs, Some(expr.clone()));
                // Don't mess with the right-hand side if it's a power operation
                if !matches!(bin_expr.operator, crate::types::Operator::Comp) {
                    self.collect_rules(&bin_expr.rhs, Some(expr.clone()));
                }
            },
            Expression::UnaryExpression(un_expr) => {
                self.collect_rules(&un_expr.value, Some(expr.clone()));
            },
            Expression::CallExpression(call_expr) => {
                self.collect_rules(&call_expr.reference, Some(expr.clone()));
                for arg in &call_expr.arguments {
                    self.collect_rules(arg, Some(expr.clone()));
                }
            },
            Expression::IndexAccessExpression(idx_expr) => {
                self.collect_rules(&idx_expr.reference, Some(expr.clone()));
                self.collect_rules(&idx_expr.index, Some(expr.clone()));
            },
            Expression::FieldAccessExpression(field_expr) => {
                self.collect_rules(&field_expr.reference, Some(expr.clone()));
            },
            Expression::ListLiteral(list_lit) => {
                for item in &list_lit.value {
                    self.collect_rules(item, Some(expr.clone()));
                }
            },
            Expression::TupleLiteral(tuple_lit) => {
                for item in &tuple_lit.value {
                    self.collect_rules(item, Some(expr.clone()));
                }
            },
            _ => {}, // Identifiers, literals have no sub-expressions
        }
    }
}
