use crate::nodes::*;

/// NodeReplacer is a visitor that replaces a target node with a replacement node
pub struct NodeReplacer;

impl NodeReplacer {
    pub fn new() -> Self {
        NodeReplacer
    }

    /// Replace target with replacement in the tree rooted at parent
    /// Returns true if replacement was successful
    pub fn replace(
        &self,
        parent: &mut Expression,
        target: &Expression,
        replacement: Expression,
    ) -> bool {
        self.replace_in_expression(parent, target, replacement)
    }

    fn expressions_equal(&self, a: &Expression, b: &Expression) -> bool {
        format!("{:?}", a) == format!("{:?}", b)
    }

    fn replace_in_expression(
        &self,
        expr: &mut Expression,
        target: &Expression,
        replacement: Expression,
    ) -> bool {
        match expr {
            Expression::BinaryExpression(bin_expr) => {
                if self.expressions_equal(&*bin_expr.lhs, target) {
                    bin_expr.lhs = Box::new(replacement);
                    return true;
                }
                if self.expressions_equal(&*bin_expr.rhs, target) {
                    bin_expr.rhs = Box::new(replacement);
                    return true;
                }
                if self.replace_in_expression(&mut bin_expr.lhs, target, replacement.clone()) {
                    return true;
                }
                if self.replace_in_expression(&mut bin_expr.rhs, target, replacement) {
                    return true;
                }
            },
            Expression::UnaryExpression(un_expr) => {
                if self.expressions_equal(&*un_expr.value, target) {
                    un_expr.value = Box::new(replacement);
                    return true;
                }
                if self.replace_in_expression(&mut un_expr.value, target, replacement) {
                    return true;
                }
            },
            Expression::CallExpression(call_expr) => {
                if self.expressions_equal(&*call_expr.reference, target) {
                    call_expr.reference = Box::new(replacement);
                    return true;
                }
                if self.replace_in_expression(&mut call_expr.reference, target, replacement.clone())
                {
                    return true;
                }
                for arg in &mut call_expr.arguments {
                    if self.expressions_equal(&**arg, target) {
                        *arg = Box::new(replacement.clone());
                        return true;
                    }
                    if self.replace_in_expression(arg, target, replacement.clone()) {
                        return true;
                    }
                }
            },
            Expression::IndexAccessExpression(idx_expr) => {
                if self.expressions_equal(&*idx_expr.reference, target) {
                    idx_expr.reference = Box::new(replacement);
                    return true;
                }
                if self.expressions_equal(&*idx_expr.index, target) {
                    idx_expr.index = Box::new(replacement);
                    return true;
                }
                if self.replace_in_expression(&mut idx_expr.reference, target, replacement.clone())
                {
                    return true;
                }
                if self.replace_in_expression(&mut idx_expr.index, target, replacement) {
                    return true;
                }
            },
            Expression::FieldAccessExpression(field_expr) => {
                if self.expressions_equal(&*field_expr.reference, target) {
                    field_expr.reference = Box::new(replacement);
                    return true;
                }
                if self.replace_in_expression(&mut field_expr.reference, target, replacement) {
                    return true;
                }
            },
            Expression::ListLiteral(list_lit) => {
                for item in &mut list_lit.value {
                    if self.expressions_equal(&**item, target) {
                        *item = Box::new(replacement.clone());
                        return true;
                    }
                    if self.replace_in_expression(item, target, replacement.clone()) {
                        return true;
                    }
                }
            },
            Expression::TupleLiteral(tuple_lit) => {
                for item in &mut tuple_lit.value {
                    if self.expressions_equal(&**item, target) {
                        *item = Box::new(replacement.clone());
                        return true;
                    }
                    if self.replace_in_expression(item, target, replacement.clone()) {
                        return true;
                    }
                }
            },
            _ => {},
        }
        false
    }
}
