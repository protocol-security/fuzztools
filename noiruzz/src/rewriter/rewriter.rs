use crate::nodes::ASTNode;
use crate::config::RewriteRule;

pub struct PointOfInterest {
    pub rule: RewriteRule,
    pub target: ASTNode,
    pub parent: Option<ASTNode>,
    pub replacement: Option<ASTNode>,
}

impl PointOfInterest {
    pub fn has_parent(&self) -> bool {
        self.parent.is_some()
    }

    pub fn apply_rules(&self, rewrite_util: RewriteUtil) -> ASTNode {
        self.replacement = self.rule.rewrite(self.target, rewrite_util);
        return self.replacement.unwrap();
    }
}

pub struct Rewriter {

}