use crate::nodes::*;

pub(crate) struct Traveller {
    indent: u64,
    buffer: String,
}

impl Traveller {
    pub(crate) fn emit(&mut self, node: ASTNode) -> String {
        self.indent = 0;
        self.buffer = String::new();
        self.visit(node);

        return self.buffer.clone();
    }

    fn visit(&mut self, node: ASTNode) {
        match node {
            ASTNode::Expression(expression) => self.visit_expression(expression),
            ASTNode::Statement(statement) => self.visit_statement(statement),
            ASTNode::Definition(definition) => self.visit_definition(definition),
        }
    }

    // @audit missing while loops and unconstrained functions

    fn visit_expression(&mut self, expression: Expression) {
        match expression {
            Expression::Identifier(identifier) => self.visit_identifier(identifier),
            Expression::BinaryExpression(binary_expression) => self.visit_binary_expression(binary_expression),
            Expression::UnaryExpression(unary_expression) => self.visit_unary_expression(unary_expression),
            Expression::CallExpression(call_expression) => self.visit_call_expression(call_expression),
            Expression::IndexAccessExpression(index_access_expression) => self.visit_index_access_expression(index_access_expression),
            Expression::FieldAccessExpression(field_access_expression) => self.visit_field_access_expression(field_access_expression),
            Expression::StringLiteral(string_literal) => self.visit_string_literal(string_literal),
            Expression::BooleanLiteral(boolean_literal) => self.visit_boolean_literal(boolean_literal),
            Expression::IntegerLiteral(integer_literal) => self.visit_integer_literal(integer_literal),
            Expression::ListLiteral(list_literal) => self.visit_list_literal(list_literal),
            Expression::TupleLiteral(tuple_literal) => self.visit_tuple_literal(tuple_literal),
        }
    }

    fn visit_statement(&mut self, statement: Statement) {
        match statement {
            Statement::BasicBlock(basic_block) => self.visit_basic_block(basic_block),
            Statement::IfStatement(if_statement) => self.visit_if_statement(if_statement),
            Statement::ForStatement(for_statement) => self.visit_for_statement(for_statement),
            Statement::LetStatement(let_statement) => self.visit_let_statement(let_statement),
            Statement::AssignStatement(assign_statement) => self.visit_assign_statement(assign_statement),
            Statement::AssertStatement(assert_statement) => self.visit_assert_statement(assert_statement),
            Statement::ExpressionStatement(expression_statement) => self.visit_expression_statement(expression_statement),
            Statement::ReturnStatement(return_statement) => self.visit_return_statement(return_statement),
        }
    }

    fn visit_definition(&mut self, definition: Definition) {
        match definition {
            Definition::VariableDefinition(variable_definition) => self.visit_variable_definition(variable_definition),
            Definition::FunctionDefinition(function_definition) => self.visit_function_definition(function_definition),
            Definition::Document(document) => self.visit_document(document),
        }
    }

    fn visit_identifier(&mut self, node: Identifier) {
        self.buffer.push_str(&node.name);
    }

    fn visit_binary_expression(&mut self, node: BinaryExpression) {
        self.buffer.push_str("(");
        self.visit(ASTNode::Expression(*node.lhs));
        self.buffer.push_str(&format!(" {} ", node.operator));
        self.visit(ASTNode::Expression(*node.rhs));
        self.buffer.push_str(")");
    }

    fn visit_unary_expression(&mut self, node: UnaryExpression) {
        self.buffer.push_str("(");
        self.buffer.push_str(&format!("{} ", node.operator));
        self.visit(ASTNode::Expression(*node.value));
        self.buffer.push_str(")");
    }

    fn visit_call_expression(&mut self, node: CallExpression) {
        self.visit(ASTNode::Expression(*node.reference));
        self.buffer.push_str("(");
        self.print_comma_separated_expressions(node.arguments);
        self.buffer.push_str(")");
    }

    fn visit_index_access_expression(&mut self, node: IndexAccessExpression) {
        self.visit(ASTNode::Expression(*node.reference));
        self.buffer.push_str("[");
        self.visit(ASTNode::Expression(*node.index));
        self.buffer.push_str("]");
    }

    fn visit_field_access_expression(&mut self, node: FieldAccessExpression) {
        self.visit(ASTNode::Expression(*node.reference));
        self.buffer.push_str(".");
        self.visit(ASTNode::Expression(Expression::Identifier(node.field)));
    }

    fn visit_basic_block(&mut self, node: BasicBlock) {
        self.buffer.push_str(&format!("{}{{\n", self.current_indent()));
        self.indent += 4;
        for statement in node.statements {
            self.visit(ASTNode::Statement(*statement));
            self.buffer.push_str("\n");
        }
        self.indent -= 4;
        self.buffer.push_str(&format!("{}}}", self.current_indent()));
    }

    fn visit_if_statement(&mut self, node: IfStatement) {
        self.buffer.push_str(&format!("{}if ", self.current_indent()));
        self.visit(ASTNode::Expression(*node.condition));
        self.buffer.push_str("\n");
        self.visit(ASTNode::Statement(*node.true_stmt));
        self.buffer.push_str("\n");
        if node.false_stmt.is_some() {
            self.buffer.push_str(&format!("{}else\n", self.current_indent()));
            self.visit(ASTNode::Statement(*node.false_stmt.unwrap()));
        }
    }

    fn visit_for_statement(&mut self, node: ForStatement) {
        self.buffer.push_str(&format!("{}for ", self.current_indent()));
        self.visit(ASTNode::Expression(Expression::Identifier(node.index)));
        self.buffer.push_str(" in ");
        self.visit(ASTNode::Expression(*node.start));
        self.buffer.push_str(".."); // @audit what about = inclusive?
        self.visit(ASTNode::Expression(*node.end));
        self.buffer.push_str(" {\n");
        self.indent += 4;
        for statement in node.statements {
            self.visit(ASTNode::Statement(*statement));
            self.buffer.push_str("\n");
        }
        self.indent -= 4;
        self.buffer.push_str(&format!("{}}}", self.current_indent()));
    }

    fn visit_let_statement(&mut self, node: LetStatement) {
        self.buffer.push_str(&format!("{}let ", self.current_indent()));
        if node.is_mutable {
            self.buffer.push_str("mut ");
        }
        self.visit(ASTNode::Expression(Expression::Identifier(node.name)));
        if node.type_.is_some() {
            self.buffer.push_str(&format!(" : {}", node.type_.unwrap()));
        }
        if node.expr.is_some() {
            self.buffer.push_str(" = ");
            self.visit(ASTNode::Expression(*node.expr.unwrap()));
        }
        self.buffer.push_str(";");
    }

    fn visit_assign_statement(&mut self, node: AssignStatement) {
        self.buffer.push_str(&format!("{}", self.current_indent()));
        self.visit(ASTNode::Expression(*node.lhs));
        self.buffer.push_str(" = ");
        self.visit(ASTNode::Expression(*node.rhs));
        self.buffer.push_str(";");
    }

    fn visit_assert_statement(&mut self, node: AssertStatement) {
        self.buffer.push_str(&format!("{}assert(", self.current_indent()));
        self.visit(ASTNode::Expression(*node.condition));
        if node.message.is_some() {
            self.buffer.push_str(", ");
            self.visit(ASTNode::Expression(Expression::StringLiteral(node.message.unwrap())));
        }
        self.buffer.push_str(");");
    }

    fn visit_expression_statement(&mut self, node: ExpressionStatement) {
        self.buffer.push_str(&format!("{}", self.current_indent()));
        self.visit(ASTNode::Expression(*node.expr));
        if node.is_semicolon {
            self.buffer.push_str(";");
        }
    }

    fn visit_return_statement(&mut self, node: ReturnStatement) {
        self.buffer.push_str(&format!("{}return ", self.current_indent()));
        self.visit(ASTNode::Expression(*node.value));
        self.buffer.push_str(";");
    }

    fn visit_string_literal(&mut self, node: StringLiteral) {
        self.buffer.push_str(&format!("\"{}\"", node.value));
    }

    fn visit_boolean_literal(&mut self, node: BooleanLiteral) {
        self.buffer.push_str(&format!("{}", if node.value { "true" } else { "false" }));
    }

    fn visit_integer_literal(&mut self, node: IntegerLiteral) {
        self.buffer.push_str(&format!("{}", node.value));
    }

    fn visit_list_literal(&mut self, node: ListLiteral) {
        self.buffer.push_str("[");
        self.print_comma_separated_expressions(node.value);
        self.buffer.push_str("]");
    }

    fn visit_tuple_literal(&mut self, node: TupleLiteral) {
        self.buffer.push_str("(");
        self.print_comma_separated_expressions(node.value);
        self.buffer.push_str(")");
    }

    fn visit_function_definition(&mut self, node: FunctionDefinition) {
        if node.is_public {
            self.buffer.push_str("pub ");
        }
        self.buffer.push_str(&format!("{}fn ", self.current_indent()));
        self.visit(ASTNode::Expression(Expression::Identifier(node.name)));
        self.buffer.push_str("(");
        self.print_comma_separated_variables(node.arguments);
        self.buffer.push_str(")");
        if node.type_.is_some() {
            if node.is_public_return {
                self.buffer.push_str(&format!(" -> pub {}", node.type_.unwrap()));
            } else {
                self.buffer.push_str(&format!(" -> {}", node.type_.unwrap()));
            }
        }
        match *node.body {
            Statement::BasicBlock(basic_block) => {
                self.buffer.push_str(" ");
                self.visit_basic_block(basic_block);
            }
            _ => {
                self.buffer.push_str("\n");
                self.indent += 4;
                self.visit(ASTNode::Statement(*node.body));
                self.indent -= 4;
            }
        }
    }

    fn visit_variable_definition(&mut self, node: VariableDefinition) {
        self.visit(ASTNode::Expression(Expression::Identifier(node.name)));
        self.buffer.push_str(&format!(" : {}", node.type_));
    }
        
    fn visit_document(&mut self, node: Document) {
        self.buffer.push_str("use dep::std;\n\n");
        self.visit(ASTNode::Definition(Definition::FunctionDefinition(node.main)));
    }

    fn current_indent(&self) -> String {
        return " ".repeat(self.indent as usize);
    }

    fn print_comma_separated_expressions(&mut self, arguments: Vec<Box<Expression>>) {
        let argument_count = arguments.len();
        let mut argument_index = 0;
        for argument in arguments {
            self.visit(ASTNode::Expression(*argument));
            argument_index += 1;
            if argument_index < argument_count {
                self.buffer.push_str(", ");
            }
        }
    }

    fn print_comma_separated_variables(&mut self, arguments: Vec<VariableDefinition>) {
        for (i, argument) in arguments.iter().enumerate() {
            if i > 0 {
                self.buffer.push_str(", ");
            }
            self.visit(ASTNode::Expression(Expression::Identifier(argument.name.clone())));
        }
    }
}