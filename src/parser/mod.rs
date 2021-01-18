use std::option::Option::Some;
use std::sync::Arc;

use crate::errors::KtError;
use crate::parser::parse_tree::{Annotation, Block, CallSiteTypeParams, CallSuffix, CatchBlock, Class, ClassBody, DelegationSpecifier, DoWhileStatement, Expr, Expression, ExprSuffix, FileAnnotation, ForStatement, Function, FunctionBody, FunctionLiteral, FunctionParameter, FunctionType, Import, KotlinFile, Modifier, Object, PackageHeader, PrimaryConstructor, Property, PropertyGetter, PropertyInitialization, PropertySetter, SetterParameter, SimpleType, Statement, StatementBlock, StringComponent, TopLevelObject, Type, TypeAlias, TypeConstraint, TypeParameter, TypeReference, UserType, ValueArgument, Variable, VariableName, VariablePattern, WhenCondition, WhenEntry, WhileStatement};
use crate::parser::token_cursor::TokenCursor;
use crate::source::{BytePos, ByteSpan, Source, SourceSpan};
use crate::source_cursor::SourceCursor;
use crate::token::Token;
use crate::token_stream::{TokenStream, TokenStreamErrorKind};

// mod file;
mod token_cursor;
pub mod parse_tree;
pub mod display;

pub struct Parser {
    cursor: TokenCursor,
    errors: Vec<ParserError>,
    parsing_delegated_by: bool,
}

#[derive(Debug, Clone)]
pub struct ParserError {
    pub span: SourceSpan,
    pub kind: ParserErrorKind,
}

#[derive(Debug, Clone)]
pub enum ParserErrorKind {
    UnexpectedToken { found: Token },
    ExpectedToken { expected: Token, found: Token },
    ExpectedTokenId { found: Token },
    ExpectedTokenOf { found: Token, options: Vec<Token> },
    TokenStreamError { kind: TokenStreamErrorKind },
}

impl Parser {
    pub fn new(cursor: TokenCursor) -> Self {
        Self { cursor, errors: vec![], parsing_delegated_by: false }
    }

    pub fn from(source: Source) -> Parser {
        Self::new(
            TokenCursor::new(
                TokenStream::new(
                    SourceCursor::new(source)
                )
            )
        )
    }

    pub fn parse(mut self) -> Result<KotlinFile, Vec<ParserError>> {
        if let Err(e) = self.cursor.init() {
            return Err(vec![e]);
        }

        let file = self.parse_file();

        if !self.errors.is_empty() {
            Err(self.errors)
        } else {
            Ok(file)
        }
    }

    fn parse_file(&mut self) -> KotlinFile {
        // File annotations
        let file_annotations = vec![];

        // TODO file annotations

        // Package
        let mut package_header = None;

        if self.cursor.token() == &Token::Package {
            match self.parse_package() {
                Ok(pkg) => {
                    package_header = Some(pkg)
                }
                Err(err) => {
                    #[cfg(test)] eprintln!("{}", err);
                    self.errors.push(err);

                    if let Err(e) = self.skip_to_next_top_level_object() {
                        #[cfg(test)] eprintln!("{}", e);
                        self.errors.push(e);
                    }
                }
            }
        }

        // Imports
        let mut imports = vec![];

        while self.cursor.match_keyword("import") {
            match self.parse_import() {
                Ok(imp) => {
                    imports.push(imp);
                }
                Err(err) => {
                    #[cfg(test)] eprintln!("{}", err);
                    self.errors.push(err);

                    if let Err(e) = self.skip_to_next_top_level_object() {
                        #[cfg(test)] eprintln!("{}", e);
                        self.errors.push(e);
                    }
                }
            }
        }

        // Top level objects
        let mut objects = vec![];

        while !self.cursor.match_token(Token::EOF) {
            match self.parse_top_level_object() {
                Ok(obj) => {
                    objects.push(obj);
                }
                Err(err) => {
                    #[cfg(test)] eprintln!("{}", err);
                    self.errors.push(err);

                    if let Err(e) = self.skip_to_next_top_level_object() {
                        #[cfg(test)] eprintln!("{}", e);
                        self.errors.push(e);
                    }
                }
            }
        }

        KotlinFile { file_annotations, package_header, imports, objects }
    }

    fn parse_file_annotation(&mut self) -> Option<FileAnnotation> {
        self.todo()
    }

    fn parse_package(&mut self) -> Result<PackageHeader, ParserError> {
        let start = self.cursor.start();
        self.cursor.expect(Token::Package)?;
        let path = self.parse_path()?;

        Ok(PackageHeader {
            span: self.cursor.make_span(start),
            path,
        })
    }

    fn parse_import(&mut self) -> Result<Import, ParserError> {
        let start = self.cursor.start();
        self.cursor.expect_keyword("import")?;

        let path = self.parse_path()?;

        let mut alias = None;
        if self.cursor.match_token(Token::As) {
            self.cursor.expect(Token::As)?;
            alias = Some(self.cursor.expect_id()?);
        }

        Ok(Import {
            span: self.cursor.make_span(start),
            path,
            alias,
        })
    }

    fn parse_path(&mut self) -> Result<Vec<String>, ParserError> {
        let mut path = vec![self.cursor.expect_id()?];

        while self.cursor.token() == &Token::Dot {
            self.cursor.expect(Token::Dot)?;
            path.push(self.cursor.expect_id()?);
        }

        Ok(path)
    }

    fn parse_top_level_object(&mut self) -> Result<TopLevelObject, ParserError> {
        let start = self.cursor.start();
        let modifiers = self.parse_top_level_modifiers()?;

        return match self.cursor.token() {
            Token::Class => {
                self.parse_class(start, modifiers).map(|i| TopLevelObject::Class(i))
            }
            Token::Object => {
                self.parse_object(start, modifiers).map(|i| TopLevelObject::Object(i))
            }
            Token::Fun => {
                self.parse_function(start, modifiers).map(|i| TopLevelObject::Function(i))
            }
            Token::Val => {
                self.parse_top_level_property(start, modifiers, false).map(|i| TopLevelObject::Property(i))
            }
            Token::Var => {
                self.parse_top_level_property(start, modifiers, true).map(|i| TopLevelObject::Property(i))
            }
            Token::TypeAlias => {
                self.parse_typealias(start, modifiers).map(|i| TopLevelObject::TypeAlias(i))
            }
            _ => {
                Err(ParserError {
                    span: self.cursor.make_source_span(start),
                    kind: ParserErrorKind::ExpectedTokenOf {
                        found: self.cursor.token().clone(),
                        options: vec![
                            Token::Fun,
                            Token::Val,
                            Token::Var,
                            Token::TypeAlias,
                            Token::Class,
                            Token::Object,
                        ],
                    },
                })
            }
        };
    }

    fn parse_top_level_modifiers(&mut self) -> Result<Vec<Modifier>, ParserError> {
        let mut vec = vec![];

        while let Token::Id(name) = self.cursor.token() {
            let modifier = match name.as_str() {
                "abstract" => Modifier::Abstract,
                "final" => Modifier::Final,
                "enum" => Modifier::Enum,
                "open" => Modifier::Open,
                "annotation" => Modifier::Annotation,
                "sealed" => Modifier::Sealed,
                "data" => Modifier::Data,
                "lateinit" => Modifier::Lateinit,
                "private" => Modifier::Private,
                "protected" => Modifier::Protected,
                "public" => Modifier::Public,
                "internal" => Modifier::Internal,
                "tailrec" => Modifier::Tailrec,
                "operator" => Modifier::Operator,
                "infix" => Modifier::Infix,
                "inline" => Modifier::Inline,
                "external" => Modifier::External,
                "suspend" => Modifier::Suspend,
                "const" => Modifier::Const,
                "expect" => Modifier::Expect,
                "actual" => Modifier::Actual,
                "inner" => Modifier::Inner,
                "override" => Modifier::Override,
                _ => {
                    break;
                }
            };

            self.cursor.expect_id()?;
            vec.push(modifier);
        }

        Ok(vec)
    }

    fn parse_class(&mut self, start: BytePos, modifiers: Vec<Modifier>) -> Result<Class, ParserError> {
        let start = self.cursor.start();

        self.cursor.expect(Token::Class)?;
        let name = self.cursor.expect_id()?;

        // <T>
        let mut type_parameters = vec![];

        if self.cursor.match_token(Token::LeftAngleBracket) {
            type_parameters = self.parse_type_parameters()?;
        }

        // ()
        let mut primary_constructor = None;
        let constructor_modifiers = self.parse_constructor_modifiers()?;

        if !constructor_modifiers.is_empty() || self.cursor.match_token(Token::LeftParen) {
            primary_constructor = Some(self.parse_primary_constructor(constructor_modifiers)?);
        }

        //  : Interface
        let mut delegation_specifiers = vec![];

        if self.cursor.match_token(Token::Colon) {
            self.cursor.expect(Token::Colon)?;

            loop {
                delegation_specifiers.push(self.parse_delegation_specifier()?);

                if self.cursor.match_token(Token::Comma) {
                    self.cursor.expect(Token::Comma)?;
                } else {
                    break;
                }
            }
        }

        // where T: Number
        let mut type_constraints = vec![];

        if self.cursor.match_keyword("where") {
            self.cursor.expect_keyword("where")?;

            loop {
                type_constraints.push(self.parse_type_constraint()?);

                if self.cursor.match_token(Token::Comma) {
                    self.cursor.expect(Token::Comma)?;
                } else {
                    break;
                }
            }
        }

        // {}
        let mut body = None;

        if self.cursor.match_token(Token::LeftBrace) {
            body = Some(self.parse_class_body()?);
        }

        Ok(Class {
            span: self.cursor.make_span(start),
            annotations: vec![],
            modifiers,
            name,
            type_parameters,
            primary_constructor,
            delegation_specifiers,
            type_constraints,
            body,
        })
    }

    fn parse_object(&mut self, start: BytePos, modifiers: Vec<Modifier>) -> Result<Object, ParserError> {
        self.cursor.expect(Token::Object)?;
        self.todo()
    }

    fn parse_constructor_modifiers(&mut self) -> Result<Vec<Modifier>, ParserError> {
        let mut modifiers = vec![];

        loop {
            let modifier = match self.cursor.token() {
                Token::Id(id) if id == "public" => Modifier::Public,
                Token::Id(id) if id == "private" => Modifier::Private,
                Token::Id(id) if id == "protected" => Modifier::Protected,
                Token::Id(id) if id == "internal" => Modifier::Internal,
                _ => {
                    break;
                }
            };

            self.cursor.next()?;
            modifiers.push(modifier);
        }

        Ok(modifiers)
    }

    fn parse_primary_constructor(&mut self, modifiers: Vec<Modifier>) -> Result<PrimaryConstructor, ParserError> {
        let start = self.cursor.start();

        if self.cursor.match_keyword("constructor") {
            self.cursor.expect_keyword("constructor")?;
        }

        let mut value_parameters = vec![];

        self.cursor.expect(Token::LeftParen)?;

        if !self.cursor.match_token(Token::RightParen) {
            loop {
                value_parameters.push(self.parse_function_parameter()?);

                if self.cursor.match_token(Token::Comma) {
                    self.cursor.expect(Token::Comma)?;
                } else {
                    break;
                }
            }
        }

        self.cursor.expect(Token::RightParen)?;

        Ok(PrimaryConstructor {
            span: self.cursor.make_span(start),
            modifiers,
            value_parameters,
        })
    }

    fn parse_class_body(&mut self) -> Result<ClassBody, ParserError> {
        let start = self.cursor.start();

        // tmp
        self.cursor.expect(Token::LeftBrace)?;
        // TODO
        self.cursor.expect(Token::RightBrace)?;

        Ok(ClassBody {
            span: self.cursor.make_span(start),
            enum_entries: vec![],
            members: vec![],
        })
    }

    fn parse_delegation_specifier(&mut self) -> Result<DelegationSpecifier, ParserError> {
        let start = self.cursor.start();
        // https://github.com/Kotlin/kotlin-spec/blob/spec-rework/src/grammar/KotlinParser.g4#L89

        if self.cursor.match_id() {
            // Number() or Comparable<Int>

            let sty = self.parse_simple_type()?;

            if sty.nullable {
                return Err(ParserError {
                    span: self.cursor.make_source_span(start),
                    kind: ParserErrorKind::UnexpectedToken {
                        found: Token::QuestionMark
                    },
                });
            }

            return if self.cursor.match_token(Token::LeftParen) {
                // constructor
                let ctor_start = self.cursor.start();
                let mut value_arguments = vec![];

                // constructor arguments
                self.cursor.expect(Token::LeftParen)?;
                while !self.cursor.match_token(Token::RightParen) {
                    value_arguments.push(self.parse_value_argument()?);

                    if self.cursor.match_token(Token::Comma) {
                        self.cursor.expect(Token::Comma)?;
                    } else {
                        break;
                    }
                }
                self.cursor.expect(Token::RightParen)?;

                let suffix = CallSuffix {
                    span: self.cursor.make_span(ctor_start),
                    value_arguments,
                };

                Ok(DelegationSpecifier::FunctionCall(
                    Type {
                        span: self.cursor.make_span(start),
                        annotations: vec![],
                        reference: TypeReference::SimpleType(sty),
                    },
                    suffix,
                ))
            } else if self.cursor.match_keyword("by") {
                // Interface by implementation
                self.cursor.expect_keyword("by")?;

                // Needed to avoid ambiguity with lambda and class body
                self.parsing_delegated_by = true;
                let opt_expr = self.parse_expression();
                self.parsing_delegated_by = false;

                let expr = opt_expr?;

                Ok(DelegationSpecifier::DelegatedBy(
                    Type {
                        span: self.cursor.make_span(start),
                        annotations: vec![],
                        reference: TypeReference::SimpleType(sty),
                    },
                    expr,
                ))
            } else {
                // interface
                Ok(DelegationSpecifier::Type(Type {
                    span: self.cursor.make_span(start),
                    annotations: vec![],
                    reference: TypeReference::SimpleType(sty),
                }))
            };
        } else {
            // () -> Unit
            Ok(DelegationSpecifier::Type(self.parse_type()?))
        }
    }

    fn parse_value_argument(&mut self) -> Result<ValueArgument, ParserError> {
        let start = self.cursor.start();
        let mut named = None;

        if self.cursor.offset_token(1) == &Token::Equals {
            named = Some(self.cursor.expect_id()?);
            self.cursor.expect(Token::Equals)?;
        }

        let mut spread = false;

        if self.cursor.match_token(Token::Asterisk) {
            self.cursor.expect(Token::Asterisk)?;
            spread = true;
        }

        let expr = self.parse_expression()?;

        Ok(ValueArgument {
            span: self.cursor.make_span(start),
            named,
            spread,
            expr,
        })
    }

    fn parse_function(&mut self, start: BytePos, modifiers: Vec<Modifier>) -> Result<Function, ParserError> {
        self.cursor.expect(Token::Fun)?;

        // Type parameters '<T>' or '<A: B, T, C: List<B>>'
        let type_parameters = self.parse_type_parameters()?;

        // Parse function name or receiver and name
        // name
        // List<Int>.name
        let (receiver, name) = self.parse_receiver_and_name()?;

        // Parameters '(a: Int)'
        let mut value_parameters = vec![];

        let args_start = self.cursor.start();
        self.cursor.expect(Token::LeftParen)?;

        if !self.cursor.match_token(Token::RightParen) {
            loop {
                value_parameters.push(self.parse_function_parameter()?);

                if self.cursor.match_token(Token::Comma) {
                    self.cursor.expect(Token::Comma)?;
                } else if self.cursor.match_token(Token::RightParen) {
                    break;
                } else {
                    self.cursor.make_error(
                        self.cursor.make_span(args_start),
                        ParserErrorKind::ExpectedTokenOf {
                            found: self.cursor.token().clone(),
                            options: vec![Token::Comma, Token::RightParen],
                        },
                    )?;
                }
            }
        }
        self.cursor.expect(Token::RightParen)?;

        // Return type, ': Int'
        let mut return_type = None;

        if self.cursor.match_token(Token::Colon) {
            self.cursor.expect(Token::Colon)?;
            return_type = Some(self.parse_type()?);
        }

        // Type constraints, 'where A: List<B>, B: Comparable<B>'
        let mut type_constraints = vec![];

        if self.cursor.match_keyword("where") {
            self.cursor.expect_keyword("where")?;

            loop {
                type_constraints.push(self.parse_type_constraint()?);
                if !self.cursor.match_token(Token::Comma) {
                    break;
                }
                self.cursor.expect(Token::Comma)?;
            }
        }

        // Function body block or expresion body
        let mut body = None;

        if self.cursor.match_token(Token::Equals) || self.cursor.match_token(Token::LeftBrace) {
            body = Some(self.parse_function_body()?);
        }

        Ok(Function {
            span: self.cursor.make_span(start),
            modifiers,
            type_parameters,
            receiver,
            name,
            value_parameters,
            return_type,
            type_constraints,
            body,
        })
    }

    fn parse_top_level_property(&mut self, start: BytePos, modifiers: Vec<Modifier>, mutable: bool) -> Result<Property, ParserError> {
        let start = self.cursor.start();

        if mutable {
            self.cursor.expect(Token::Var)?;
        } else {
            self.cursor.expect(Token::Val)?;
        }

        let type_parameters = self.parse_type_parameters()?;

        let (receiver, name) = self.parse_receiver_and_name()?;
        let mut variable_type = None;

        if self.cursor.match_token(Token::Colon) {
            self.cursor.expect(Token::Colon)?;
            variable_type = Some(self.parse_type()?);
        }

        let variable = VariableName { name, variable_type };
        let mut type_constraints = vec![];

        if self.cursor.match_keyword("where") {
            self.cursor.expect_keyword("where")?;

            loop {
                type_constraints.push(self.parse_type_constraint()?);
                if self.cursor.match_token(Token::Comma) {
                    self.cursor.expect(Token::Comma)?;
                } else {
                    break;
                }
            }
        }

        let mut initialization = PropertyInitialization::None;

        if self.cursor.match_token(Token::Equals) {
            self.cursor.expect(Token::Equals)?;

            initialization = PropertyInitialization::Expr(self.parse_expression()?);
        } else if self.cursor.match_keyword("by") {
            self.cursor.expect_keyword("by")?;

            initialization = PropertyInitialization::Delegation(self.parse_expression()?);
        }

        let mut getter = None;
        let mut setter = None;

        let mut prop_modifiers = self.parse_property_modifiers()?;

        if self.cursor.match_keyword("get") {
            getter = Some(self.parse_property_getter(prop_modifiers)?);

            prop_modifiers = self.parse_property_modifiers()?;

            if self.cursor.match_keyword("set") {
                setter = Some(self.parse_property_setter(prop_modifiers)?);
            }
        } else if self.cursor.match_keyword("set") {
            setter = Some(self.parse_property_setter(prop_modifiers)?);

            prop_modifiers = self.parse_property_modifiers()?;

            if self.cursor.match_keyword("get") {
                getter = Some(self.parse_property_getter(prop_modifiers)?);
            }
        }

        Ok(Property {
            span: self.cursor.make_span(start),
            modifiers,
            mutable,
            type_parameters,
            receiver,
            variable,
            type_constraints,
            initialization,
            getter,
            setter,
        })
    }

    fn parse_property_getter(&mut self, modifiers: Vec<Modifier>) -> Result<PropertyGetter, ParserError> {
        let start = self.cursor.start();
        self.cursor.expect_keyword("get")?;

        let mut getter_type = None;
        let mut body = None;

        if self.cursor.match_token(Token::LeftParen) {
            self.cursor.expect(Token::LeftParen)?;
            self.cursor.expect(Token::RightParen)?;

            if self.cursor.match_token(Token::Colon) {
                self.cursor.expect(Token::Colon)?;

                getter_type = Some(self.parse_type()?);
            }

            body = Some(self.parse_function_body()?);
        }

        Ok(PropertyGetter {
            span: self.cursor.make_span(start),
            modifiers,
            getter_type,
            body,
        })
    }

    fn parse_property_setter(&mut self, modifiers: Vec<Modifier>) -> Result<PropertySetter, ParserError> {
        let start = self.cursor.start();
        self.cursor.expect_keyword("set")?;

        let mut setter_type = None;
        let mut parameter = None;
        let mut body = None;

        if self.cursor.match_token(Token::LeftParen) {
            // (value: Int)
            self.cursor.expect(Token::LeftParen)?;
            parameter = Some(self.parse_setter_parameter()?);
            self.cursor.expect(Token::RightParen)?;

            if self.cursor.match_token(Token::Colon) {
                self.cursor.expect(Token::Colon)?;

                setter_type = Some(self.parse_type()?);
            }

            self.cursor.expect(Token::LeftBrace)?;
            body = Some(self.parse_block_statements()?);
            self.cursor.expect(Token::RightBrace)?;
        }

        Ok(PropertySetter {
            span: self.cursor.make_span(start),
            modifiers,
            setter_type,
            parameter,
            body,
        })
    }

    fn parse_typealias(&mut self, start: BytePos, modifiers: Vec<Modifier>) -> Result<TypeAlias, ParserError> {
        let start = self.cursor.start();

        self.cursor.expect(Token::TypeAlias)?;
        let name = self.cursor.expect_id()?;

        let type_parameters = self.parse_type_parameters()?;

        self.cursor.expect(Token::Equals)?;

        let aliased_type = self.parse_type()?;

        Ok(TypeAlias {
            span: self.cursor.make_span(start),
            modifiers,
            name,
            type_parameters,
            aliased_type,
        })
    }

    fn parse_type_parameters(&mut self) -> Result<Vec<TypeParameter>, ParserError> {
        let mut type_parameters = vec![];

        if self.cursor.match_token(Token::LeftAngleBracket) {
            self.cursor.expect(Token::LeftAngleBracket)?;
            loop {
                type_parameters.push(self.parse_type_parameter()?);

                if self.cursor.match_token(Token::Comma) {
                    self.cursor.expect(Token::Comma)?;
                } else {
                    break;
                }
            }
            self.cursor.expect(Token::RightAngleBracket)?;
        }

        Ok(type_parameters)
    }

    fn parse_type_parameter(&mut self) -> Result<TypeParameter, ParserError> {
        let start = self.cursor.start();
        let modifiers = self.parse_type_parameter_modifiers()?;
        let name = self.cursor.expect_id()?;

        let mut user_type = None;

        if self.cursor.match_token(Token::Colon) {
            self.cursor.expect(Token::Colon)?;
            user_type = Some(self.parse_type()?);
        }

        Ok(TypeParameter {
            modifiers,
            name,
            user_type,
        })
    }

    fn parse_type(&mut self) -> Result<Type, ParserError> {
        let start = self.cursor.start();

        let reference = if self.cursor.match_token(Token::LeftParen) {
            // Function type ()->Unit
            self.cursor.expect(Token::LeftParen)?;

            // (() -> Unit)
            let ty = if self.cursor.match_token(Token::LeftParen) {
                let mut ty = self.parse_type()?;
                self.cursor.expect(Token::RightParen)?;

                if self.cursor.match_token(Token::QuestionMark) {
                    self.cursor.expect(Token::QuestionMark)?;
                    match &mut ty.reference {
                        TypeReference::Function(t) => {
                            t.nullable = true;
                        }
                        TypeReference::SimpleType(t) => {
                            t.nullable = true;
                        }
                    }
                }

                ty
            } else {
                let mut params = vec![];

                if !self.cursor.match_token(Token::RightParen) {
                    loop {
                        params.push(self.parse_type()?);
                        if self.cursor.match_token(Token::Comma) {
                            self.cursor.expect(Token::Comma)?
                        } else {
                            break;
                        }
                    }
                }

                self.cursor.expect(Token::RightParen)?;

                if params.len() == 1 && !self.cursor.match_token(Token::RightArrow) {
                    // (Int)
                    let mut ty = params[0].clone();

                    if self.cursor.match_token(Token::QuestionMark) {
                        self.cursor.expect(Token::QuestionMark)?;
                        match &mut ty.reference {
                            TypeReference::Function(t) => {
                                t.nullable = true;
                            }
                            TypeReference::SimpleType(t) => {
                                t.nullable = true;
                            }
                        }
                    }

                    ty
                } else {
                    // (Int) -> Unit
                    self.cursor.expect(Token::RightArrow)?;
                    let ret = self.parse_type()?;

                    let func = TypeReference::Function(FunctionType {
                        nullable: false,
                        receiver: None,
                        parameters: params,
                        return_type: Box::new(ret),
                    });

                    Type {
                        span: self.cursor.make_span(start),
                        annotations: vec![],
                        reference: func,
                    }
                }
            };

            return Ok(ty);
        } else {
            TypeReference::SimpleType(self.parse_simple_type()?)
        };

        Ok(Type {
            span: self.cursor.make_span(start),
            annotations: vec![],
            reference,
        })
    }

    fn parse_simple_type(&mut self) -> Result<SimpleType, ParserError> {
        let mut package = vec![];
        let mut name = self.cursor.expect_id()?;
        let mut type_params = vec![];
        let mut nullable = false;

        // a.b.c.List
        while self.cursor.match_token(Token::Dot) {
            self.cursor.expect(Token::Dot)?;
            package.push(name);
            name = self.cursor.expect_id()?;
        }

        // Type arguments <T>
        if self.cursor.match_token(Token::LeftAngleBracket) {
            self.cursor.expect(Token::LeftAngleBracket)?;

            loop {
                if self.cursor.match_token(Token::Asterisk) {
                    self.cursor.expect(Token::Asterisk)?;
                    type_params.push(CallSiteTypeParams::Projection);
                } else {
                    type_params.push(CallSiteTypeParams::Type(self.parse_type()?));
                }

                if self.cursor.match_token(Token::Comma) {
                    self.cursor.expect(Token::Comma)?;
                } else {
                    break;
                }
            }
            self.cursor.expect(Token::RightAngleBracket)?;
        }

        // Nullable ?
        if self.cursor.match_token(Token::QuestionMark) {
            self.cursor.expect(Token::QuestionMark)?;
            nullable = true;
        }

        Ok(SimpleType {
            nullable,
            name,
            package,
            type_params,
        })
    }

    fn parse_receiver_and_name(&mut self) -> Result<(Option<Type>, String), ParserError> {
        let start = self.cursor.start();

        // Receiver in parens (Int).sum or (()->Unit).sum
        if self.cursor.match_token(Token::LeftParen) {
            let receiver = Some(self.parse_type()?);
            self.cursor.expect(Token::Dot)?;
            let name = self.cursor.expect_id()?;

            return Ok((receiver, name));
        }

        // Normal type
        let mut receiver = false;
        let mut package = vec![];
        let mut name = self.cursor.expect_id()?;
        let mut type_params = vec![];
        let mut nullable = false;

        // a.b.c.List
        while self.cursor.match_token(Token::Dot) {
            receiver = true;
            self.cursor.expect(Token::Dot)?;
            package.push(name);
            name = self.cursor.expect_id()?;
        }

        // Type arguments <T>
        if self.cursor.match_token(Token::LeftAngleBracket) {
            receiver = true;
            self.cursor.expect(Token::LeftAngleBracket)?;

            loop {
                if self.cursor.match_token(Token::Asterisk) {
                    self.cursor.expect(Token::Asterisk)?;
                    type_params.push(CallSiteTypeParams::Projection);
                } else {
                    type_params.push(CallSiteTypeParams::Type(self.parse_type()?));
                }

                if self.cursor.match_token(Token::Comma) {
                    self.cursor.expect(Token::Comma)?;
                } else {
                    break;
                }
            }
            self.cursor.expect(Token::RightAngleBracket)?;
        }

        // Nullable ?
        if self.cursor.match_token(Token::QuestionMark) {
            receiver = true;
            self.cursor.expect(Token::QuestionMark)?;
            nullable = true;
        }

        if receiver {
            let ty: SimpleType;

            if nullable || !type_params.is_empty() {
                // a.b<*>.c
                //       |
                // ------=

                ty = SimpleType {
                    nullable,
                    name,
                    package,
                    type_params,
                };

                self.cursor.expect(Token::Dot)?;
                name = self.cursor.expect_id()?;
            } else {
                // a.b.c
                //     |
                // ----=

                ty = SimpleType {
                    nullable,
                    name: package.last().unwrap().clone(),
                    package: package.iter().take(package.len() - 1).cloned().collect(),
                    type_params,
                }
            }

            Ok((
                Some(Type {
                    span: self.cursor.make_span(start),
                    annotations: vec![],
                    reference: TypeReference::SimpleType(ty),
                }),
                name
            ))
        } else {
            Ok((None, name))
        }
    }

    fn parse_function_parameter(&mut self) -> Result<FunctionParameter, ParserError> {
        let start = self.cursor.start();
        let modifiers = self.parse_function_parameter_modifiers()?;
        let name = self.cursor.expect_id()?;

        self.cursor.expect(Token::Colon)?;

        let parameter_type = self.parse_type()?;

        let mut default_value = None;

        if self.cursor.match_token(Token::Equals) {
            default_value = Some(self.parse_expression()?);
        }

        Ok(FunctionParameter { modifiers, name, parameter_type, default_value })
    }

    fn parse_setter_parameter(&mut self) -> Result<SetterParameter, ParserError> {
        let start = self.cursor.start();
        let modifiers = self.parse_function_parameter_modifiers()?;
        let name = self.cursor.expect_id()?;
        let mut parameter_type = None;

        if self.cursor.match_token(Token::Colon) {
            self.cursor.expect(Token::Colon)?;

            parameter_type = Some(self.parse_type()?);
        }

        Ok(SetterParameter { modifiers, name, parameter_type })
    }

    fn parse_type_parameter_modifiers(&mut self) -> Result<Vec<Modifier>, ParserError> {
        let mut vec = vec![];

        loop {
            let modifier = match self.cursor.token() {
                Token::In => Modifier::In,
                Token::Id(id) if id == "out" => Modifier::Out,
                Token::Id(id) if id == "reified" => Modifier::Reified,
                _ => {
                    break;
                }
            };

            self.cursor.next()?;
            vec.push(modifier);
        }

        Ok(vec)
    }

    fn parse_property_modifiers(&mut self) -> Result<Vec<Modifier>, ParserError> {
        let mut vec = vec![];

        loop {
            let modifier = match self.cursor.token() {
                Token::Id(id) if id == "public" => Modifier::Public,
                Token::Id(id) if id == "private" => Modifier::Private,
                Token::Id(id) if id == "protected" => Modifier::Protected,
                Token::Id(id) if id == "internal" => Modifier::Internal,
                Token::Id(id) if id == "inline" => Modifier::Inline,
                _ => {
                    break;
                }
            };

            self.cursor.next()?;
            vec.push(modifier);
        }

        Ok(vec)
    }

    fn parse_function_parameter_modifiers(&mut self) -> Result<Vec<Modifier>, ParserError> {
        let mut vec = vec![];

        loop {
            let modifier = match self.cursor.token() {
                Token::Id(id) if id == "noinline" => Modifier::Noinline,
                Token::Id(id) if id == "crossinline" => Modifier::Crossinline,
                Token::Id(id) if id == "vararg" => Modifier::Vararg,
                _ => {
                    break;
                }
            };

            self.cursor.next()?;
            vec.push(modifier);
        }

        Ok(vec)
    }

    fn parse_type_constraint(&mut self) -> Result<TypeConstraint, ParserError> {
        // T: List<B>
        let name = self.cursor.expect_id()?;
        self.cursor.expect(Token::Colon)?;
        let ty = self.parse_type()?;

        Ok(TypeConstraint {
            annotations: vec![],
            name,
            ty,
        })
    }

    fn parse_expression(&mut self) -> Result<Expression, ParserError> {
        self.parse_expr_disjunction()
    }

    fn parse_expr_disjunction(&mut self) -> Result<Expression, ParserError> {
        self.parse_chain(Self::parse_expr_conjunction, |tk| {
            if tk == &Token::DoubleAmpersand { Some("&&".to_string()) } else { None }
        }, true)
    }

    fn parse_expr_conjunction(&mut self) -> Result<Expression, ParserError> {
        self.parse_chain(Self::parse_expr_equality_comparison, |tk| {
            if tk == &Token::DoublePipe { Some("||".to_string()) } else { None }
        }, true)
    }

    fn parse_expr_equality_comparison(&mut self) -> Result<Expression, ParserError> {
        self.parse_chain_same_line(Self::parse_expr_comparison, |tk| {
            match tk {
                Token::DoubleEquals => Some("==".to_string()),
                Token::TripleEquals => Some("===".to_string()),
                Token::NotEquals => Some("!=".to_string()),
                Token::NotDoubleEquals => Some("!==".to_string()),
                _ => None
            }
        }, true)
    }

    fn parse_expr_comparison(&mut self) -> Result<Expression, ParserError> {
        self.parse_chain_same_line(Self::parse_expr_in, |tk| {
            match tk {
                Token::LeftAngleBracket => Some("<".to_string()),
                Token::LessEquals => Some("<=".to_string()),
                Token::RightAngleBracket => Some(">".to_string()),
                Token::GreaterEquals => Some(">=".to_string()),
                _ => None
            }
        }, true)
    }

    fn parse_expr_in(&mut self) -> Result<Expression, ParserError> {
        self.parse_chain_same_line(Self::parse_expr_is, |tk| {
            match tk {
                Token::In => Some("in".to_string()),
                Token::NotIn => Some("!in".to_string()),
                _ => None
            }
        }, true)
    }

    fn parse_expr_is(&mut self) -> Result<Expression, ParserError> {
        let start = self.cursor.start();
        let expr = self.parse_expr_elvis()?;

        if !self.cursor.at_newline()
            && (self.cursor.match_token(Token::Is) || self.cursor.match_token(Token::NotIs))
        {
            let negated = self.cursor.match_token(Token::NotIs);
            self.cursor.next()?;
            let is_type = self.parse_type()?;

            return Ok(Expression {
                span: self.cursor.make_span(start),
                kind: Expr::Is {
                    expr: Box::new(expr),
                    is_type,
                    negated,
                },
            });
        }

        Ok(expr)
    }

    fn parse_expr_elvis(&mut self) -> Result<Expression, ParserError> {
        self.parse_chain(Self::parse_expr_infix_function, |tk| {
            match tk {
                Token::Elvis => Some("?:".to_string()),
                _ => None
            }
        }, true)
    }

    fn parse_expr_infix_function(&mut self) -> Result<Expression, ParserError> {
        self.parse_chain_same_line(Self::parse_expr_range, |tk| {
            match tk {
                Token::Id(id) => Some(id.to_string()),
                _ => None
            }
        }, true)
    }

    fn parse_expr_range(&mut self) -> Result<Expression, ParserError> {
        self.parse_chain_same_line(Self::parse_expr_addition, |tk| {
            match tk {
                Token::DoubleDot => Some("..".to_string()),
                _ => None
            }
        }, true)
    }

    fn parse_expr_addition(&mut self) -> Result<Expression, ParserError> {
        self.parse_chain(Self::parse_expr_multiply, |tk| {
            match tk {
                Token::Plus => Some("+".to_string()),
                Token::Minus => Some("-".to_string()),
                _ => None
            }
        }, true)
    }

    fn parse_expr_multiply(&mut self) -> Result<Expression, ParserError> {
        self.parse_chain_same_line(Self::parse_expr_type_rhs, |tk| {
            match tk {
                Token::Asterisk => Some("*".to_string()),
                Token::Slash => Some("/".to_string()),
                Token::Percent => Some("%".to_string()),
                _ => None
            }
        }, true)
    }

    fn parse_expr_type_rhs(&mut self) -> Result<Expression, ParserError> {
        self.parse_chain(Self::parse_expr_prefix_unary, |tk| {
            match tk {
                Token::As => Some("as".to_string()),
                Token::AsQuestionMark => Some("as?".to_string()),
                _ => None
            }
        }, true)
    }

    fn parse_expr_prefix_unary(&mut self) -> Result<Expression, ParserError> {
        let start = self.cursor.start();
        let mut unary_operators = vec![];

        loop {
            let op = match self.cursor.token() {
                Token::Minus => "-",
                Token::Plus => "+",
                Token::DoublePlus => "++",
                Token::DoubleMinus => "--",
                Token::ExclamationMark => "!",
                _ => break
            };

            self.cursor.next()?;
            unary_operators.push(op.to_string());
        }

        let expr = self.parse_expr_with_suffix()?;

        if unary_operators.is_empty() {
            return Ok(expr);
        }

        Ok(Expression {
            span: self.cursor.make_span(start),
            kind: Expr::UnaryOperator {
                unary_operators,
                expr: Box::new(expr),
            },
        })
    }

    fn parse_expr_with_suffix(&mut self) -> Result<Expression, ParserError> {
        if self.cursor.match_token(Token::DoubleColon) {
            return self.parse_expr_callable_ref(None);
        }

        let expr = self.parse_expr_primary()?;
        self.parse_suffix_of_expr(expr)
    }

    fn parse_expr_callable_ref(&mut self, expr: Option<Expression>) -> Result<Expression, ParserError> {
        let start = self.cursor.start();
        self.cursor.expect(Token::DoubleColon)?;

        let name = if self.cursor.match_token(Token::Class) {
            "class".to_string()
        } else {
            self.cursor.expect_id()?
        };

        Ok(Expression {
            span: self.cursor.make_span(start),
            kind: Expr::ExprCallableRef {
                receiver_expr: expr.map(|i| Box::new(i)),
                name,
            },
        })
    }

    fn parse_suffix_of_expr(&mut self, mut expr: Expression) -> Result<Expression, ParserError> {
        loop {
            let suffix = match self.cursor.token() {
                Token::DoublePlus if !self.cursor.at_newline() => {
                    self.cursor.expect(Token::DoublePlus)?;
                    ExprSuffix::Increment
                }
                Token::DoubleMinus if !self.cursor.at_newline() => {
                    self.cursor.expect(Token::DoubleMinus)?;
                    ExprSuffix::Decrement
                }
                Token::DoubleExclamationMark if !self.cursor.at_newline() => {
                    self.cursor.expect(Token::DoubleExclamationMark)?;
                    ExprSuffix::AssertNonNull
                }
                Token::LeftParen | Token::LeftAngleBracket  if !self.cursor.at_newline() => {
                    return self.parse_function_call_suffix(expr);
                }
                Token::LeftBrace if !self.cursor.at_newline() && !self.parsing_delegated_by => {
                    return self.parse_function_call_suffix(expr);
                }
                Token::Dot => {
                    return self.parse_method_suffix(expr);
                }
                Token::DoubleColon => {
                    expr = self.parse_expr_callable_ref(Some(expr))?;
                    continue
                }
                Token::LeftBracket => {
                    let mut indices = vec![];
                    self.cursor.expect(Token::LeftBracket)?;
                    loop {
                        indices.push(self.parse_expression()?);
                        if self.cursor.match_token(Token::Comma) {
                            self.cursor.expect(Token::Comma)?;
                        } else {
                            break;
                        }
                    }
                    self.cursor.expect(Token::RightBracket)?;
                    ExprSuffix::ArrayAccess(indices)
                }
                Token::QuestionMark if self.cursor.offset_token(1) == &Token::Dot => {
                    return self.parse_method_suffix(expr);
                }
                _ => {
                    return Ok(expr);
                }
            };

            expr = Expression {
                span: self.cursor.make_span(expr.span.start()),
                kind: Expr::Suffix {
                    expr: Box::new(expr),
                    suffix,
                },
            };
        }
    }

    fn parse_function_call_suffix(&mut self, prev: Expression) -> Result<Expression, ParserError> {
        let mut type_arguments = vec![];

        // <T>
        if self.cursor.match_token(Token::LeftAngleBracket) {
            self.cursor.expect(Token::LeftAngleBracket)?;
            loop {
                type_arguments.push(self.parse_type()?);
                if self.cursor.match_token(Token::Comma) {
                    self.cursor.expect(Token::Comma)?;
                } else {
                    break;
                }
            }
            self.cursor.expect(Token::RightAngleBracket)?;
        }

        let mut value_arguments = vec![];
        let mut lambda = None;

        if self.cursor.match_token(Token::LeftParen) {
            // ()
            self.cursor.expect(Token::LeftParen)?;

            while !self.cursor.match_token(Token::RightParen) {
                value_arguments.push(self.parse_value_argument()?);

                if self.cursor.match_token(Token::Comma) {
                    self.cursor.expect(Token::Comma)?;
                } else {
                    break;
                }
            }
            self.cursor.expect(Token::RightParen)?;

            // {}
            if !self.parsing_delegated_by &&
                self.cursor.match_token(Token::LeftBrace)
            {
                lambda = Some(self.parse_function_literal()?);
            }
        } else {
            // {}
            if !self.parsing_delegated_by {
                lambda = Some(self.parse_function_literal()?);
            }
        }

        let expr = Expression {
            span: self.cursor.make_span(prev.span.start()),
            kind: Expr::FunctionCall {
                function: Box::new(prev),
                type_arguments,
                value_arguments,
                lambda,
            },
        };

        self.parse_suffix_of_expr(expr)
    }

    fn parse_method_suffix(&mut self, prev: Expression) -> Result<Expression, ParserError> {
        let start = self.cursor.start();
        let mut safe_call = false;

        // ?.value
        if self.cursor.match_token(Token::QuestionMark) {
            self.cursor.expect(Token::QuestionMark)?;
            safe_call = true;
        }
        // .value
        self.cursor.expect(Token::Dot)?;
        let name = self.cursor.expect_id()?;

        let mut expr = Expression {
            span: self.cursor.make_span(start),
            kind: Expr::PropertyAccess {
                object: Box::new(prev),
                property: name,
                safe_call,
            },
        };

        if self.cursor.match_token(Token::LeftAngleBracket) || self.cursor.match_token(Token::LeftParen) || self.cursor.match_token(Token::LeftBrace) {
            expr = self.parse_function_call_suffix(expr)?;
        }

        Ok(expr)
    }

    fn parse_expr_primary(&mut self) -> Result<Expression, ParserError> {
        let start = self.cursor.start();

        match self.cursor.token().clone() {
            Token::LeftParen => {
                self.cursor.expect(Token::LeftParen)?;
                let e = self.parse_expression()?;
                self.cursor.expect(Token::RightParen)?;
                Ok(e)
            }
            Token::LeftBrace => self.parse_expr_lambda(),
            Token::Id(name) => {
                self.cursor.next()?;

                // if self.cursor.match_token(Token::DoubleColon) {
                //     self.cursor.expect(Token::DoubleColon)?;
                //
                //     let method = self.cursor.expect_id()?;
                //
                //     Ok(Expression {
                //         span: self.cursor.make_span(start),
                //         kind: Expr::CallableRef{
                //
                //         },
                //     })
                // }

                Ok(Expression {
                    span: self.cursor.make_span(start),
                    kind: Expr::Ref(name),
                })
            }
            Token::True => {
                self.cursor.expect(Token::True)?;
                Ok(Expression {
                    span: self.cursor.make_span(start),
                    kind: Expr::Boolean(true),
                })
            }
            Token::False => {
                self.cursor.expect(Token::False)?;
                Ok(Expression {
                    span: self.cursor.make_span(start),
                    kind: Expr::Boolean(false),
                })
            }
            Token::Null => {
                self.cursor.expect(Token::Null)?;
                Ok(Expression {
                    span: self.cursor.make_span(start),
                    kind: Expr::Null,
                })
            }
            Token::This => {
                self.cursor.expect(Token::This)?;

                // Ignore 'this@Class' label
                if self.cursor.match_token(Token::At) {
                    self.cursor.expect(Token::At)?;
                    // Ignored for now
                    self.cursor.expect_id()?;
                }

                Ok(Expression {
                    span: self.cursor.make_span(start),
                    kind: Expr::This,
                })
            }
            Token::Super => {
                self.cursor.expect(Token::Super)?;

                // Ignore 'super@Class' label
                if self.cursor.match_token(Token::At) {
                    self.cursor.expect(Token::At)?;
                    // Ignored for now
                    self.cursor.expect_id()?;
                }

                Ok(Expression {
                    span: self.cursor.make_span(start),
                    kind: Expr::Super,
                })
            }
            Token::StringStart => self.parse_expr_string(),
            Token::Char(single_char) => {
                self.cursor.next()?;
                Ok(Expression {
                    span: self.cursor.make_span(start),
                    kind: Expr::Char(single_char),
                })
            }
            Token::Number(lit) => {
                self.cursor.next()?;
                Ok(Expression {
                    span: self.cursor.make_span(start),
                    kind: Expr::Number(lit),
                })
            }
            Token::If => self.parse_expr_if(),
            Token::Try => self.parse_expr_try(),
            Token::When => self.parse_expr_when(),
            Token::Object => self.parse_object_literal(),
            Token::Throw => {
                self.cursor.expect(Token::Throw)?;
                let expr = self.parse_expression()?;

                Ok(Expression {
                    span: self.cursor.make_span(start),
                    kind: Expr::Throw(Box::new(expr)),
                })
            }
            Token::Return => {
                self.cursor.expect(Token::Return)?;

                // Ignore 'super@Class' label
                if self.cursor.match_token(Token::At) {
                    self.cursor.expect(Token::At)?;
                    // Ignored for now
                    self.cursor.expect_id()?;
                }

                if self.cursor.at_newline()
                    || self.cursor.match_token(Token::RightParen)
                    || self.cursor.match_token(Token::RightBrace)
                    || self.cursor.match_token(Token::Semicolon)
                {
                    Ok(Expression {
                        span: self.cursor.make_span(start),
                        kind: Expr::Return(None),
                    })
                } else {
                    let expr = self.parse_expression()?;

                    Ok(Expression {
                        span: self.cursor.make_span(start),
                        kind: Expr::Return(Some(Box::new(expr))),
                    })
                }
            }
            Token::Continue => {
                self.cursor.expect(Token::Continue)?;

                // Ignore 'continue@Class' label
                if self.cursor.match_token(Token::At) {
                    self.cursor.expect(Token::At)?;
                    // Ignored for now
                    self.cursor.expect_id()?;
                }

                Ok(Expression {
                    span: self.cursor.make_span(start),
                    kind: Expr::Continue,
                })
            }
            Token::Break => {
                self.cursor.expect(Token::Break)?;

                // Ignore 'break@Class' label
                if self.cursor.match_token(Token::At) {
                    self.cursor.expect(Token::At)?;
                    // Ignored for now
                    self.cursor.expect_id()?;
                }

                Ok(Expression {
                    span: self.cursor.make_span(start),
                    kind: Expr::Break,
                })
            }
            tk => {
                Err(ParserError {
                    span: self.cursor.make_source_span(start),
                    kind: ParserErrorKind::ExpectedTokenOf {
                        found: tk,
                        options: vec![
                            Token::LeftParen,
                            Token::LeftBrace,
                            Token::True,
                            Token::False,
                            Token::Null,
                            Token::This,
                            Token::Super,
                            Token::StringStart,
                            Token::If,
                            Token::Try,
                            Token::When,
                            Token::Object,
                            Token::Throw,
                            Token::Return,
                            Token::Continue,
                            Token::Break
                        ],
                    },
                })
            }
        }
    }

    fn parse_expr_string(&mut self) -> Result<Expression, ParserError> {
        let start = self.cursor.start();
        let mut components = vec![];

        self.cursor.expect(Token::StringStart)?;
        loop {
            match self.cursor.token().clone() {
                Token::StringContent(it) => {
                    self.cursor.next()?;
                    components.push(StringComponent::Content(it));
                }
                Token::StringVariable(it) => {
                    self.cursor.next()?;
                    components.push(StringComponent::Variable(it));
                }
                Token::StringTemplateStart => {
                    self.cursor.next()?;
                    let expr = self.parse_expression()?;
                    self.cursor.expect(Token::StringTemplateEnd)?;
                    components.push(StringComponent::Template(expr));
                }
                Token::StringEnd => {
                    break;
                }
                tk => {
                    return Err(ParserError {
                        span: self.cursor.make_source_span(start),
                        kind: ParserErrorKind::ExpectedTokenOf {
                            found: tk,
                            options: vec![
                                Token::StringContent("The content of a string".into()),
                                Token::StringVariable("A variable template like $a".into()),
                                Token::StringTemplateStart,
                                Token::StringEnd
                            ],
                        },
                    });
                }
            }
        }
        self.cursor.expect(Token::StringEnd)?;

        Ok(Expression {
            span: self.cursor.make_span(start),
            kind: Expr::String(components),
        })
    }

    fn parse_expr_lambda(&mut self) -> Result<Expression, ParserError> {
        let start = self.cursor.start();
        let literal = self.parse_function_literal()?;

        Ok(Expression {
            span: self.cursor.make_span(start),
            kind: Expr::Lambda(literal),
        })
    }

    fn parse_function_literal(&mut self) -> Result<FunctionLiteral, ParserError> {
        let mut parameters = vec![];

        self.cursor.expect(Token::LeftBrace)?;

        if !self.cursor.match_token(Token::RightBrace) && (
            self.cursor.offset_token(1) == &Token::Colon
                || self.cursor.offset_token(1) == &Token::Comma
                || self.cursor.offset_token(1) == &Token::RightArrow)
        {
            loop {
                let var = self.parse_variable_name()?;

                parameters.push(var);

                if self.cursor.match_token(Token::Comma) {
                    self.cursor.expect(Token::Comma)?;
                } else {
                    break;
                }
            }

            self.cursor.expect(Token::RightArrow)?;
        }

        let body = self.parse_block_statements()?;

        self.cursor.expect(Token::RightBrace)?;

        Ok(FunctionLiteral {
            annotations: vec![],
            parameters,
            body,
        })
    }

    fn parse_expr_if(&mut self) -> Result<Expression, ParserError> {
        let start = self.cursor.start();

        self.cursor.expect(Token::If)?;
        self.cursor.expect(Token::LeftParen)?;
        let cond = self.parse_expression()?;
        self.cursor.expect(Token::RightParen)?;
        let if_true = self.parse_statement_body()?;
        let mut if_false = None;

        if self.cursor.match_token(Token::Else) {
            self.cursor.expect(Token::Else)?;
            if_false = Some(self.parse_statement_body()?);
        }

        Ok(Expression {
            span: self.cursor.make_span(start),
            kind: Expr::If {
                cond: Box::new(cond),
                if_true: Box::new(if_true),
                if_false: if_false.map(|i| Box::new(i)),
            },
        })
    }

    fn parse_expr_try(&mut self) -> Result<Expression, ParserError> {
        let start = self.cursor.start();

        self.cursor.expect(Token::Try)?;
        let block = self.parse_statement_body()?;

        let mut catch_blocks = vec![];

        loop {
            self.cursor.expect_keyword("catch")?;
            self.cursor.expect(Token::LeftParen)?;
            let variable = self.cursor.expect_id()?;
            self.cursor.expect(Token::Colon)?;
            let exception_type = self.parse_type()?;
            self.cursor.expect(Token::RightParen)?;
            let catch = self.parse_statement_body()?;

            catch_blocks.push(CatchBlock {
                annotations: vec![],
                variable,
                exception_type,
                block: catch,
            });

            if !self.cursor.match_keyword("catch") {
                break;
            }
        }

        let mut finally = None;

        if self.cursor.match_keyword("finally") {
            self.cursor.expect_keyword("finally")?;
            finally = Some(self.parse_statement_body()?);
        }

        Ok(Expression {
            span: self.cursor.make_span(start),
            kind: Expr::Try {
                block: Box::new(block),
                catch_blocks,
                finally: finally.map(|i| Box::new(i)),
            },
        })
    }

    fn parse_expr_when(&mut self) -> Result<Expression, ParserError> {
        let start = self.cursor.start();

        self.cursor.expect(Token::When)?;

        let mut expr = None;

        if self.cursor.match_token(Token::LeftParen) {
            self.cursor.expect(Token::LeftParen)?;
            expr = Some(Box::new(self.parse_expression()?));
            self.cursor.expect(Token::RightParen)?;
        }

        let mut entries = vec![];

        self.cursor.expect(Token::LeftBrace)?;
        while !self.cursor.match_token(Token::RightBrace) {
            match self.parse_when_entry() {
                Ok(e) => entries.push(e),
                Err(err) => {
                    self.errors.push(err);

                    self.skip_to_next_statement()?;
                }
            }
        }
        self.cursor.expect(Token::RightBrace)?;

        Ok(Expression {
            span: self.cursor.make_span(start),
            kind: Expr::When {
                expr,
                entries,
            },
        })
    }

    fn parse_when_entry(&mut self) -> Result<WhenEntry, ParserError> {
        let mut conditions = vec![];

        loop {
            if self.cursor.match_token(Token::Else) {
                self.cursor.expect(Token::Else)?;
                conditions.push(WhenCondition::Else);
                break;
                //
            } else if self.cursor.match_token(Token::In) || self.cursor.match_token(Token::NotIn) {
                let negated = self.cursor.match_token(Token::NotIn);
                self.cursor.next()?;
                let expr = self.parse_expression()?;

                conditions.push(WhenCondition::In { negated, expr });
                //
            } else if self.cursor.match_token(Token::Is) || self.cursor.match_token(Token::NotIs) {
                let negated = self.cursor.match_token(Token::NotIs);
                self.cursor.next()?;
                let ty = self.parse_type()?;

                conditions.push(WhenCondition::Is { negated, is_type: ty });
                //
            } else {
                let expr = self.parse_expression()?;

                conditions.push(WhenCondition::Expr(expr));
            }

            if self.cursor.match_token(Token::Comma) {
                self.cursor.expect(Token::Comma)?;
            } else {
                break;
            }
        }

        self.cursor.expect(Token::RightArrow)?;

        let body = self.parse_statement_body()?;

        Ok(WhenEntry {
            conditions,
            body,
        })
    }

    fn parse_object_literal(&mut self) -> Result<Expression, ParserError> {
        let start = self.cursor.start();
        let mut delegation_specifiers = vec![];

        self.cursor.expect(Token::Object)?;

        if self.cursor.match_token(Token::Colon) {
            self.cursor.expect(Token::Colon)?;

            loop {
                delegation_specifiers.push(self.parse_delegation_specifier()?);

                if self.cursor.match_token(Token::Comma) {
                    self.cursor.expect(Token::Comma)?;
                } else {
                    break;
                }
            }
        }

        let body = self.parse_class_body()?;

        Ok(Expression {
            span: self.cursor.make_span(start),
            kind: Expr::Object {
                delegation_specifiers,
                body,
            },
        })
    }

    fn parse_chain<F, P>(&mut self, parse_func: F, is_operator: P, left_associativity: bool) -> Result<Expression, ParserError>
        where P: Fn(&Token) -> Option<String>,
              F: Fn(&mut Parser) -> Result<Expression, ParserError>
    {
        let mut operands = vec![parse_func(self)?];
        let mut operators = vec![];

        while let Some(op) = is_operator(self.cursor.token()) {
            operators.push(op);
            // DEBUG
            // self.cursor.expect(self.cursor.token().clone())?;
            self.cursor.next()?;
            operands.push(parse_func(self)?);
        }

        Ok(Self::create_tree(operands, operators, left_associativity))
    }

    fn parse_chain_same_line<F, P>(&mut self, parse_func: F, is_operator: P, left_associativity: bool) -> Result<Expression, ParserError>
        where P: Fn(&Token) -> Option<String>,
              F: Fn(&mut Parser) -> Result<Expression, ParserError>
    {
        let mut operands = vec![parse_func(self)?];
        let mut operators = vec![];

        loop {
            if self.cursor.at_newline() {
                break;
            }

            if let Some(op) = is_operator(self.cursor.token()) {
                operators.push(op);
                self.cursor.next()?;
                operands.push(parse_func(self)?);
            } else {
                break;
            }
        }

        Ok(Self::create_tree(operands, operators, left_associativity))
    }

    fn create_tree(operands: Vec<Expression>, operators: Vec<String>, left_associativity: bool) -> Expression {
        if operands.len() == 1 {
            return operands.into_iter().next().unwrap();
        }

        let mut iter = operands.into_iter();
        let mut tree = iter.next().unwrap();

        // TODO check associativity of every subexpression
        if left_associativity {
            // left_associativity = true
            // 1 + 2 + 3
            // =>
            // ((1) + 2) + 3
            for operator in operators {
                let right = iter.next().unwrap();

                tree = Expression {
                    span: ByteSpan::from(tree.span.start(), right.span.end()),
                    kind: Expr::Tree {
                        left: Box::new(tree),
                        right: Box::new(right),
                        operator,
                    },
                }
            }
        } else {
            // left_associativity = false
            // 1 + 2 + 3
            // =>
            // 1 + (2 + (3))
            let mut iter = iter.rev();

            for operator in operators.into_iter().rev() {
                let left = iter.next().unwrap();

                tree = Expression {
                    span: ByteSpan::from(tree.span.start(), left.span.end()),
                    kind: Expr::Tree {
                        left: Box::new(left),
                        right: Box::new(tree),
                        operator,
                    },
                }
            }
        }

        tree
    }

    fn parse_block_statements(&mut self) -> Result<StatementBlock, ParserError> {
        let start = self.cursor.start();
        let mut statements = vec![];

        while !self.cursor.match_token(Token::RightBrace) {
            match self.parse_statement() {
                Ok(stm) => {
                    statements.push(stm);
                }
                Err(err) => {
                    #[cfg(test)] eprintln!("{}", err);
                    self.errors.push(err);

                    if let Err(e) = self.skip_to_next_statement() {
                        #[cfg(test)] eprintln!("{}", e);
                        self.errors.push(e);
                    }
                }
            }

            while self.cursor.match_token(Token::Semicolon) {
                self.cursor.expect(Token::Semicolon)?;
            }
        }

        Ok(StatementBlock {
            span: self.cursor.make_span(start),
            statements,
        })
    }

    fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        let stm = match self.cursor.token() {
            Token::Val | Token::Var => Statement::Variable(self.parse_variable()?),
            Token::For => Statement::For(self.parse_for()?),
            Token::While => Statement::While(self.parse_while()?),
            Token::Do => Statement::DoWhile(self.parse_do_while()?),
            Token::Fun => Statement::Function(self.parse_function(self.cursor.start(), vec![])?),
            Token::Class => Statement::Class(self.parse_class(self.cursor.start(), vec![])?),
            _ => {
                let expr = self.parse_expression()?;

                let op = match self.cursor.token() {
                    Token::Equals => Some("="),
                    Token::PlusEquals => Some("+="),
                    Token::MinusEquals => Some("-="),
                    Token::TimesEquals => Some("*="),
                    Token::DivEquals => Some("/="),
                    Token::ModEquals => Some("%="),
                    _ => None
                };

                if let Some(op) = op {
                    self.cursor.next()?;
                    let value = self.parse_expression()?;

                    Statement::Assignment {
                        left: expr,
                        right: value,
                        operator: op.to_string(),
                    }
                } else {
                    Statement::Expression(expr)
                }
            }
        };

        Ok(stm)
    }

    fn parse_variable(&mut self) -> Result<Variable, ParserError> {
        let start = self.cursor.start();
        let mut mutable = false;

        if self.cursor.match_token(Token::Val) {
            self.cursor.expect(Token::Val)?;
            mutable = true;
        } else {
            self.cursor.expect(Token::Var)?;
        }

        let variable = self.parse_variable_pattern()?;

        let mut initialization = PropertyInitialization::None;

        if self.cursor.match_token(Token::Equals) {
            self.cursor.expect(Token::Equals)?;
            initialization = PropertyInitialization::Expr(self.parse_expression()?);
        } else if self.cursor.match_keyword("by") {
            self.cursor.expect_keyword("by")?;
            initialization = PropertyInitialization::Delegation(self.parse_expression()?);
        }

        Ok(Variable {
            span: self.cursor.make_span(start),
            mutable,
            variable,
            initialization,
        })
    }

    fn parse_variable_pattern(&mut self) -> Result<VariablePattern, ParserError> {
        if self.cursor.match_token(Token::LeftParen) {
            let mut names = vec![];
            self.cursor.expect(Token::LeftParen)?;
            loop {
                names.push(self.parse_variable_name()?);
                if self.cursor.match_token(Token::RightParen) {
                    self.cursor.expect(Token::RightParen)?;
                    break;
                }
                self.cursor.expect(Token::Comma)?;
            }

            Ok(VariablePattern::Tuple(names))
        } else {
            Ok(VariablePattern::Single(self.parse_variable_name()?))
        }
    }

    fn parse_variable_name(&mut self) -> Result<VariableName, ParserError> {
        let name = self.cursor.expect_id()?;
        let mut variable_type = None;

        if self.cursor.match_token(Token::Colon) {
            self.cursor.expect(Token::Colon)?;
            variable_type = Some(self.parse_type()?);
        }

        Ok(VariableName {
            name,
            variable_type,
        })
    }

    fn parse_for(&mut self) -> Result<ForStatement, ParserError> {
        let start = self.cursor.start();

        // for(var in iterable) { }
        self.cursor.expect(Token::For)?;
        self.cursor.expect(Token::LeftParen)?;
        let variable = self.parse_variable_pattern()?;
        self.cursor.expect(Token::In)?;
        let iterable = self.parse_expression()?;
        self.cursor.expect(Token::RightParen)?;

        let body = self.parse_statement_body()?;

        Ok(ForStatement {
            span: self.cursor.make_span(start),
            annotations: vec![],
            variable,
            iterable,
            body,
        })
    }

    fn parse_while(&mut self) -> Result<WhileStatement, ParserError> {
        let start = self.cursor.start();

        // while(cond) { }
        self.cursor.expect(Token::While)?;
        self.cursor.expect(Token::LeftParen)?;
        let condition = self.parse_expression()?;
        self.cursor.expect(Token::RightParen)?;

        let body = self.parse_statement_body()?;

        Ok(WhileStatement {
            span: self.cursor.make_span(start),
            condition,
            body,
        })
    }

    fn parse_do_while(&mut self) -> Result<DoWhileStatement, ParserError> {
        let start = self.cursor.start();

        // do { } while(cond)
        self.cursor.expect(Token::Do)?;
        let body = self.parse_statement_body()?;
        self.cursor.expect(Token::While)?;
        self.cursor.expect(Token::LeftParen)?;
        let condition = self.parse_expression()?;
        self.cursor.expect(Token::RightParen)?;

        Ok(DoWhileStatement {
            span: self.cursor.make_span(start),
            condition,
            body,
        })
    }

    fn parse_statement_body(&mut self) -> Result<FunctionBody, ParserError> {
        if self.cursor.match_token(Token::LeftBrace) {
            self.cursor.expect(Token::LeftBrace)?;
            let block = self.parse_block_statements()?;
            self.cursor.expect(Token::RightBrace)?;

            Ok(FunctionBody::Block(block))
        } else {
            Ok(FunctionBody::Expression(self.parse_expression()?))
        }
    }

    fn parse_function_body(&mut self) -> Result<FunctionBody, ParserError> {
        if self.cursor.match_token(Token::Equals) {
            self.cursor.expect(Token::Equals)?;
            Ok(FunctionBody::Expression(self.parse_expression()?))
        } else {
            self.cursor.expect(Token::LeftBrace)?;
            let block = self.parse_block_statements()?;
            self.cursor.expect(Token::RightBrace)?;

            Ok(FunctionBody::Block(block))
        }
    }

    fn skip_to_next_top_level_object(&mut self) -> Result<(), ParserError> {
        if !self.cursor.match_token(Token::EOF) {
            self.cursor.next()?;
        }

        while !self.cursor.match_token(Token::EOF)
            && !self.cursor.match_keyword("public")
            && !self.cursor.match_keyword("private")
            && !self.cursor.match_keyword("internal")
            && !self.cursor.match_keyword("protected")
            && !self.cursor.match_token(Token::TypeAlias)
            && !self.cursor.match_token(Token::Fun)
            && !self.cursor.match_token(Token::Val)
            && !self.cursor.match_token(Token::Var)
            && !self.cursor.match_token(Token::Class)
            && !self.cursor.match_token(Token::Object)
        {
            self.cursor.next()?;
        }

        Ok(())
    }

    fn skip_to_next_statement(&mut self) -> Result<(), ParserError> {
        while !self.cursor.match_token(Token::EOF)
            && !self.cursor.match_token(Token::Semicolon)
            && !self.cursor.match_token(Token::TypeAlias)
            && !self.cursor.match_token(Token::Fun)
            && !self.cursor.match_token(Token::Val)
            && !self.cursor.match_token(Token::Var)
            && !self.cursor.match_token(Token::Class)
            && !self.cursor.match_token(Token::Object)
            && !self.cursor.at_newline()
        {
            self.cursor.next()?;
        }

        while self.cursor.match_token(Token::Semicolon) {
            self.cursor.next()?;
        }

        Ok(())
    }

    fn todo(&self) -> ! {
        for error in &self.errors {
            println!("{}", error);
        }
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn file2() {
        let src = Source::from_file("./examples/tests/test_file_2.kt").expect("Unable to read file");
        let file = Parser::from(src).parse().unwrap();

        assert_eq!(file.package_header, Some(PackageHeader {
            span: ByteSpan::new(0, 17),
            path: vec!["test".to_string(), "com".to_string(), "example".to_string()],
        }));

        assert_eq!(file.imports.len(), 4);

        assert_eq!(file.imports[0], Import {
            span: ByteSpan::new(26, 45),
            path: vec!["kotlin".to_string(), "math".to_string(), "cos".to_string()],
            alias: None,
        });
        assert_eq!(file.imports[1], Import {
            span: ByteSpan::new(49, 68),
            path: vec!["kotlin".to_string(), "math".to_string(), "sin".to_string()],
            alias: None,
        });
        assert_eq!(file.imports[2], Import {
            span: ByteSpan::new(72, 91),
            path: vec!["kotlin".to_string(), "math".to_string(), "PI".to_string()],
            alias: None,
        });
        assert_eq!(file.imports[3], Import {
            span: ByteSpan::new(94, 134),
            path: vec!["kotlin".to_string(), "collections".to_string(), "ArrayDeque".to_string()],
            alias: Some("VecDeque".to_string()),
        });
    }

    fn parse_function(path: &str) {
        let src = Source::from_file(path).expect("Unable to read file");

        match Parser::from(src).parse() {
            Ok(file) => {
                println!("{}", file.objects[0]);
            }
            Err(e) => {
                for error in e {
                    println!("{}", error);
                }
                panic!("Parser returned errors")
            }
        }
    }

    fn parse_file(path: &str) {
        let src = Source::from_file(path).expect("Unable to read file");

        match Parser::from(src).parse() {
            Ok(file) => {
                println!("{}", file);
            }
            Err(e) => {
                for error in e {
                    println!("{}", error);
                }
                panic!("Parser returned errors")
            }
        }
    }

    #[test]
    fn file3() {
        parse_file("./examples/tests/test_file_3.kt");
    }

    #[test]
    fn file4() {
        parse_function("./examples/tests/test_file_4.kt");
    }

    #[test]
    fn file5() {
        parse_function("./examples/tests/test_file_5.kt");
    }

    #[test]
    fn file6() {
        parse_function("./examples/tests/test_file_6.kt");
    }

    #[test]
    fn file7() {
        parse_function("./examples/tests/test_file_7.kt");
    }

    #[test]
    fn file8() {
        parse_file("./examples/tests/test_file_8.kt");
    }

    #[test]
    fn file9() {
        parse_file("./examples/tests/test_file_9.kt");
    }

    #[test]
    fn file10() {
        parse_function("./examples/tests/test_file_10.kt");
    }
}
