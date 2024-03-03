import { Comment, Node } from './javascript';
import { SourceLocation } from './scanner';
import { Syntax } from './syntax';
import { TokenEntry } from './token';

export type ArgumentListElement = Expression | SpreadElement;
export type ArrayExpressionElement = Expression | SpreadElement | null;
export type ArrayPatternElement = ArrayPattern | AssignmentPattern | Identifier | ObjectPattern | RestElement | null;
export type Declaration = ClassDeclaration | ExportDeclaration | FunctionDeclaration | ImportDeclaration | VariableDeclaration;
export type ExportableDefaultDeclaration = ArrayPattern | Identifier | ClassDeclaration | Expression | FunctionDeclaration | ObjectPattern;
export type ExportableNamedDeclaration = ClassDeclaration | FunctionDeclaration | VariableDeclaration;
export type ExportDeclaration = ExportAllDeclaration | ExportDefaultDeclaration | ExportNamedDeclaration;
export type Expression = ArrayExpression | ArrowFunctionExpression | AssignmentExpression | AsyncArrowFunctionExpression | FunctionExpression |
    AwaitExpression | BinaryExpression | CallExpression | ClassExpression | MemberExpression |
    ConditionalExpression | Identifier | FunctionExpression | Literal | NewExpression | ObjectExpression |
    RegexLiteral | SequenceExpression | MemberExpression | TaggedTemplateExpression |
    ThisExpression | UnaryExpression | UpdateExpression | YieldExpression;

export type FunctionParameter = ArrayPattern | AssignmentPattern | Identifier | ObjectPattern | RestElement;

export function is_function_parameter(node: Node): node is FunctionParameter {
    return is_array_pattern(node) || is_assignment_pattern(node) || is_identifier(node) || is_object_pattern(node) || is_rest_element(node);
}

export function assert_function_parameter(node: Node): FunctionParameter {
    if (is_function_parameter(node)) {
        return node;
    }
    else {
        throw new Error();
    }
}

export function is_function_parameters(nodes: Node[]): nodes is FunctionParameter[] {
    return nodes.every(is_function_parameter);
}

export function assert_function_parameters(nodes: Node[]): FunctionParameter[] {
    if (is_function_parameters(nodes)) {
        return nodes;
    }
    else {
        throw new Error();
    }
}

export type ImportDeclarationSpecifier = ImportDefaultSpecifier | ImportNamespaceSpecifier | ImportSpecifier;
export type ObjectExpressionProperty = Property | SpreadProperty;
export type ObjectPatternProperty = Property | RestProperty;
export type Statement = BreakStatement | ContinueStatement | DebuggerStatement | DoWhileStatement |
    EmptyStatement | ExpressionStatement | Directive | ForStatement | ForInStatement | ForOfStatement |
    FunctionDeclaration | IfStatement | ReturnStatement | SwitchStatement | ThrowStatement |
    TryStatement | VariableDeclaration | WhileStatement | WithStatement;

export type PropertyKey = Identifier | Literal;

export function is_property_key(node: Node): node is PropertyKey {
    return is_identifier(node) || is_literal(node);
}

export function assert_property_key(node: Node): PropertyKey {
    if (is_property_key(node)) {
        return node;
    }
    else {
        throw new Error();
    }
}

export type PropertyValue = ArrayPattern | AssignmentPattern | Identifier | FunctionExpression | ObjectPattern;

export function is_property_value(node: Node): node is PropertyValue {
    return is_array_pattern(node) || is_assignment_pattern(node) || is_identifier(node) || is_function_expression(node) || is_object_pattern(node);
}

export function assert_property_value(node: Node): PropertyValue {
    if (is_property_value(node)) {
        return node;
    }
    else {
        throw new Error();
    }
}

export type StatementListItem = Declaration | Statement;

export abstract class BaseNode implements Node {
    leadingComments?: Comment[];
    trailingComments?: Comment[];
    innerComments?: Comment[];
    range?: [number, number];
    loc?: SourceLocation;
    constructor(readonly type: string) {
    }
}

export class ArrayExpression extends BaseNode {
    readonly elements: ArrayExpressionElement[];
    constructor(elements: ArrayExpressionElement[]) {
        super(Syntax.ArrayExpression);
        this.elements = elements;
    }
}

export function is_array_expression(node: unknown): node is ArrayExpression {
    return node instanceof ArrayExpression;
}

export class ArrayPattern extends BaseNode {
    readonly elements: ArrayPatternElement[];
    constructor(elements: ArrayPatternElement[]) {
        super(Syntax.ArrayPattern);
        this.elements = elements;
    }
}

export function is_array_pattern(node: unknown): node is ArrayPattern {
    return node instanceof ArrayPattern;
}

export class ArrowFunctionExpression extends BaseNode {
    readonly id: Identifier | null;
    readonly params: FunctionParameter[];
    readonly body: BlockStatement | Expression;
    readonly generator: boolean;
    readonly expression: boolean;
    readonly async: boolean;
    constructor(params: FunctionParameter[], body: BlockStatement | Expression, expression: boolean) {
        super(Syntax.ArrowFunctionExpression);
        this.id = null;
        this.params = params;
        this.body = body;
        this.generator = false;
        this.expression = expression;
        this.async = false;
    }
}

export class ArrowParameterPlaceHolder extends BaseNode {
    readonly params: Expression[];
    readonly async: boolean;
    constructor(params: Expression[], async: boolean) {
        super(Syntax.ArrowParameterPlaceHolder);
        this.params = params;
        this.async = async;
    }
}

export function is_arrow_parameter_placeholder(node: unknown): node is ArrowParameterPlaceHolder {
    return node instanceof ArrowParameterPlaceHolder;
}

export class AssignmentExpression extends BaseNode {
    readonly operator: string;
    readonly left: Expression;
    readonly right: Expression;
    constructor(operator: string, left: Expression, right: Expression) {
        super(Syntax.AssignmentExpression);
        this.operator = operator;
        this.left = left;
        this.right = right;
    }
}

export function is_assignment_expression(node: unknown): node is AssignmentExpression {
    return node instanceof AssignmentExpression;
}

export class AssignmentPattern extends BaseNode {
    readonly left: ArrayPattern | Identifier | ObjectPattern;
    readonly right: Expression;
    constructor(left: ArrayPattern | Identifier | ObjectPattern, right: Expression) {
        super(Syntax.AssignmentPattern);
        this.left = left;
        this.right = right;
    }
}

export function is_assignment_pattern(node: unknown): node is AssignmentPattern {
    return node instanceof AssignmentPattern;
}

export class AsyncArrowFunctionExpression extends BaseNode {
    readonly id: Identifier | null;
    readonly params: FunctionParameter[];
    readonly body: BlockStatement | Expression;
    readonly generator: boolean;
    readonly expression: boolean;
    readonly async: boolean;
    constructor(params: FunctionParameter[], body: BlockStatement | Expression, expression: boolean) {
        super(Syntax.ArrowFunctionExpression);
        this.id = null;
        this.params = params;
        this.body = body;
        this.generator = false;
        this.expression = expression;
        this.async = true;
    }
}

export class AwaitExpression extends BaseNode {
    readonly argument: Expression;
    constructor(argument: Expression) {
        super(Syntax.AwaitExpression);
        this.argument = argument;
    }
}

export class BinaryExpression extends BaseNode {
    readonly operator: string;
    readonly left: Expression;
    readonly right: Expression;
    constructor(operator: string, left: Expression, right: Expression) {
        const logical = (operator === '||' || operator === '&&');
        super(logical ? Syntax.LogicalExpression : Syntax.BinaryExpression);
        this.operator = operator;
        this.left = left;
        this.right = right;
    }
}

export class BlockStatement extends BaseNode {
    readonly body: Statement[];
    constructor(body: Statement[]) {
        super(Syntax.BlockStatement);
        this.body = body;
    }
}

export function is_block_statement(node: Node): node is BlockStatement {
    return node instanceof BlockStatement;
}

export class BreakStatement extends BaseNode {
    readonly label: Identifier | null;
    constructor(label: Identifier | null) {
        super(Syntax.BreakStatement);
        this.label = label;
    }
}

export class CallExpression extends BaseNode {
    callee: Expression | Import;
    arguments: ArgumentListElement[];
    constructor(callee: Expression | Import, args: ArgumentListElement[]) {
        super(Syntax.CallExpression);
        this.callee = callee;
        this.arguments = args;
    }
}

export class CatchClause extends BaseNode {
    readonly param: ArrayPattern | Identifier | ObjectPattern;
    readonly body: BlockStatement;
    constructor(param: ArrayPattern | Identifier | ObjectPattern, body: BlockStatement) {
        super(Syntax.CatchClause);
        this.param = param;
        this.body = body;
    }
}

export class ClassBody extends BaseNode {
    readonly body: (Property | MethodDefinition)[];
    constructor(body: (Property | MethodDefinition)[]) {
        super(Syntax.ClassBody);
        this.body = body;
    }
}

export class ClassDeclaration extends BaseNode {
    readonly id: Identifier | null;
    readonly superClass: Identifier | null;
    readonly body: ClassBody;
    constructor(id: Identifier | null, superClass: Identifier | null, body: ClassBody) {
        super(Syntax.ClassDeclaration);
        this.id = id;
        this.superClass = superClass;
        this.body = body;
    }
}

export class ClassExpression extends BaseNode {
    readonly id: Identifier | null;
    readonly superClass: Identifier | null;
    readonly body: ClassBody;
    constructor(id: Identifier | null, superClass: Identifier | null, body: ClassBody) {
        super(Syntax.ClassExpression);
        this.id = id;
        this.superClass = superClass;
        this.body = body;
    }
}

export class ConditionalExpression extends BaseNode {
    readonly test: Expression;
    readonly consequent: Expression;
    readonly alternate: Expression;
    constructor(test: Expression, consequent: Expression, alternate: Expression) {
        super(Syntax.ConditionalExpression);
        this.test = test;
        this.consequent = consequent;
        this.alternate = alternate;
    }
}

export class ContinueStatement extends BaseNode {
    readonly label: Identifier | null;
    constructor(label: Identifier | null) {
        super(Syntax.ContinueStatement);
        this.label = label;
    }
}

export class DebuggerStatement extends BaseNode {
    constructor() {
        super(Syntax.DebuggerStatement);
    }
}

export class Directive extends BaseNode {
    readonly expression: Expression;
    readonly directive: string;
    constructor(expression: Expression, directive: string) {
        super(Syntax.ExpressionStatement);
        this.expression = expression;
        this.directive = directive;
    }
}

export class DoWhileStatement extends BaseNode {
    body: Statement;
    readonly test: Expression;
    constructor(body: Statement, test: Expression) {
        super(Syntax.DoWhileStatement);
        this.body = body;
        this.test = test;
    }
}

export class EmptyStatement extends BaseNode {
    constructor() {
        super(Syntax.EmptyStatement);
    }
}

export class ExportAllDeclaration extends BaseNode {
    readonly source: Literal;
    constructor(source: Literal) {
        super(Syntax.ExportAllDeclaration);
        this.source = source;
    }
}

export class ExportDefaultDeclaration extends BaseNode {
    readonly declaration: ExportableDefaultDeclaration;
    constructor(declaration: ExportableDefaultDeclaration) {
        super(Syntax.ExportDefaultDeclaration);
        this.declaration = declaration;
    }
}

export class ExportNamedDeclaration extends BaseNode {
    readonly declaration: ExportableNamedDeclaration | null;
    readonly specifiers: ExportSpecifier[];
    readonly source: Literal | null;
    constructor(declaration: ExportableNamedDeclaration | null, specifiers: ExportSpecifier[], source: Literal | null) {
        super(Syntax.ExportNamedDeclaration);
        this.declaration = declaration;
        this.specifiers = specifiers;
        this.source = source;
    }
}

export class ExportSpecifier extends BaseNode {
    readonly exported: Identifier;
    readonly local: Identifier;
    constructor(local: Identifier, exported: Identifier) {
        super(Syntax.ExportSpecifier);
        this.exported = exported;
        this.local = local;
    }
}

export class ExpressionStatement extends BaseNode {
    readonly expression: Expression;
    constructor(expression: Expression) {
        super(Syntax.ExpressionStatement);
        this.expression = expression;
    }
}

export function is_expression_statement(node: unknown): node is Directive | ExpressionStatement {
    return (node instanceof Directive) || (node instanceof ExpressionStatement);
}

export class ForInStatement extends BaseNode {
    readonly left: Expression;
    readonly right: Expression;
    readonly body: Statement;
    readonly each: boolean;
    constructor(left: Expression, right: Expression, body: Statement) {
        super(Syntax.ForInStatement);
        this.left = left;
        this.right = right;
        this.body = body;
        this.each = false;
    }
}

export class ForOfStatement extends BaseNode {
    readonly left: Expression;
    readonly right: Expression;
    readonly body: Statement;
    constructor(left: Expression, right: Expression, body: Statement) {
        super(Syntax.ForOfStatement);
        this.left = left;
        this.right = right;
        this.body = body;
    }
}

export class ForStatement extends BaseNode {
    readonly init: Expression | null;
    readonly test: Expression | null;
    readonly update: Expression | null;
    body: Statement;
    constructor(init: Expression | null, test: Expression | null, update: Expression | null, body: Statement) {
        super(Syntax.ForStatement);
        this.init = init;
        this.test = test;
        this.update = update;
        this.body = body;
    }
}

export class FunctionDeclaration extends BaseNode {
    readonly id: Identifier | null;
    readonly params: FunctionParameter[];
    readonly body: BlockStatement;
    readonly generator: boolean;
    readonly expression: boolean;
    readonly async: boolean;
    constructor(id: Identifier | null, params: FunctionParameter[], body: BlockStatement, generator: boolean, async: boolean) {
        super(Syntax.FunctionDeclaration);
        this.id = id;
        this.params = params;
        this.body = body;
        this.generator = generator;
        this.expression = false;
        this.async = async;
    }
}

export class FunctionExpression extends BaseNode {
    readonly id: Identifier | null;
    readonly params: FunctionParameter[];
    readonly body: BlockStatement;
    readonly generator: boolean;
    readonly expression: boolean;
    readonly async: boolean;
    constructor(id: Identifier | null, params: FunctionParameter[], body: BlockStatement, generator: boolean, async: boolean) {
        super(Syntax.FunctionExpression);
        this.id = id;
        this.params = params;
        this.body = body;
        this.generator = generator;
        this.expression = false;
        this.async = async;
    }
}

export function is_function_expression(node: unknown): node is FunctionExpression {
    return node instanceof FunctionExpression;
}

export class Identifier extends BaseNode {
    name: string;
    constructor(name: string) {
        super(Syntax.Identifier);
        this.name = name;
    }
}

export function is_identifier(node: unknown): node is Identifier {
    return node instanceof Identifier;
}

export function assert_identifier(node: unknown): Identifier {
    if (is_identifier(node)) {
        return node;
    }
    else {
        throw new Error();
    }
}

export class IfStatement extends BaseNode {
    readonly test: Expression;
    readonly consequent: Statement;
    readonly alternate: Statement | null;
    constructor(test: Expression, consequent: Statement, alternate: Statement | null) {
        super(Syntax.IfStatement);
        this.test = test;
        this.consequent = consequent;
        this.alternate = alternate;
    }
}

export function is_if_statement(node: Node): node is IfStatement {
    return node.type === Syntax.IfStatement;
}

export class Import extends BaseNode {
    constructor() {
        super(Syntax.Import);
    }
}

export class ImportDeclaration extends BaseNode {
    readonly specifiers: ImportDeclarationSpecifier[];
    readonly source: Literal;
    constructor(specifiers: ImportDeclarationSpecifier[], source: Literal) {
        super(Syntax.ImportDeclaration);
        this.specifiers = specifiers;
        this.source = source;
    }
}

export class ImportDefaultSpecifier extends BaseNode {
    readonly local: Identifier;
    constructor(local: Identifier) {
        super(Syntax.ImportDefaultSpecifier);
        this.local = local;
    }
}

export class ImportNamespaceSpecifier extends BaseNode {
    readonly local: Identifier;
    constructor(local: Identifier) {
        super(Syntax.ImportNamespaceSpecifier);
        this.local = local;
    }
}

export class ImportSpecifier extends BaseNode {
    readonly local: Identifier;
    readonly imported: Identifier;
    constructor(local: Identifier, imported: Identifier) {
        super(Syntax.ImportSpecifier);
        this.local = local;
        this.imported = imported;
    }
}

export class LabeledStatement extends BaseNode {
    readonly label: Identifier;
    readonly body: Statement;
    constructor(label: Identifier, body: Statement) {
        super(Syntax.LabeledStatement);
        this.label = label;
        this.body = body;
    }
}

export class Literal extends BaseNode {
    readonly value: boolean | number | string | null;
    readonly raw: string;
    constructor(value: boolean | number | string | null, raw: string) {
        super(Syntax.Literal);
        this.value = value;
        this.raw = raw;
    }
}

export function is_literal(node: unknown): node is Literal {
    return node instanceof Literal;
}

export class MetaProperty extends BaseNode {
    readonly meta: Identifier;
    readonly property: Identifier;
    constructor(meta: Identifier, property: Identifier) {
        super(Syntax.MetaProperty);
        this.meta = meta;
        this.property = property;
    }
}

export class MethodDefinition extends BaseNode {
    readonly key: Expression | null;
    readonly computed: boolean;
    readonly value: FunctionExpression | null;
    readonly kind: string;
    readonly static: boolean;
    constructor(key: Expression | null, computed: boolean, value: FunctionExpression | null, kind: string, isStatic: boolean) {
        super(Syntax.MethodDefinition);
        this.key = key;
        this.computed = computed;
        this.value = value;
        this.kind = kind;
        this.static = isStatic;
    }
}

export function is_module(node: unknown): node is Program {
    return (node instanceof Program) && node.sourceType === 'module';
}

export class NewExpression extends BaseNode {
    readonly callee: Expression;
    readonly arguments: ArgumentListElement[];
    constructor(callee: Expression, args: ArgumentListElement[]) {
        super(Syntax.NewExpression);
        this.callee = callee;
        this.arguments = args;
    }
}

export class ObjectExpression extends BaseNode {
    readonly properties: ObjectExpressionProperty[];
    constructor(properties: ObjectExpressionProperty[]) {
        super(Syntax.ObjectExpression);
        this.properties = properties;
    }
}

export function is_object_expression(node: unknown): node is ObjectExpression {
    return node instanceof ObjectExpression;
}

export class ObjectPattern extends BaseNode {
    readonly properties: ObjectPatternProperty[];
    constructor(properties: ObjectPatternProperty[]) {
        super(Syntax.ObjectPattern);
        this.properties = properties;
    }
}

export function is_object_pattern(node: unknown): node is ObjectPattern {
    return node instanceof ObjectPattern;
}

export class Property extends BaseNode {
    readonly key: PropertyKey;
    readonly computed: boolean;
    readonly value: PropertyValue | null;
    readonly kind: string;
    readonly method: boolean;
    readonly shorthand: boolean;
    constructor(kind: 'get' | 'set' | 'init', key: PropertyKey, computed: boolean, value: PropertyValue | null, method: boolean, shorthand: boolean) {
        super(Syntax.Property);
        this.key = key;
        this.computed = computed;
        this.value = value;
        this.kind = kind;
        this.method = method;
        this.shorthand = shorthand;
    }
}

export function is_property(node: unknown): node is Property {
    return node instanceof Property;
}

export class RegexLiteral extends BaseNode {
    readonly value: RegExp;
    readonly raw: string;
    readonly regex: { pattern: string, flags: string | undefined };
    constructor(value: RegExp, raw: string, pattern: string, flags: string | undefined) {
        super(Syntax.Literal);
        this.value = value;
        this.raw = raw;
        this.regex = { pattern, flags };
    }
}

export class RestElement extends BaseNode {
    readonly argument: ArrayPattern | Identifier | ObjectPattern;
    constructor(argument: ArrayPattern | Identifier | ObjectPattern) {
        super(Syntax.RestElement);
        this.argument = argument;
    }
}

export function is_rest_element(node: unknown): node is RestElement {
    return node instanceof RestElement;
}

export class RestProperty extends BaseNode {
    readonly argument: Expression;
    constructor(argument: Expression) {
        super(Syntax.RestProperty);
        this.argument = argument;
    }
}

export function is_rest_property(node: unknown): node is RestProperty {
    return node instanceof RestProperty;
}

export class ReturnStatement extends BaseNode {
    readonly argument: Expression | null;
    constructor(argument: Expression | null) {
        super(Syntax.ReturnStatement);
        this.argument = argument;
    }
}

export class Program extends BaseNode {
    readonly body: StatementListItem[];
    readonly sourceType: 'module' | 'script';
    comments?: Comment[];
    tokens?: TokenEntry[];
    errors?: Error[];
    constructor(body: StatementListItem[], sourceType: 'module' | 'script') {
        super(Syntax.Program);
        this.sourceType = sourceType;
        this.body = body;
    }
}

export function is_script(node: unknown): node is Program {
    return (node instanceof Program) && node.sourceType === 'script';
}

export function is_program(node: unknown): node is Program {
    return is_module(node) || is_script(node);
}

export class SequenceExpression extends BaseNode {
    readonly expressions: Expression[];
    constructor(expressions: Expression[]) {
        super(Syntax.SequenceExpression);
        this.expressions = expressions;
    }
}

export function is_sequence_expression(node: unknown): node is SequenceExpression {
    return node instanceof SequenceExpression;
}

/**
 * ... AssignmentExpression
 */
export class SpreadElement extends BaseNode {
    readonly argument: Expression;
    constructor(argument: Expression) {
        super(Syntax.SpreadElement);
        this.argument = argument;
    }
}

export function is_spread_element(node: unknown): node is SpreadElement {
    return node instanceof SpreadElement;
}

export class SpreadProperty extends BaseNode {
    readonly argument: Expression;
    constructor(argument: Expression) {
        super(Syntax.SpreadProperty);
        this.argument = argument;
    }
}

export function is_spread_property(node: unknown): node is SpreadProperty {
    return node instanceof SpreadProperty;
}

export class MemberExpression extends BaseNode {
    readonly computed: boolean;
    readonly object: Expression;
    readonly property: Expression;
    constructor(object: Expression, property: Expression, computed: boolean) {
        super(Syntax.MemberExpression);
        this.computed = computed;
        this.object = object;
        this.property = property;
    }
}

export function is_member_expression(node: unknown): node is MemberExpression {
    return node instanceof MemberExpression;
}

export class Super extends BaseNode {
    constructor() {
        super(Syntax.Super);
    }
}

export class SwitchCase extends BaseNode {
    /**
     * The null value corresponds to the default case.
     */
    readonly test: Expression | null;
    readonly consequent: Statement[];
    constructor(test: Expression | null, consequent: Statement[]) {
        super(Syntax.SwitchCase);
        this.test = test;
        this.consequent = consequent;
    }
}

export class SwitchStatement extends BaseNode {
    readonly discriminant: Expression;
    readonly cases: SwitchCase[];
    constructor(discriminant: Expression, cases: SwitchCase[]) {
        super(Syntax.SwitchStatement);
        this.discriminant = discriminant;
        this.cases = cases;
    }
}

export class TaggedTemplateExpression extends BaseNode {
    readonly tag: Expression;
    readonly quasi: TemplateLiteral;
    constructor(tag: Expression, quasi: TemplateLiteral) {
        super(Syntax.TaggedTemplateExpression);
        this.tag = tag;
        this.quasi = quasi;
    }
}

interface TemplateElementValue {
    cooked: string;
    raw: string;
}

export class TemplateElement extends BaseNode {
    readonly value: TemplateElementValue;
    readonly tail: boolean;
    constructor(value: TemplateElementValue, tail: boolean) {
        super(Syntax.TemplateElement);
        this.value = value;
        this.tail = tail;
    }
}

export class TemplateLiteral extends BaseNode {
    readonly quasis: TemplateElement[];
    readonly expressions: Expression[];
    constructor(quasis: TemplateElement[], expressions: Expression[]) {
        super(Syntax.TemplateLiteral);
        this.quasis = quasis;
        this.expressions = expressions;
    }
}

export class ThisExpression extends BaseNode {
    constructor() {
        super(Syntax.ThisExpression);
    }
}

export class ThrowStatement extends BaseNode {
    readonly argument: Expression;
    constructor(argument: Expression) {
        super(Syntax.ThrowStatement);
        this.argument = argument;
    }
}

export class TryStatement extends BaseNode {
    readonly block: BlockStatement;
    readonly handler: CatchClause | null;
    readonly finalizer: BlockStatement | null;
    constructor(block: BlockStatement, handler: CatchClause | null, finalizer: BlockStatement | null) {
        super(Syntax.TryStatement);
        this.block = block;
        this.handler = handler;
        this.finalizer = finalizer;
    }
}

export class UnaryExpression extends BaseNode {
    readonly operator: string;
    readonly argument: Expression;
    readonly prefix: boolean;
    constructor(operator: string, argument: Expression) {
        super(Syntax.UnaryExpression);
        this.operator = operator;
        this.argument = argument;
        this.prefix = true;
    }
}

export class UpdateExpression extends BaseNode {
    readonly operator: string;
    readonly argument: Expression;
    readonly prefix: boolean;
    constructor(operator: string, argument: Expression, prefix: boolean) {
        super(Syntax.UpdateExpression);
        this.operator = operator;
        this.argument = argument;
        this.prefix = prefix;
    }
}

export class VariableDeclaration extends BaseNode {
    readonly declarations: VariableDeclarator[];
    readonly kind: string;
    constructor(declarations: VariableDeclarator[], kind: string) {
        super(Syntax.VariableDeclaration);
        this.declarations = declarations;
        this.kind = kind;
    }
}

export class VariableDeclarator extends BaseNode {
    readonly id: ArrayPattern | Identifier | ObjectPattern;
    readonly init: Expression | null;
    constructor(id: ArrayPattern | Identifier | ObjectPattern, init: Expression | null) {
        super(Syntax.VariableDeclarator);
        this.id = id;
        this.init = init;
    }
}

export class WhileStatement extends BaseNode {
    readonly test: Expression;
    body: Statement;
    constructor(test: Expression, body: Statement) {
        super(Syntax.WhileStatement);
        this.test = test;
        this.body = body;
    }
}

export class WithStatement extends BaseNode {
    readonly object: Expression;
    readonly body: Statement;
    constructor(object: Expression, body: Statement) {
        super(Syntax.WithStatement);
        this.object = object;
        this.body = body;
    }
}

export class YieldExpression extends BaseNode {
    readonly argument: Expression | null;
    readonly delegate: boolean;
    constructor(argument: Expression | null, delegate: boolean) {
        super(Syntax.YieldExpression);
        this.argument = argument;
        this.delegate = delegate;
    }
}

export function is_yield_expression(node: unknown): node is YieldExpression {
    return node instanceof YieldExpression;
}
