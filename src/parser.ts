import { assert } from './assert';
import { ErrorHandler } from './error-handler';
import { ParseDelegate, ParseOptions } from './esprima';
import { Node } from './javascript';
import { Messages } from './messages';
import {
    ArgumentListElement,
    ArrayExpression,
    ArrayExpressionElement,
    ArrayPattern,
    ArrayPatternElement,
    ArrowFunctionExpression,
    ArrowParameterPlaceHolder,
    assert_function_parameters,
    assert_identifier,
    assert_property_key,
    assert_property_value,
    AssignmentExpression,
    AssignmentPattern,
    AsyncArrowFunctionExpression,
    AwaitExpression,
    BaseNode,
    BinaryExpression,
    BlockStatement,
    BreakStatement,
    CallExpression,
    CatchClause,
    ClassBody,
    ClassDeclaration,
    ClassExpression,
    ConditionalExpression,
    ContinueStatement,
    DebuggerStatement,
    Directive,
    DoWhileStatement,
    EmptyStatement,
    ExportAllDeclaration,
    ExportDeclaration,
    ExportDefaultDeclaration,
    ExportNamedDeclaration,
    ExportSpecifier,
    Expression,
    ExpressionStatement,
    ForInStatement,
    ForOfStatement,
    ForStatement,
    FunctionDeclaration,
    FunctionExpression,
    FunctionParameter,
    Identifier,
    IfStatement,
    Import,
    ImportDeclaration,
    ImportDeclarationSpecifier,
    ImportDefaultSpecifier,
    ImportNamespaceSpecifier,
    ImportSpecifier,
    is_array_expression,
    is_array_pattern,
    is_arrow_parameter_placeholder,
    is_assignment_expression,
    is_assignment_pattern,
    is_identifier,
    is_literal,
    is_member_expression,
    is_object_expression,
    is_object_pattern,
    is_property,
    is_rest_element,
    is_rest_property,
    is_sequence_expression,
    is_spread_element,
    is_spread_property,
    is_yield_expression,
    LabeledStatement,
    Literal, MemberExpression,
    MetaProperty,
    MethodDefinition,
    NewExpression,
    ObjectExpression,
    ObjectExpressionProperty,
    ObjectPattern,
    ObjectPatternProperty,
    Program,
    Property,
    PropertyKey,
    PropertyValue,
    RegexLiteral,
    RestElement,
    RestProperty,
    ReturnStatement,
    SequenceExpression,
    SpreadElement,
    SpreadProperty,
    Statement,
    StatementListItem, Super,
    SwitchCase,
    SwitchStatement,
    TaggedTemplateExpression,
    TemplateElement,
    TemplateLiteral,
    ThisExpression,
    ThrowStatement,
    TryStatement,
    UnaryExpression,
    UpdateExpression,
    VariableDeclaration,
    VariableDeclarator,
    WhileStatement,
    WithStatement,
    YieldExpression
} from './nodes';
import { Precedence } from './Precedence';
import { Comment, Scanner } from './scanner';
import { Syntax } from './syntax';
import { assert_raw_token_value, as_string, RawToken, ReaderEntry, TokenEntry, TokenKind, TokenName } from './token';

// TODO: Move to ./nodes
type Param = ArrayPattern | AssignmentPattern | ObjectPattern | RestElement | RestProperty;

interface FormalParameters {
    firstRestricted?: RawToken;
    simple: boolean;
    params: FunctionParameter[];
    paramSet: { [key: string]: boolean };
    stricted?: RawToken;
    message?: string;
}

interface Config {
    range: boolean;
    loc: boolean;
    source: string | null;
    tokens: boolean;
    comment: boolean;
    tolerant: boolean;
}

interface Context {
    isModule: boolean;
    allowIn: boolean;
    allowStrictDirective: boolean;
    allowYield: boolean;
    await: boolean;
    firstCoverInitializedNameError: RawToken | null;
    isAssignmentTarget: boolean;
    isBindingElement: boolean;
    inFunctionBody: boolean;
    inIteration: boolean;
    inSwitch: boolean;
    labelSet: { [key: string]: boolean };
    strict: boolean;
}

export interface Marker {
    index: number;
    line: number;
    column: number;
}

interface DeclarationOptions {
    inFor: boolean;
}

export interface MetaData {
    start: {
        line: number;
        column: number;
        offset: number;
    };
    end: {
        line: number;
        column: number;
        offset: number;
    };
}

export class BlockComment extends BaseNode {
    constructor(readonly value: string) {
        super(Syntax.BlockComment);
    }
}

export function is_block_comment(node: Node): node is BlockComment {
    return node instanceof BlockComment;
}

export class LineComment extends BaseNode {
    constructor(readonly value: string) {
        super(Syntax.LineComment);
    }
}

export function is_line_comment(node: Node): node is LineComment {
    return node instanceof LineComment;
}

function create_comment_node(multiline: boolean, value: string) {
    if (multiline) {
        return new BlockComment(value);
    }
    else {
        return new LineComment(value);
    }
}

export class Parser {
    readonly config: Config;
    readonly delegate?: ParseDelegate;
    readonly errorHandler: ErrorHandler;
    readonly scanner: Scanner;
    readonly operatorPrecedence: { [ch: string]: number };

    lookahead: RawToken;
    hasLineTerminator: boolean;

    context: Context;
    tokens: TokenEntry[];
    startMarker: Marker;
    lastMarker: Marker;

    constructor(code: string, options: ParseOptions = {}, delegate?: ParseDelegate) {
        this.config = {
            range: (typeof options.range === 'boolean') && options.range,
            loc: (typeof options.loc === 'boolean') && options.loc,
            source: null,
            tokens: (typeof options.tokens === 'boolean') && options.tokens,
            comment: (typeof options.comment === 'boolean') && options.comment,
            tolerant: (typeof options.tolerant === 'boolean') && options.tolerant
        };
        if (this.config.loc && options.source && options.source !== null) {
            this.config.source = String(options.source);
        }

        this.delegate = delegate;

        this.errorHandler = new ErrorHandler();
        this.errorHandler.tolerant = this.config.tolerant;
        this.scanner = new Scanner(code, this.errorHandler);
        this.scanner.trackComment = this.config.comment;

        this.operatorPrecedence = {
            ')': Precedence.Sequence,
            ';': Precedence.Sequence,
            ',': Precedence.Sequence,
            '=': Precedence.Sequence,
            ']': Precedence.Sequence,
            '||': Precedence.LogicalOR,
            '&&': Precedence.LogicalAND,
            '|': Precedence.BitwiseOR,
            '^': Precedence.BitwiseXOR,
            '&': Precedence.BitwiseAND,
            '==': Precedence.Equality,
            '!=': Precedence.Equality,
            '===': Precedence.Equality,
            '!==': Precedence.Equality,
            '<': Precedence.Relational,
            '>': Precedence.Relational,
            '<=': Precedence.Relational,
            '>=': Precedence.Relational,
            '<<': Precedence.BitwiseSHIFT,
            '>>': Precedence.BitwiseSHIFT,
            '>>>': Precedence.BitwiseSHIFT,
            '+': Precedence.Additive,
            '-': Precedence.Additive,
            '*': Precedence.Multiplicative,
            '/': Precedence.Multiplicative,
            '%': Precedence.BitwiseSHIFT
        };

        this.operatorPrecedence = {
            ')': 0,
            ';': 0,
            ',': 0,
            '=': 0,
            ']': 0,
            '??': 5,
            '||': 6,
            '&&': 7,
            '|': 8,
            '^': 9,
            '&': 10,
            '==': 11,
            '!=': 11,
            '===': 11,
            '!==': 11,
            '<': 12,
            '>': 12,
            '<=': 12,
            '>=': 12,
            '<<': 13,
            '>>': 13,
            '>>>': 13,
            '+': 14,
            '-': 14,
            '*': 15,
            '/': 15,
            '%': 15
        };

        this.lookahead = {
            type: TokenKind.EOF,
            value: '',
            lineNumber: this.scanner.lineNumber,
            lineStart: 0,
            start: 0,
            end: 0
        };
        this.hasLineTerminator = false;

        this.context = {
            isModule: false,
            await: false,
            allowIn: true,
            allowStrictDirective: true,
            allowYield: true,
            firstCoverInitializedNameError: null,
            isAssignmentTarget: false,
            isBindingElement: false,
            inFunctionBody: false,
            inIteration: false,
            inSwitch: false,
            labelSet: {},
            strict: false
        };
        this.tokens = [];

        this.startMarker = {
            index: 0,
            line: this.scanner.lineNumber,
            column: 0
        };
        this.lastMarker = {
            index: 0,
            line: this.scanner.lineNumber,
            column: 0
        };
        this.nextToken();
        this.lastMarker = {
            index: this.scanner.index,
            line: this.scanner.lineNumber,
            column: this.scanner.index - this.scanner.lineStart
        };
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    throwError(messageFormat: string, ..._values: ReaderEntry[]): void {
        // eslint-disable-next-line prefer-rest-params
        const args = Array.prototype.slice.call(arguments, 1);
        const msg = messageFormat.replace(/%(\d)/g, (_whole, idx) => {
            assert(idx < args.length, 'Message reference must be in range');
            return args[idx];
        }
        );

        const index = this.lastMarker.index;
        const line = this.lastMarker.line;
        const column = this.lastMarker.column + 1;
        throw this.errorHandler.createError(index, line, column, msg);
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    tolerateError(messageFormat: string, ..._values: ReaderEntry[]) {
        // eslint-disable-next-line prefer-rest-params
        const args = Array.prototype.slice.call(arguments, 1);
        const msg = messageFormat.replace(/%(\d)/g, (_whole, idx) => {
            assert(idx < args.length, 'Message reference must be in range');
            return args[idx];
        }
        );

        const index = this.lastMarker.index;
        const line = this.scanner.lineNumber;
        const column = this.lastMarker.column + 1;
        this.errorHandler.tolerateError(index, line, column, msg);
    }

    // Throw an exception because of the token.
    unexpectedTokenError(token?: RawToken, message?: string): Error {
        let msg = message || Messages.UnexpectedToken;

        if (token) {
            if (!message) {
                msg = (token.type === TokenKind.EOF) ? Messages.UnexpectedEOS :
                    (token.type === TokenKind.Identifier) ? Messages.UnexpectedIdentifier :
                        (token.type === TokenKind.NumericLiteral) ? Messages.UnexpectedNumber :
                            (token.type === TokenKind.StringLiteral) ? Messages.UnexpectedString :
                                (token.type === TokenKind.Template) ? Messages.UnexpectedTemplate :
                                    Messages.UnexpectedToken;

                if (token.type === TokenKind.Keyword) {
                    if (this.scanner.isFutureReservedWord(<string>token.value)) {
                        msg = Messages.UnexpectedReserved;
                    }
                    else if (this.context.strict && this.scanner.isStrictModeReservedWord(<string>token.value)) {
                        msg = Messages.StrictReservedWord;
                    }
                }
            }

            msg = msg.replace('%0', as_string(token.value));
        }
        else {
            msg = msg.replace('%0', 'ILLEGAL');
        }


        if (token && typeof token.lineNumber === 'number') {
            const index = token.start;
            const line = token.lineNumber;
            const lastMarkerLineStart = this.lastMarker.index - this.lastMarker.column;
            const column = token.start - lastMarkerLineStart + 1;
            return this.errorHandler.createError(index, line, column, msg);
        }
        else {
            const index = this.lastMarker.index;
            const line = this.lastMarker.line;
            const column = this.lastMarker.column + 1;
            return this.errorHandler.createError(index, line, column, msg);
        }
    }

    throwUnexpectedToken(token?: RawToken, message?: string): never {
        throw this.unexpectedTokenError(token, message);
    }

    tolerateUnexpectedToken(token?: RawToken, message?: string): void {
        this.errorHandler.tolerate(this.unexpectedTokenError(token, message));
    }

    collectComments(): void {
        if (!this.config.comment) {
            this.scanner.scanComments();
        }
        else {
            const comments: Comment[] = this.scanner.scanComments();
            if (comments.length > 0 && this.delegate) {
                for (let i = 0; i < comments.length; ++i) {
                    const e: Comment = comments[i];

                    const node = create_comment_node(e.multiLine, this.scanner.source.slice(e.slice[0], e.slice[1]));
                    if (this.config.range) {
                        node.range = e.range;
                    }
                    if (this.config.loc) {
                        node.loc = e.loc;
                    }
                    const metadata = {
                        start: {
                            line: e.loc.start.line,
                            column: e.loc.start.column,
                            offset: e.range[0]
                        },
                        end: {
                            line: e.loc.end.line,
                            column: e.loc.end.column,
                            offset: e.range[1]
                        }
                    };
                    this.delegate(node, metadata);
                }
            }
        }
    }

    // From internal representation to an external structure

    getTokenRaw(token: RawToken): string {
        return this.scanner.source.slice(token.start, token.end);
    }

    convertToken(token: RawToken): TokenEntry {
        const t: TokenEntry = {
            type: TokenName[token.type],
            value: this.getTokenRaw(token)
        };
        if (this.config.range) {
            t.range = [token.start, token.end];
        }
        if (this.config.loc) {
            t.loc = {
                start: {
                    line: this.startMarker.line,
                    column: this.startMarker.column
                },
                end: {
                    line: this.scanner.lineNumber,
                    column: this.scanner.index - this.scanner.lineStart
                }
            };
        }
        if (token.type === TokenKind.RegularExpression) {
            const pattern = token.pattern as string;
            const flags = token.flags as string;
            t.regex = { pattern, flags };
        }

        return t;
    }

    /**
     * Advances the scanner by one token.
     * @returns the token corresponding to lookahead before the scanner advanced.
     */
    nextToken(): RawToken {
        const token = this.lookahead;

        this.lastMarker.index = this.scanner.index;
        this.lastMarker.line = this.scanner.lineNumber;
        this.lastMarker.column = this.scanner.index - this.scanner.lineStart;

        this.collectComments();

        if (this.scanner.index !== this.startMarker.index) {
            this.startMarker.index = this.scanner.index;
            this.startMarker.line = this.scanner.lineNumber;
            this.startMarker.column = this.scanner.index - this.scanner.lineStart;
        }

        const next = this.scanner.lex();
        this.hasLineTerminator = (token.lineNumber !== next.lineNumber);

        if (next && this.context.strict && next.type === TokenKind.Identifier) {
            if (this.scanner.isStrictModeReservedWord(next.value as string)) {
                next.type = TokenKind.Keyword;
            }
        }
        this.lookahead = next;

        if (this.config.tokens && next.type !== TokenKind.EOF) {
            this.tokens.push(this.convertToken(next));
        }

        return token;
    }

    nextRegexToken(): RawToken {
        this.collectComments();

        const token = this.scanner.scanRegExp();
        if (this.config.tokens) {
            // Pop the previous token, '/' or '/='
            // This is added from the lookahead token.
            this.tokens.pop();

            this.tokens.push(this.convertToken(token));
        }

        // Prime the next lookahead.
        this.lookahead = token;
        this.nextToken();

        return token;
    }

    createMarker(): Marker {
        return {
            index: this.startMarker.index,
            line: this.startMarker.line,
            column: this.startMarker.column
        };
    }

    tokenMarker(token: RawToken): Marker {
        return {
            index: token.start,
            line: token.lineNumber,
            column: token.start - token.lineStart
        };
    }

    finalize<N extends Node>(marker: Marker, node: N): N {
        if (this.config.range) {
            node.range = [marker.index, this.lastMarker.index];
        }

        if (this.config.loc) {
            node.loc = {
                start: {
                    line: marker.line,
                    column: marker.column,
                },
                end: {
                    line: this.lastMarker.line,
                    column: this.lastMarker.column
                }
            };
            if (this.config.source) {
                node.loc.source = this.config.source;
            }
        }

        if (this.delegate) {
            const metadata = {
                start: {
                    line: marker.line,
                    column: marker.column,
                    offset: marker.index
                },
                end: {
                    line: this.lastMarker.line,
                    column: this.lastMarker.column,
                    offset: this.lastMarker.index
                }
            };
            this.delegate(node, metadata);
        }

        return node;
    }

    // Expect the next token to match the specified punctuator.
    // If not, an exception will be thrown.

    expect(value: string): void {
        const token = this.nextToken();
        if (token.type !== TokenKind.Punctuator || token.value !== value) {
            this.throwUnexpectedToken(token);
        }
    }

    // Quietly expect a comma when in tolerant mode, otherwise delegates to expect().

    expectCommaSeparator() {
        if (this.config.tolerant) {
            const token = this.lookahead;
            if (token.type === TokenKind.Punctuator && token.value === ',') {
                this.nextToken();
            }
            else if (token.type === TokenKind.Punctuator && token.value === ';') {
                this.nextToken();
                this.tolerateUnexpectedToken(token);
            }
            else {
                this.tolerateUnexpectedToken(token, Messages.UnexpectedToken);
            }
        }
        else {
            this.expect(',');
        }
    }

    // Expect the next token to match the specified keyword.
    // If not, an exception will be thrown.

    expectKeyword(keyword: string): void {
        const token = this.nextToken();
        if (token.type !== TokenKind.Keyword || token.value !== keyword) {
            this.throwUnexpectedToken(token);
        }
    }

    // Return true if the next token matches the specified punctuator.

    match(value: string) {
        return this.lookahead.type === TokenKind.Punctuator && this.lookahead.value === value;
    }

    // Return true if the next token matches the specified keyword

    matchKeyword(keyword: string): boolean {
        return this.lookahead.type === TokenKind.Keyword && this.lookahead.value === keyword;
    }

    // Return true if the next token matches the specified contextual keyword
    // (where an identifier is sometimes a keyword depending on the context)

    matchContextualKeyword(keyword: string): boolean {
        return this.lookahead.type === TokenKind.Identifier && this.lookahead.value === keyword;
    }

    // Return true if the next token is an assignment operator

    matchAssign() {
        if (this.lookahead.type !== TokenKind.Punctuator) {
            return false;
        }
        const op = this.lookahead.value;
        return op === '=' ||
            op === '*=' ||
            op === '**=' ||
            op === '/=' ||
            op === '%=' ||
            op === '+=' ||
            op === '-=' ||
            op === '<<=' ||
            op === '>>=' ||
            op === '>>>=' ||
            op === '&=' ||
            op === '^=' ||
            op === '|=';
    }

    // Cover grammar support.
    //
    // When an assignment expression position starts with an left parenthesis, the determination of the type
    // of the syntax is to be deferred arbitrarily long until the end of the parentheses pair (plus a lookahead)
    // or the first comma. This situation also defers the determination of all the expressions nested in the pair.
    //
    // There are three productions that can be parsed in a parentheses pair that needs to be determined
    // after the outermost pair is closed. They are:
    //
    //   1. AssignmentExpression
    //   2. BindingElements
    //   3. AssignmentTargets
    //
    // In order to avoid exponential backtracking, we use two flags to denote if the production can be
    // binding element or assignment target.
    //
    // The three productions have the relationship:
    //
    //   BindingElements ⊆ AssignmentTargets ⊆ AssignmentExpression
    //
    // with a single exception that CoverInitializedName when used directly in an Expression, generates
    // an early error. Therefore, we need the third state, firstCoverInitializedNameError, to track the
    // first usage of CoverInitializedName and report it when we reached the end of the parentheses pair.
    //
    // isolateCoverGrammar function runs the given parser function with a new cover grammar context, and it does not
    // effect the current flags. This means the production the parser parses is only used as an expression. Therefore
    // the CoverInitializedName check is conducted.
    //
    // inheritCoverGrammar function runs the given parse function with a new cover grammar context, and it propagates
    // the flags outside of the parser. This means the production the parser parses is used as a part of a potential
    // pattern. The CoverInitializedName check is deferred.

    isolateCoverGrammar<T>(parseFunction: (this: Parser) => T) {
        const previousIsBindingElement = this.context.isBindingElement;
        const previousIsAssignmentTarget = this.context.isAssignmentTarget;
        const previousFirstCoverInitializedNameError = this.context.firstCoverInitializedNameError;

        this.context.isBindingElement = true;
        this.context.isAssignmentTarget = true;
        this.context.firstCoverInitializedNameError = null;

        const result = parseFunction.call(this);
        if (this.context.firstCoverInitializedNameError !== null) {
            this.throwUnexpectedToken(this.context.firstCoverInitializedNameError);
        }

        this.context.isBindingElement = previousIsBindingElement;
        this.context.isAssignmentTarget = previousIsAssignmentTarget;
        this.context.firstCoverInitializedNameError = previousFirstCoverInitializedNameError;

        return result;
    }

    inheritCoverGrammar<T>(parseFunction: (this: Parser) => T): T {
        const previousIsBindingElement = this.context.isBindingElement;
        const previousIsAssignmentTarget = this.context.isAssignmentTarget;
        const previousFirstCoverInitializedNameError = this.context.firstCoverInitializedNameError;

        this.context.isBindingElement = true;
        this.context.isAssignmentTarget = true;
        this.context.firstCoverInitializedNameError = null;
        try {
            return parseFunction.call(this);
        }
        finally {
            this.context.isBindingElement = this.context.isBindingElement && previousIsBindingElement;
            this.context.isAssignmentTarget = this.context.isAssignmentTarget && previousIsAssignmentTarget;
            this.context.firstCoverInitializedNameError = previousFirstCoverInitializedNameError || this.context.firstCoverInitializedNameError;
        }
    }

    consumeSemicolon() {
        if (this.match(';')) {
            this.nextToken();
        }
        else if (!this.hasLineTerminator) {
            if (this.lookahead.type !== TokenKind.EOF && !this.match('}')) {
                this.throwUnexpectedToken(this.lookahead);
            }
            this.lastMarker.index = this.startMarker.index;
            this.lastMarker.line = this.startMarker.line;
            this.lastMarker.column = this.startMarker.column;
        }
    }

    // https://tc39.github.io/ecma262/#sec-primary-expression

    parsePrimaryExpression(): Expression {
        const node = this.createMarker();

        let expr: Expression;
        let token, raw;

        switch (this.lookahead.type) {
            case TokenKind.Identifier:
                if ((this.context.isModule || this.context.await) && this.lookahead.value === 'await') {
                    this.tolerateUnexpectedToken(this.lookahead);
                }
                expr = this.matchAsyncFunction() ? this.parseFunctionExpression() : this.finalize(node, new Identifier(this.nextToken().value as string));
                break;

            case TokenKind.NumericLiteral:
            case TokenKind.StringLiteral:
                if (this.context.strict && this.lookahead.octal) {
                    this.tolerateUnexpectedToken(this.lookahead, Messages.StrictOctalLiteral);
                }
                this.context.isAssignmentTarget = false;
                this.context.isBindingElement = false;
                token = this.nextToken();
                raw = this.getTokenRaw(token);
                expr = this.finalize(node, new Literal(token.value, raw));
                break;

            case TokenKind.BooleanLiteral:
                this.context.isAssignmentTarget = false;
                this.context.isBindingElement = false;
                token = this.nextToken();
                raw = this.getTokenRaw(token);
                expr = this.finalize(node, new Literal(token.value === 'true', raw));
                break;

            case TokenKind.NullLiteral:
                this.context.isAssignmentTarget = false;
                this.context.isBindingElement = false;
                token = this.nextToken();
                raw = this.getTokenRaw(token);
                expr = this.finalize(node, new Literal(null, raw));
                break;

            case TokenKind.Template:
                expr = this.parseTemplateLiteral();
                break;

            case TokenKind.Punctuator:
                switch (this.lookahead.value) {
                    case '(':
                        this.context.isBindingElement = false;
                        expr = this.inheritCoverGrammar(this.parseGroupExpression);
                        break;
                    case '[':
                        expr = this.inheritCoverGrammar(this.parseArrayInitializer);
                        break;
                    case '{':
                        expr = this.inheritCoverGrammar(this.parseObjectInitializer);
                        break;
                    case '/':
                    case '/=':
                        this.context.isAssignmentTarget = false;
                        this.context.isBindingElement = false;
                        this.scanner.index = this.startMarker.index;
                        token = this.nextRegexToken();
                        raw = this.getTokenRaw(token);
                        expr = this.finalize(node, new RegexLiteral(token.regex as RegExp, raw, token.pattern as string, token.flags));
                        break;
                    default:
                        this.throwUnexpectedToken(this.nextToken());
                }
                break;

            case TokenKind.Keyword:
                if (!this.context.strict && this.context.allowYield && this.matchKeyword('yield')) {
                    expr = this.parseIdentifierName();
                }
                else if (!this.context.strict && this.matchKeyword('let')) {
                    expr = this.finalize(node, new Identifier(this.nextToken().value as string));
                }
                else {
                    this.context.isAssignmentTarget = false;
                    this.context.isBindingElement = false;
                    if (this.matchKeyword('function')) {
                        expr = this.parseFunctionExpression();
                    }
                    else if (this.matchKeyword('this')) {
                        this.nextToken();
                        expr = this.finalize(node, new ThisExpression());
                    }
                    else if (this.matchKeyword('class')) {
                        expr = this.parseClassExpression();
                    }
                    else if (this.matchImportCall()) {
                        expr = this.parseImportCall();
                    }
                    else {
                        this.throwUnexpectedToken(this.nextToken());
                    }
                }
                break;

            default:
                this.throwUnexpectedToken(this.nextToken());
        }

        return expr;
    }

    // https://tc39.github.io/ecma262/#sec-array-initializer

    parseSpreadElement(): SpreadElement {
        const node = this.createMarker();
        this.expect('...');
        const arg = this.inheritCoverGrammar(this.parseAssignmentExpression);
        return this.finalize(node, new SpreadElement(arg));
    }

    parseArrayInitializer(): ArrayExpression {
        const node = this.createMarker();
        const elements: ArrayExpressionElement[] = [];

        this.expect('[');
        while (!this.match(']')) {
            if (this.match(',')) {
                this.nextToken();
                elements.push(null);
            }
            else if (this.match('...')) {
                const element = this.parseSpreadElement();
                if (!this.match(']')) {
                    this.context.isAssignmentTarget = false;
                    this.context.isBindingElement = false;
                    this.expect(',');
                }
                elements.push(element);
            }
            else {
                elements.push(this.inheritCoverGrammar(this.parseAssignmentExpression));
                if (!this.match(']')) {
                    this.expect(',');
                }
            }
        }
        this.expect(']');

        return this.finalize(node, new ArrayExpression(elements));
    }

    // https://tc39.github.io/ecma262/#sec-object-initializer

    parsePropertyMethod(params: FormalParameters): BlockStatement {
        this.context.isAssignmentTarget = false;
        this.context.isBindingElement = false;

        const previousStrict = this.context.strict;
        const previousAllowStrictDirective = this.context.allowStrictDirective;
        this.context.allowStrictDirective = params.simple;
        const body = this.isolateCoverGrammar(this.parseFunctionSourceElements);
        if (this.context.strict && params.firstRestricted) {
            this.tolerateUnexpectedToken(params.firstRestricted, params.message);
        }
        if (this.context.strict && params.stricted) {
            this.tolerateUnexpectedToken(params.stricted, params.message);
        }
        this.context.strict = previousStrict;
        this.context.allowStrictDirective = previousAllowStrictDirective;

        return body;
    }

    parsePropertyMethodFunction(): FunctionExpression {
        const isGenerator = false;
        const node = this.createMarker();

        const previousAllowYield = this.context.allowYield;
        this.context.allowYield = false;
        const params = this.parseFormalParameters();
        const method = this.parsePropertyMethod(params);
        this.context.allowYield = previousAllowYield;

        return this.finalize(node, new FunctionExpression(null, params.params, method, isGenerator, false));
    }

    parsePropertyMethodAsyncFunction(): FunctionExpression {
        const node = this.createMarker();

        const previousAllowYield = this.context.allowYield;
        const previousAwait = this.context.await;
        this.context.allowYield = false;
        this.context.await = true;
        const params = this.parseFormalParameters();
        const method = this.parsePropertyMethod(params);
        this.context.allowYield = previousAllowYield;
        this.context.await = previousAwait;

        return this.finalize(node, new FunctionExpression(null, params.params, method, false, true));
    }

    parseObjectPropertyKey(): Identifier | Literal | Expression {
        const node = this.createMarker();
        const token = this.nextToken();

        switch (token.type) {
            case TokenKind.StringLiteral:
            case TokenKind.NumericLiteral: {
                if (this.context.strict && token.octal) {
                    this.tolerateUnexpectedToken(token, Messages.StrictOctalLiteral);
                }
                const raw = this.getTokenRaw(token);
                return this.finalize(node, new Literal(token.value as string, raw));
            }
            case TokenKind.Identifier:
            case TokenKind.BooleanLiteral:
            case TokenKind.NullLiteral:
            case TokenKind.Keyword:
                return this.finalize(node, new Identifier(as_string(token.value)));
            case TokenKind.Punctuator:
                if (token.value === '[') {
                    const expr = this.isolateCoverGrammar(this.parseAssignmentExpression);
                    this.expect(']');
                    return expr;
                }
                else {
                    this.throwUnexpectedToken(token);
                }
                break;

            default:
                this.throwUnexpectedToken(token);
        }
    }

    isPropertyKey(key: Node | null, value: string): boolean {
        return (is_identifier(key) && key.name === value) || (is_literal(key) && key.value === value);
    }

    parseObjectProperty(hasProto: { value: boolean }): Property {
        const node = this.createMarker();
        const token = this.lookahead;

        let kind: 'get' | 'set' | 'init';
        let key: PropertyKey | Identifier | Expression | null = null;
        let value: PropertyValue | Expression | null = null;

        let computed = false;
        let method = false;
        let shorthand = false;
        let isAsync = false;

        if (token.type === TokenKind.Identifier) {
            const id = token.value;
            this.nextToken();
            computed = this.match('[');
            isAsync = !this.hasLineTerminator && (id === 'async') &&
                !this.match(':') && !this.match('(') && !this.match('*');
            key = isAsync ? this.parseObjectPropertyKey() : this.finalize(node, new Identifier(as_string(id)));
        }
        else if (this.match('*')) {
            this.nextToken();
        }
        else {
            computed = this.match('[');
            key = this.parseObjectPropertyKey();
        }

        const lookaheadPropertyKey = this.qualifiedPropertyName(this.lookahead);
        if (token.type === TokenKind.Identifier && !isAsync && token.value === 'get' && lookaheadPropertyKey) {
            kind = 'get';
            computed = this.match('[');
            key = this.parseObjectPropertyKey();
            this.context.allowYield = false;
            value = this.parseGetterMethod();

        }
        else if (token.type === TokenKind.Identifier && !isAsync && token.value === 'set' && lookaheadPropertyKey) {
            kind = 'set';
            computed = this.match('[');
            key = this.parseObjectPropertyKey();
            value = this.parseSetterMethod();

        }
        else if (token.type === TokenKind.Punctuator && token.value === '*' && lookaheadPropertyKey) {
            kind = 'init';
            computed = this.match('[');
            key = this.parseObjectPropertyKey();
            value = this.parseGeneratorMethod();
            method = true;

        }
        else {
            if (!key) {
                this.throwUnexpectedToken(this.lookahead);
            }

            kind = 'init';
            if (this.match(':') && !isAsync) {
                if (!computed && this.isPropertyKey(key, '__proto__')) {
                    if (hasProto.value) {
                        this.tolerateError(Messages.DuplicateProtoProperty);
                    }
                    hasProto.value = true;
                }
                this.nextToken();
                value = this.inheritCoverGrammar(this.parseAssignmentExpression);

            }
            else if (this.match('(')) {
                value = isAsync ? this.parsePropertyMethodAsyncFunction() : this.parsePropertyMethodFunction();
                method = true;

            }
            else if (token.type === TokenKind.Identifier) {
                const id = this.finalize(node, new Identifier(as_string(token.value)));
                if (this.match('=')) {
                    this.context.firstCoverInitializedNameError = this.lookahead;
                    this.nextToken();
                    shorthand = true;
                    const init = this.isolateCoverGrammar(this.parseAssignmentExpression);
                    value = this.finalize(node, new AssignmentPattern(id, init));
                }
                else {
                    shorthand = true;
                    value = id;
                }
            }
            else {
                this.throwUnexpectedToken(this.nextToken());
            }
        }

        return this.finalize(node, new Property(kind, assert_property_key(key), computed, assert_property_value(value), method, shorthand));
    }

    parseSpreadProperty(): SpreadProperty {
        const node = this.createMarker();
        this.expect('...');
        const arg = this.inheritCoverGrammar(this.parseAssignmentExpression);
        return this.finalize(node, new SpreadProperty(arg));
    }

    parseObjectInitializer(): ObjectExpression {
        const node = this.createMarker();

        this.expect('{');
        const properties: ObjectExpressionProperty[] = [];
        const hasProto = { value: false };
        while (!this.match('}')) {
            properties.push(this.match('...') ? this.parseSpreadElement() : this.parseObjectProperty(hasProto));
            if (!this.match('}')) {
                this.expectCommaSeparator();
            }
        }
        this.expect('}');

        return this.finalize(node, new ObjectExpression(properties));
    }

    // https://tc39.github.io/ecma262/#sec-template-literals

    parseTemplateHead(): TemplateElement {
        assert(this.lookahead.head as boolean, 'Template literal must start with a template head');

        const node = this.createMarker();
        const token = this.nextToken();
        const raw = token.value as string;
        const cooked = token.cooked as string;

        return this.finalize(node, new TemplateElement({ raw, cooked }, token.tail as boolean));
    }

    parseTemplateElement(): TemplateElement {
        if (this.lookahead.type !== TokenKind.Template) {
            this.throwUnexpectedToken();
        }

        const node = this.createMarker();
        const token = this.nextToken();
        const raw = token.value as string;
        const cooked = token.cooked as string;

        return this.finalize(node, new TemplateElement({ raw, cooked }, token.tail as boolean));
    }

    parseTemplateLiteral(): TemplateLiteral {
        const node = this.createMarker();

        const expressions: Expression[] = [];
        const quasis: TemplateElement[] = [];

        let quasi = this.parseTemplateHead();
        quasis.push(quasi);
        while (!quasi.tail) {
            expressions.push(this.parseExpression());
            quasi = this.parseTemplateElement();
            quasis.push(quasi);
        }

        return this.finalize(node, new TemplateLiteral(quasis, expressions));
    }

    // https://tc39.github.io/ecma262/#sec-grouping-operator

    reintepretSpreadElementAsRestElement(node: SpreadElement): RestElement {
        const pattern = this.reinterpretExpressionAsPattern(node.argument);
        // TODO: 
        if (is_array_pattern(pattern)) {
            return new RestElement(pattern);
        }
        else if (is_assignment_pattern(pattern)) {
            throw new Error();
        }
        else if (is_identifier(pattern)) {
            return new RestElement(pattern);
        }
        else if (is_member_expression(pattern)) {
            throw new Error();
        }
        else if (is_object_pattern(pattern)) {
            return new RestElement(pattern);
        }
        else if (is_rest_element(pattern)) {
            throw new Error();
        }
        else if (is_rest_property(pattern)) {
            throw new Error();
        }
        else {
            throw new Error();
        }
    }

    reinterpretArrayExpressionElementAsArrayPatternElement(expr: ArrayExpressionElement): ArrayPatternElement {
        if (is_spread_element(expr)) {
            return this.reintepretSpreadElementAsRestElement(expr);
        }
        if (expr) {
            // TODO: Identifier, ArrayPattern, AssignmentPattern, ObjectPattern may all be created.
            throw new Error(`${expr.type}`);
        }
        else {
            return null;
        }
    }

    reinterpretArrayExpressionAsArrayPattern(expr: ArrayExpression): ArrayPattern {
        const elements: ArrayPatternElement[] = expr.elements.map(e => this.reinterpretArrayExpressionElementAsArrayPatternElement(e));
        return new ArrayPattern(elements);
    }

    reintepretSpreadPropertyAsRestProperty(node: SpreadProperty): RestProperty {
        const argument = this.reinterpretExpressionAsPattern(node.argument);
        return new RestProperty(argument);

    }

    reintepretPropertyAsRestProperty(node: Property): RestProperty {
        if (node.value) {
            const argument = this.reinterpretExpressionAsPattern(node.value);
            return new RestProperty(argument);
        }
        else {
            throw new Error();
            // return new RestProperty(null);
        }
    }

    reinterpretObjectExpressionAsObjectPattern(expr: ObjectExpression): ObjectPattern {
        const properties = expr.properties.map(p => this.reintepretAsRestProperty(p));
        return new ObjectPattern(properties);
    }

    reintepretAsRestProperty(node: Property | SpreadProperty): RestProperty {
        if (is_property(node)) {
            return this.reintepretPropertyAsRestProperty(node);
        }
        else {
            return this.reintepretSpreadPropertyAsRestProperty(node);
        }
    }

    // TODO: Would be nice to have a more specific API?
    reinterpretExpressionAsPattern(expr: Expression): ArrayPattern | AssignmentPattern | ObjectPattern | RestElement | RestProperty | Identifier | MemberExpression {
        if (is_array_expression(expr)) {
            return this.reinterpretArrayExpressionAsArrayPattern(expr);
        }
        else if (is_assignment_expression(expr)) {
            return this.reinterpretExpressionAsPattern(expr.left);
        }
        else if (is_object_expression(expr)) {
            return this.reinterpretObjectExpressionAsObjectPattern(expr);
        }
        else if (is_spread_element(expr)) {
            return this.reintepretSpreadElementAsRestElement(expr);
        }
        else if (is_spread_property(expr)) {
            return this.reintepretSpreadPropertyAsRestProperty(expr);
        }
        else if (is_identifier(expr)) {
            return expr;
        }
        else if (is_member_expression(expr)) {
            return expr;
        }
        else if (is_rest_element(expr)) {
            return expr;
        }
        else if (is_assignment_pattern(expr)) {
            return expr;
        }
        else {
            // Allowing other node types for tolerant parsing rellay messes with the type system.
            // It would be better to return some dummy node that fits the type system.
            throw new Error();
        }
    }

    parseGroupExpression(): ArrowParameterPlaceHolder | Expression {

        this.expect('(');
        if (this.match(')')) {
            this.nextToken();
            if (!this.match('=>')) {
                this.expect('=>');
            }
            return new ArrowParameterPlaceHolder([], false);
        }
        else {
            const startToken = this.lookahead;
            const params: RawToken[] = [];
            if (this.match('...')) {
                const restElement = this.parseRestElement(params);
                this.expect(')');
                if (!this.match('=>')) {
                    this.expect('=>');
                }
                return new ArrowParameterPlaceHolder([restElement], false);
            }
            else {
                let arrow = false;
                this.context.isBindingElement = true;
                // TODO: The use of the member function is a bit scarey.
                let expr = this.inheritCoverGrammar(this.parseAssignmentExpression);

                if (this.match(',')) {
                    const expressions: Expression[] = [];

                    this.context.isAssignmentTarget = false;
                    expressions.push(expr);
                    while (this.lookahead.type !== TokenKind.EOF) {
                        if (!this.match(',')) {
                            break;
                        }
                        this.nextToken();
                        if (this.match(')')) {
                            this.nextToken();
                            for (let i = 0; i < expressions.length; i++) {
                                expressions[i] = this.reinterpretExpressionAsPattern(expressions[i]);
                            }
                            arrow = true;
                            expr = new ArrowParameterPlaceHolder(expressions, false);
                        }
                        else if (this.match('...')) {
                            if (!this.context.isBindingElement) {
                                this.throwUnexpectedToken(this.lookahead);
                            }
                            expressions.push(this.parseRestElement(params));
                            this.expect(')');
                            if (!this.match('=>')) {
                                this.expect('=>');
                            }
                            this.context.isBindingElement = false;
                            for (let i = 0; i < expressions.length; i++) {
                                expressions[i] = this.reinterpretExpressionAsPattern(expressions[i]);
                            }
                            arrow = true;
                            expr = new ArrowParameterPlaceHolder(expressions, false);
                        }
                        else {
                            expressions.push(this.inheritCoverGrammar(this.parseAssignmentExpression));
                        }
                        if (arrow) {
                            break;
                        }
                    }
                    if (!arrow) {
                        expr = this.finalize(this.tokenMarker(startToken), new SequenceExpression(expressions));
                    }
                }

                if (!arrow) {
                    this.expect(')');
                    if (this.match('=>')) {
                        if (is_identifier(expr) && expr.name === 'yield') {
                            arrow = true;
                            expr = new ArrowParameterPlaceHolder([expr], false);
                        }
                        if (!arrow) {
                            if (!this.context.isBindingElement) {
                                this.throwUnexpectedToken(this.lookahead);
                            }

                            if (is_sequence_expression(expr)) {
                                for (let i = 0; i < expr.expressions.length; i++) {
                                    expr.expressions[i] = this.reinterpretExpressionAsPattern(expr.expressions[i]);
                                }
                            }
                            else {
                                expr = this.reinterpretExpressionAsPattern(expr);
                            }

                            const parameters = (is_sequence_expression(expr) ? expr.expressions : [expr]);
                            expr = new ArrowParameterPlaceHolder(parameters, false);
                        }
                    }
                    this.context.isBindingElement = false;
                }
                return expr;
            }
        }
    }

    // https://tc39.github.io/ecma262/#sec-left-hand-side-expressions

    parseArguments(): ArgumentListElement[] {
        this.expect('(');
        const args: ArgumentListElement[] = [];
        if (!this.match(')')) {
            // eslint-disable-next-line no-constant-condition
            while (true) {
                const expr = this.match('...') ? this.parseSpreadElement() :
                    this.isolateCoverGrammar(this.parseAssignmentExpression);
                args.push(expr);
                if (this.match(')')) {
                    break;
                }
                this.expectCommaSeparator();
                if (this.match(')')) {
                    break;
                }
            }
        }
        this.expect(')');

        return args;
    }

    isIdentifierName(token: RawToken): boolean {
        return token.type === TokenKind.Identifier ||
            token.type === TokenKind.Keyword ||
            token.type === TokenKind.BooleanLiteral ||
            token.type === TokenKind.NullLiteral;
    }

    parseIdentifierName(): Identifier {
        const node = this.createMarker();
        const token = this.nextToken();
        if (!this.isIdentifierName(token)) {
            this.throwUnexpectedToken(token);
        }
        return this.finalize(node, new Identifier(as_string(token.value)));
    }

    parseNewExpression(): MetaProperty | NewExpression {
        const node = this.createMarker();

        const id = this.parseIdentifierName();
        assert(id.name === 'new', 'New expression must start with `new`');

        let expr;
        if (this.match('.')) {
            this.nextToken();
            if (this.lookahead.type === TokenKind.Identifier && this.context.inFunctionBody && this.lookahead.value === 'target') {
                const property = this.parseIdentifierName();
                expr = new MetaProperty(id, property);
            }
            else {
                this.throwUnexpectedToken(this.lookahead);
            }
        }
        else if (this.matchKeyword('import')) {
            this.throwUnexpectedToken(this.lookahead);
        }
        else {
            const callee = this.isolateCoverGrammar(this.parseLeftHandSideExpression);
            const args = this.match('(') ? this.parseArguments() : [];
            expr = new NewExpression(callee, args);
            this.context.isAssignmentTarget = false;
            this.context.isBindingElement = false;
        }

        return this.finalize(node, expr);
    }

    parseAsyncArgument() {
        const arg = this.parseAssignmentExpression();
        this.context.firstCoverInitializedNameError = null;
        return arg;
    }

    parseAsyncArguments(): ArgumentListElement[] {
        this.expect('(');
        const args: ArgumentListElement[] = [];
        if (!this.match(')')) {
            // eslint-disable-next-line no-constant-condition
            while (true) {
                const expr = this.match('...') ? this.parseSpreadElement() :
                    this.isolateCoverGrammar(this.parseAsyncArgument);
                args.push(expr);
                if (this.match(')')) {
                    break;
                }
                this.expectCommaSeparator();
                if (this.match(')')) {
                    break;
                }
            }
        }
        this.expect(')');

        return args;
    }

    matchImportCall(): boolean {
        let match = this.matchKeyword('import');
        if (match) {
            const state = this.scanner.saveState();
            this.scanner.scanComments();
            const next = this.scanner.lex();
            this.scanner.restoreState(state);
            match = (next.type === TokenKind.Punctuator) && (next.value === '(');
        }

        return match;
    }

    parseImportCall(): Import {
        const marker = this.createMarker();
        this.expectKeyword('import');
        return this.finalize(marker, new Import());
    }

    parseLeftHandSideExpressionAllowCall(): Expression {
        const startToken = this.lookahead;
        const maybeAsync = this.matchContextualKeyword('async');

        const previousAllowIn = this.context.allowIn;
        this.context.allowIn = true;

        let expr: Expression;
        if (this.matchKeyword('super') && this.context.inFunctionBody) {
            const marker = this.createMarker();
            this.nextToken();
            expr = this.finalize(marker, new Super());
            if (!this.match('(') && !this.match('.') && !this.match('[')) {
                this.throwUnexpectedToken(this.lookahead);
            }
        }
        else {
            expr = this.inheritCoverGrammar(this.matchKeyword('new') ? this.parseNewExpression : this.parsePrimaryExpression);
        }

        // eslint-disable-next-line no-constant-condition
        while (true) {
            if (this.match('.')) {
                this.context.isBindingElement = false;
                this.context.isAssignmentTarget = true;
                this.expect('.');
                const property = this.parseIdentifierName();
                expr = this.finalize(this.tokenMarker(startToken), new MemberExpression(expr, property, false));

            }
            else if (this.match('(')) {
                const asyncArrow = maybeAsync && (startToken.lineNumber === this.lookahead.lineNumber);
                this.context.isBindingElement = false;
                this.context.isAssignmentTarget = false;
                const args = asyncArrow ? this.parseAsyncArguments() : this.parseArguments();
                if (expr.type === Syntax.Import && args.length !== 1) {
                    this.tolerateError(Messages.BadImportCallArity);
                }
                expr = this.finalize(this.tokenMarker(startToken), new CallExpression(expr, args));
                if (asyncArrow && this.match('=>')) {
                    for (let i = 0; i < args.length; ++i) {
                        args[i] = this.reinterpretExpressionAsPattern(args[i]);
                    }
                    expr = new ArrowParameterPlaceHolder(args, true);
                }
            }
            else if (this.match('[')) {
                this.context.isBindingElement = false;
                this.context.isAssignmentTarget = true;
                this.expect('[');
                const property = this.isolateCoverGrammar(this.parseExpression);
                this.expect(']');
                expr = this.finalize(this.tokenMarker(startToken), new MemberExpression(expr, property, true));

            }
            else if (this.lookahead.type === TokenKind.Template && this.lookahead.head) {
                const quasi = this.parseTemplateLiteral();
                expr = this.finalize(this.tokenMarker(startToken), new TaggedTemplateExpression(expr, quasi));

            }
            else {
                break;
            }
        }
        this.context.allowIn = previousAllowIn;

        return expr;
    }

    parseSuper(): Super {
        const node = this.createMarker();

        this.expectKeyword('super');
        if (!this.match('[') && !this.match('.')) {
            this.throwUnexpectedToken(this.lookahead);
        }

        return this.finalize(node, new Super());
    }

    parseLeftHandSideExpression(): Expression {
        assert(this.context.allowIn, 'callee of new expression always allow in keyword.');

        const node = this.tokenMarker(this.lookahead);
        let expr: Expression = (this.matchKeyword('super') && this.context.inFunctionBody) ? this.parseSuper() :
            this.inheritCoverGrammar(this.matchKeyword('new') ? this.parseNewExpression : this.parsePrimaryExpression);

        // eslint-disable-next-line no-constant-condition
        while (true) {
            if (this.match('[')) {
                this.context.isBindingElement = false;
                this.context.isAssignmentTarget = true;
                this.expect('[');
                const property = this.isolateCoverGrammar(this.parseExpression);
                this.expect(']');
                expr = this.finalize(node, new MemberExpression(expr, property, true));

            }
            else if (this.match('.')) {
                this.context.isBindingElement = false;
                this.context.isAssignmentTarget = true;
                this.expect('.');
                const property = this.parseIdentifierName();
                expr = this.finalize(node, new MemberExpression(expr, property, false));

            }
            else if (this.lookahead.type === TokenKind.Template && this.lookahead.head) {
                const quasi = this.parseTemplateLiteral();
                expr = this.finalize(node, new TaggedTemplateExpression(expr, quasi));

            }
            else {
                break;
            }
        }

        return expr;
    }

    // https://tc39.github.io/ecma262/#sec-update-expressions

    parseUpdateExpression(): Expression {
        const startToken = this.lookahead;

        if (this.match('++') || this.match('--')) {
            const node = this.tokenMarker(startToken);
            const token = this.nextToken();
            const temp = this.inheritCoverGrammar(this.parseUnaryExpression);
            if (this.context.strict && is_identifier(temp) && this.scanner.isRestrictedWord(temp.name)) {
                this.tolerateError(Messages.StrictLHSPrefix);
            }
            if (!this.context.isAssignmentTarget) {
                this.tolerateError(Messages.InvalidLHSInAssignment);
            }
            const prefix = true;
            const expr = this.finalize(node, new UpdateExpression(as_string(token.value), temp, prefix));
            this.context.isAssignmentTarget = false;
            this.context.isBindingElement = false;
            return expr;
        }
        else {
            let expr = this.inheritCoverGrammar(this.parseLeftHandSideExpressionAllowCall);
            if (!this.hasLineTerminator && this.lookahead.type === TokenKind.Punctuator) {
                if (this.match('++') || this.match('--')) {
                    if (this.context.strict && is_identifier(expr) && this.scanner.isRestrictedWord(expr.name)) {
                        this.tolerateError(Messages.StrictLHSPostfix);
                    }
                    if (!this.context.isAssignmentTarget) {
                        this.tolerateError(Messages.InvalidLHSInAssignment);
                    }
                    this.context.isAssignmentTarget = false;
                    this.context.isBindingElement = false;
                    const operator = this.nextToken().value;
                    const prefix = false;
                    expr = this.finalize(this.tokenMarker(startToken), new UpdateExpression(as_string(operator), expr, prefix));
                }
            }
            return expr;
        }
    }

    // https://tc39.github.io/ecma262/#sec-unary-operators

    parseAwaitExpression(): AwaitExpression {
        const node = this.createMarker();
        this.nextToken();
        const argument = this.parseUnaryExpression();
        return this.finalize(node, new AwaitExpression(argument));
    }

    parseUnaryExpression(): Expression {

        if (this.match('+') || this.match('-') || this.match('~') || this.match('!') ||
            this.matchKeyword('delete') || this.matchKeyword('void') || this.matchKeyword('typeof')) {
            const node = this.tokenMarker(this.lookahead);
            const token = this.nextToken();
            const argument = this.inheritCoverGrammar(this.parseUnaryExpression);
            const expr = this.finalize(node, new UnaryExpression(as_string(token.value), argument));
            if (this.context.strict && expr.operator === 'delete' && is_identifier(argument)) {
                this.tolerateError(Messages.StrictDelete);
            }
            this.context.isAssignmentTarget = false;
            this.context.isBindingElement = false;
            return expr;
        }
        else if (this.context.await && this.matchContextualKeyword('await')) {
            return this.parseAwaitExpression();
        }
        else {
            return this.parseUpdateExpression();
        }
    }

    parseExponentiationExpression(): Expression {
        const startToken = this.lookahead;

        let expr = this.inheritCoverGrammar(this.parseUnaryExpression);
        if (expr.type !== Syntax.UnaryExpression && this.match('**')) {
            this.nextToken();
            this.context.isAssignmentTarget = false;
            this.context.isBindingElement = false;
            const left = expr;
            const right = this.isolateCoverGrammar(this.parseExponentiationExpression);
            expr = this.finalize(this.tokenMarker(startToken), new BinaryExpression('**', left, right));
        }

        return expr;
    }

    // https://tc39.github.io/ecma262/#sec-exp-operator
    // https://tc39.github.io/ecma262/#sec-multiplicative-operators
    // https://tc39.github.io/ecma262/#sec-additive-operators
    // https://tc39.github.io/ecma262/#sec-bitwise-shift-operators
    // https://tc39.github.io/ecma262/#sec-relational-operators
    // https://tc39.github.io/ecma262/#sec-equality-operators
    // https://tc39.github.io/ecma262/#sec-binary-bitwise-operators
    // https://tc39.github.io/ecma262/#sec-binary-logical-operators

    binaryPrecedence(token: RawToken): number {
        const op = as_string(token.value);
        let precedence;
        if (token.type === TokenKind.Punctuator) {
            precedence = this.operatorPrecedence[op] || 0;
        }
        else if (token.type === TokenKind.Keyword) {
            precedence = (op === 'instanceof' || (this.context.allowIn && op === 'in')) ? 7 : 0;
        }
        else {
            precedence = 0;
        }
        return precedence;
    }

    parseBinaryExpression(): Expression {
        const startToken = this.lookahead;

        let expr = this.inheritCoverGrammar(this.parseExponentiationExpression);

        const token = this.lookahead;
        let prec = this.binaryPrecedence(token);
        if (prec > 0) {
            this.nextToken();

            this.context.isAssignmentTarget = false;
            this.context.isBindingElement = false;

            const markers = [startToken, this.lookahead];
            let left = expr;
            let right = this.isolateCoverGrammar(this.parseExponentiationExpression);

            const stack = [left, token.value, right];
            const precedences: number[] = [prec];
            // eslint-disable-next-line no-constant-condition
            while (true) {
                prec = this.binaryPrecedence(this.lookahead);
                if (prec <= 0) {
                    break;
                }

                // Reduce: make a binary expression from the three topmost entries.
                while ((stack.length > 2) && (prec <= precedences[precedences.length - 1])) {
                    right = stack.pop() as Expression;
                    const operator: ReaderEntry = assert_raw_token_value(stack.pop());
                    precedences.pop();
                    left = stack.pop() as Expression;
                    markers.pop();
                    const node = this.tokenMarker(markers[markers.length - 1]);
                    stack.push(this.finalize(node, new BinaryExpression(as_string(operator), left, right)));
                }

                // Shift.
                stack.push(this.nextToken().value);
                precedences.push(prec);
                markers.push(this.lookahead);
                stack.push(this.isolateCoverGrammar(this.parseExponentiationExpression));
            }

            // Final reduce to clean-up the stack.
            let i = stack.length - 1;
            expr = stack[i] as Expression;
            markers.pop();
            while (i > 1) {
                const node = this.tokenMarker(markers.pop()!);
                const operator: ReaderEntry = assert_raw_token_value(stack[i - 1]);
                expr = this.finalize(node, new BinaryExpression(as_string(operator), stack[i - 2] as Expression, expr));
                i -= 2;
            }
        }

        return expr;
    }

    // https://tc39.github.io/ecma262/#sec-conditional-operator

    parseConditionalExpression(): Expression {
        const startToken = this.lookahead;

        const binExpr = this.inheritCoverGrammar(this.parseBinaryExpression);
        if (this.match('?')) {
            this.nextToken();

            const previousAllowIn = this.context.allowIn;
            this.context.allowIn = true;
            const consequent = this.isolateCoverGrammar(this.parseAssignmentExpression);
            this.context.allowIn = previousAllowIn;

            this.expect(':');
            const alternate = this.isolateCoverGrammar(this.parseAssignmentExpression);

            const expr = this.finalize(this.tokenMarker(startToken), new ConditionalExpression(binExpr, consequent, alternate));
            this.context.isAssignmentTarget = false;
            this.context.isBindingElement = false;
            return expr;
        }
        else {
            return binExpr;
        }
    }

    // https://tc39.github.io/ecma262/#sec-assignment-operators

    checkPatternParam(options: FormalParameters, param: ArrayPatternElement | Param | Identifier | Expression) {
        if (is_array_pattern(param)) {
            for (let i = 0; i < param.elements.length; i++) {
                if (param.elements[i] !== null) {
                    this.checkPatternParam(options, param.elements[i]);
                }
            }
        }
        else if (is_assignment_pattern(param)) {
            this.checkPatternParam(options, param.left);
        }
        else if (is_identifier(param)) {
            // TODO: The param argument does not match.
            throw new Error();
            // this.validateParam(options, param, param.name);
        }
        else if (is_rest_element(param)) {
            this.checkPatternParam(options, param.argument);
        }
        else if (is_rest_property(param)) {
            this.checkPatternParam(options, param.argument);
        }
        else if (is_object_pattern(param)) {
            for (let i = 0; i < param.properties.length; i++) {
                const property = param.properties[i];
                if (is_rest_property(property)) {
                    this.checkPatternParam(options, property);
                }
                else {
                    // this.checkPatternParam(options, property.value);
                    // What are we missing?
                    throw new Error(`${param.type}`);
                }
            }
        }
        options.simple = options.simple && is_identifier(param);
    }

    reinterpretAsCoverParams(expr: Expression): [params: Expression[], asyncArrow: boolean] {
        if (is_identifier(expr)) {
            return [[expr], false];
        }
        else if (is_arrow_parameter_placeholder(expr)) {
            return [expr.params, expr.async];
        }
        else {
            throw new Error();
        }
    }

    reinterpretAsCoverFormalsList(expr: Expression): FormalParameters | null {

        if (is_identifier(expr)) {
            // Fall through
        }
        else if (is_arrow_parameter_placeholder(expr)) {
            // Fall through
        }
        else {
            return null;
        }

        const [params, asyncArrow] = this.reinterpretAsCoverParams(expr);

        const options: FormalParameters = {
            simple: true,
            params: [],
            paramSet: {}
        };

        for (let i = 0; i < params.length; ++i) {
            const param = params[i];
            if (is_assignment_pattern(param)) {
                if (is_yield_expression(param.right)) {
                    if (param.right.argument) {
                        this.throwUnexpectedToken(this.lookahead);
                    }
                    params[i] = new AssignmentPattern(param.left, new Identifier('yield'));
                    /*
                    param.right = new Identifier('yield');
                    param.right.type = Syntax.Identifier;
                    param.right.name = 'yield';
                    delete param.right.argument;
                    delete param.right.delegate;
                    */
                }
                this.checkPatternParam(options, param);
            }
            else if (asyncArrow && is_identifier(param) && param.name === 'await') {
                this.throwUnexpectedToken(this.lookahead);
            }
            else {
                this.checkPatternParam(options, param);
            }
            params[i] = param;
        }

        if (this.context.strict || !this.context.allowYield) {
            for (let i = 0; i < params.length; ++i) {
                const param = params[i];
                if (is_yield_expression(param)) {
                    this.throwUnexpectedToken(this.lookahead);
                }
            }
        }

        if (options.message === Messages.StrictParamDupe) {
            const token = this.context.strict ? options.stricted : options.firstRestricted;
            this.throwUnexpectedToken(token, options.message);
        }

        return {
            simple: options.simple,
            params: assert_function_parameters(params),
            paramSet: options.paramSet,
            stricted: options.stricted,
            firstRestricted: options.firstRestricted,
            message: options.message
        };
    }

    parseAssignmentExpression(): Expression {
        if (!this.context.allowYield && this.matchKeyword('yield')) {
            return this.parseYieldExpression();
        }
        else {
            let expr: Expression;
            const startToken = this.lookahead;
            let token = startToken;
            expr = this.parseConditionalExpression();

            if (token.type === TokenKind.Identifier && (token.lineNumber === this.lookahead.lineNumber) && token.value === 'async') {
                if (this.lookahead.type === TokenKind.Identifier || this.matchKeyword('yield')) {
                    const arg = this.parsePrimaryExpression();
                    const pattern = this.reinterpretExpressionAsPattern(arg);
                    expr = new ArrowParameterPlaceHolder([pattern], true);
                }
            }

            if (is_arrow_parameter_placeholder(expr) || this.match('=>')) {

                // https://tc39.github.io/ecma262/#sec-arrow-function-definitions
                this.context.isAssignmentTarget = false;
                this.context.isBindingElement = false;
                const isAsync = is_arrow_parameter_placeholder(expr) ? expr.async : false;
                const list = this.reinterpretAsCoverFormalsList(expr);

                if (list) {
                    if (this.hasLineTerminator) {
                        this.tolerateUnexpectedToken(this.lookahead);
                    }
                    this.context.firstCoverInitializedNameError = null;

                    const previousStrict = this.context.strict;
                    const previousAllowStrictDirective = this.context.allowStrictDirective;
                    this.context.allowStrictDirective = list.simple;

                    const previousAllowYield = this.context.allowYield;
                    const previousAwait = this.context.await;
                    this.context.allowYield = true;
                    this.context.await = isAsync;

                    const node = this.tokenMarker(startToken);
                    this.expect('=>');
                    const body = this.match('{') ? this.parseFunctionSourceElements() : this.isolateCoverGrammar(this.parseAssignmentExpression);
                    const expression = body.type !== Syntax.BlockStatement;

                    if (this.context.strict && list.firstRestricted) {
                        this.throwUnexpectedToken(list.firstRestricted, list.message);
                    }
                    if (this.context.strict && list.stricted) {
                        this.tolerateUnexpectedToken(list.stricted, list.message);
                    }
                    expr = isAsync ? this.finalize(node, new AsyncArrowFunctionExpression(list.params, body, expression)) :
                        this.finalize(node, new ArrowFunctionExpression(list.params, body, expression));

                    this.context.strict = previousStrict;
                    this.context.allowStrictDirective = previousAllowStrictDirective;
                    this.context.allowYield = previousAllowYield;
                    this.context.await = previousAwait;
                }
            }
            else {
                if (this.matchAssign()) {
                    if (!this.context.isAssignmentTarget) {
                        this.tolerateError(Messages.InvalidLHSInAssignment);
                    }

                    if (this.context.strict && is_identifier(expr)) {
                        if (this.scanner.isRestrictedWord(expr.name)) {
                            this.tolerateUnexpectedToken(token, Messages.StrictLHSAssignment);
                        }
                        if (this.scanner.isStrictModeReservedWord(expr.name)) {
                            this.tolerateUnexpectedToken(token, Messages.StrictReservedWord);
                        }
                    }

                    if (!this.match('=')) {
                        this.context.isAssignmentTarget = false;
                        this.context.isBindingElement = false;
                    }
                    else {
                        expr = this.reinterpretExpressionAsPattern(expr);
                    }

                    token = this.nextToken();
                    const operator = token.value as string;
                    const right = this.isolateCoverGrammar(this.parseAssignmentExpression);
                    expr = this.finalize(this.tokenMarker(startToken), new AssignmentExpression(operator, expr, right));
                    this.context.firstCoverInitializedNameError = null;
                }
            }
            return expr;
        }
    }

    // https://tc39.github.io/ecma262/#sec-comma-operator

    parseExpression(): Expression | SequenceExpression {
        const startToken = this.lookahead;
        let expr = this.isolateCoverGrammar(this.parseAssignmentExpression);

        if (this.match(',')) {
            const expressions: Expression[] = [];
            expressions.push(expr);
            while (this.lookahead.type !== TokenKind.EOF) {
                if (!this.match(',')) {
                    break;
                }
                this.nextToken();
                expressions.push(this.isolateCoverGrammar(this.parseAssignmentExpression));
            }

            expr = this.finalize(this.tokenMarker(startToken), new SequenceExpression(expressions));
        }

        return expr;
    }

    // https://tc39.github.io/ecma262/#sec-block

    parseStatementListItem(): ExportDeclaration | ExpressionStatement | VariableDeclaration | FunctionDeclaration | ClassDeclaration | Statement {
        const previousIsAssignmentTarget = this.context.isAssignmentTarget;
        const previousIsBindingElement = this.context.isBindingElement;
        this.context.isAssignmentTarget = true;
        this.context.isBindingElement = true;
        try {
            if (this.lookahead.type === TokenKind.Keyword) {
                switch (this.lookahead.value) {
                    case 'export':
                        if (!this.context.isModule) {
                            this.tolerateUnexpectedToken(this.lookahead, Messages.IllegalExportDeclaration);
                        }
                        return this.parseExportDeclaration();
                    case 'import':
                        if (this.matchImportCall()) {
                            return this.parseExpressionStatement();
                        }
                        else {
                            if (!this.context.isModule) {
                                this.tolerateUnexpectedToken(this.lookahead, Messages.IllegalImportDeclaration);
                            }
                            return this.parseImportDeclaration();
                        }
                    case 'const':
                        return this.parseLexicalDeclaration({ inFor: false });
                    case 'function':
                        return this.parseFunctionDeclaration();
                    case 'class':
                        return this.parseClassDeclaration();
                    case 'let':
                        return this.isLexicalDeclaration() ? this.parseLexicalDeclaration({ inFor: false }) : this.parseStatement();
                    default:
                        return this.parseStatement();
                }
            }
            else {
                return this.parseStatement();
            }
        }
        finally {
            this.context.isAssignmentTarget = previousIsAssignmentTarget;
            this.context.isBindingElement = previousIsBindingElement;
        }
    }

    parseBlock(): BlockStatement {
        const node = this.createMarker();

        this.expect('{');
        const block: StatementListItem[] = [];
        // eslint-disable-next-line no-constant-condition
        while (true) {
            if (this.match('}')) {
                break;
            }
            block.push(this.parseStatementListItem());
        }
        this.expect('}');

        return this.finalize(node, new BlockStatement(block));
    }

    // https://tc39.github.io/ecma262/#sec-let-and-const-declarations

    parseLexicalBinding(kind: 'const' | 'let', options: { inFor: boolean }): VariableDeclarator {
        const node = this.createMarker();
        const params: RawToken[] = [];
        const id = this.parsePattern(params, kind);

        if (this.context.strict && is_identifier(id)) {
            if (this.scanner.isRestrictedWord(id.name)) {
                this.tolerateError(Messages.StrictVarName);
            }
        }

        let init: Expression | null = null;
        if (kind === 'const') {
            if (!this.matchKeyword('in') && !this.matchContextualKeyword('of')) {
                if (this.match('=')) {
                    this.nextToken();
                    init = this.isolateCoverGrammar(this.parseAssignmentExpression);
                }
                else {
                    this.throwError(Messages.DeclarationMissingInitializer, 'const');
                }
            }
        }
        else if ((!options.inFor && id.type !== Syntax.Identifier) || this.match('=')) {
            this.expect('=');
            init = this.isolateCoverGrammar(this.parseAssignmentExpression);
        }

        return this.finalize(node, new VariableDeclarator(id, init));
    }

    parseBindingList(kind: 'const' | 'let', options: { inFor: boolean }): VariableDeclarator[] {
        const list = [this.parseLexicalBinding(kind, options)];

        while (this.match(',')) {
            this.nextToken();
            list.push(this.parseLexicalBinding(kind, options));
        }

        return list;
    }

    isLexicalDeclaration(): boolean {
        const state = this.scanner.saveState();
        this.scanner.scanComments();
        const next = this.scanner.lex();
        this.scanner.restoreState(state);

        return (next.type === TokenKind.Identifier) ||
            (next.type === TokenKind.Punctuator && next.value === '[') ||
            (next.type === TokenKind.Punctuator && next.value === '{') ||
            (next.type === TokenKind.Keyword && next.value === 'let') ||
            (next.type === TokenKind.Keyword && next.value === 'yield');
    }

    /**
     * Handle the case when this.nextToken().value is 'const' or 'let'.
     */
    parseLexicalDeclaration(options: { inFor: boolean }): VariableDeclaration {
        const node = this.createMarker();
        const kind = this.nextToken().value as 'const' | 'let';
        assert(kind === 'let' || kind === 'const', 'Lexical declaration must be either let or const');

        const declarations = this.parseBindingList(kind, options);
        this.consumeSemicolon();

        return this.finalize(node, new VariableDeclaration(declarations, kind));
    }

    // https://tc39.github.io/ecma262/#sec-destructuring-binding-patterns

    /**
     * A side-effect is to push the lookahead token into `params`.
     */
    parseBindingRestElement(params: RawToken[], kind?: 'const' | 'let' | 'var'): RestElement {
        const node = this.createMarker();

        this.expect('...');
        const arg = this.parsePattern(params, kind);

        return this.finalize(node, new RestElement(arg));
    }

    /**
     * A side-effect is to push the lookahead token into `params`.
     */
    parseArrayPattern(params: RawToken[], kind?: 'const' | 'let' | 'var'): ArrayPattern {
        const node = this.createMarker();

        this.expect('[');
        const elements: ArrayPatternElement[] = [];
        while (!this.match(']')) {
            if (this.match(',')) {
                this.nextToken();
                elements.push(null);
            }
            else {
                if (this.match('...')) {
                    elements.push(this.parseBindingRestElement(params, kind));
                    break;
                }
                else {
                    elements.push(this.parsePatternWithDefault(params, kind));
                }
                if (!this.match(']')) {
                    this.expect(',');
                }
            }

        }
        this.expect(']');

        return this.finalize(node, new ArrayPattern(elements));
    }

    /**
     * A side-effect is to push the lookahead token into `params`.
     */
    parsePropertyPattern(params: RawToken[], kind?: 'const' | 'let' | 'var'): Property {
        const node = this.createMarker();

        let computed = false;
        let shorthand = false;
        const method = false;

        let value: PropertyValue;

        if (this.lookahead.type === TokenKind.Identifier) {
            const keyToken = this.lookahead;
            const key = this.parseVariableIdentifier();
            const init = this.finalize(node, new Identifier(as_string(keyToken.value)));
            if (this.match('=')) {
                params.push(keyToken);
                shorthand = true;
                this.nextToken();
                const expr = this.parseAssignmentExpression();
                value = this.finalize(this.tokenMarker(keyToken), new AssignmentPattern(init, expr));
            }
            else if (!this.match(':')) {
                params.push(keyToken);
                shorthand = true;
                value = init;
            }
            else {
                this.expect(':');
                value = this.parsePatternWithDefault(params, kind);
            }
            return this.finalize(node, new Property('init', key, computed, value, method, shorthand));
        }
        else {
            computed = this.match('[');
            const key = this.parseObjectPropertyKey();
            this.expect(':');
            value = this.parsePatternWithDefault(params, kind);
            return this.finalize(node, new Property('init', assert_property_key(key), computed, value, method, shorthand));
        }

    }

    /**
     * A side-effect is to push the lookahead token into `params`.
     */
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    parseRestProperty(params: RawToken[], _kind?: 'const' | 'let' | 'var'): RestProperty {
        const marker = this.createMarker();
        this.expect('...');
        const arg = this.parsePattern(params);
        if (this.match('=')) {
            this.throwError(Messages.DefaultRestProperty);
        }
        if (!this.match('}')) {
            this.throwError(Messages.PropertyAfterRestProperty);
        }
        return this.finalize(marker, new RestProperty(arg));
    }

    /**
     * A side-effect is to push the lookahead token into `params`.
     */
    parseObjectPattern(params: RawToken[], kind?: 'const' | 'let' | 'var'): ObjectPattern {
        const node = this.createMarker();
        const properties: ObjectPatternProperty[] = [];

        this.expect('{');
        while (!this.match('}')) {
            properties.push(this.match('...') ? this.parseRestProperty(params, kind) : this.parsePropertyPattern(params, kind));
            if (!this.match('}')) {
                this.expect(',');
            }
        }
        this.expect('}');

        return this.finalize(node, new ObjectPattern(properties));
    }

    /**
     * A side-effect is to push the lookahead token into `params`.
     */
    parsePattern(params: RawToken[], kind?: 'const' | 'let' | 'var'): ArrayPattern | Identifier | ObjectPattern {
        if (this.match('[')) {
            return this.parseArrayPattern(params, kind);
        }
        else if (this.match('{')) {
            return this.parseObjectPattern(params, kind);
        }
        else {
            if (this.matchKeyword('let') && (kind === 'const' || kind === 'let')) {
                this.tolerateUnexpectedToken(this.lookahead, Messages.LetInLexicalBinding);
            }
            params.push(this.lookahead);
            return this.parseVariableIdentifier(kind);
        }
    }

    /**
     * A side-effect is to push the lookahead token into `params`.
     */
    parsePatternWithDefault(params: RawToken[], kind?: 'const' | 'let' | 'var'): ArrayPattern | AssignmentPattern | Identifier | ObjectPattern {
        const startToken = this.lookahead;
        const pattern = this.parsePattern(params, kind);
        if (this.match('=')) {
            this.nextToken();
            const previousAllowYield = this.context.allowYield;
            this.context.allowYield = true;
            const right = this.isolateCoverGrammar(this.parseAssignmentExpression);
            this.context.allowYield = previousAllowYield;
            return this.finalize(this.tokenMarker(startToken), new AssignmentPattern(pattern, right));
        }
        else {
            return pattern;
        }
    }

    // https://tc39.github.io/ecma262/#sec-variable-statement

    parseVariableIdentifier(kind?: 'const' | 'let' | 'var'): Identifier {
        const node = this.createMarker();

        const token = this.nextToken();
        if (token.type === TokenKind.Keyword && token.value === 'yield') {
            if (this.context.strict) {
                this.tolerateUnexpectedToken(token, Messages.StrictReservedWord);
            }
            else if (!this.context.allowYield) {
                this.throwUnexpectedToken(token);
            }
        }
        else if (token.type !== TokenKind.Identifier) {
            if (this.context.strict && token.type === TokenKind.Keyword && this.scanner.isStrictModeReservedWord(token.value as string)) {
                this.tolerateUnexpectedToken(token, Messages.StrictReservedWord);
            }
            else {
                if (this.context.strict || token.value !== 'let' || kind !== 'var') {
                    this.throwUnexpectedToken(token);
                }
            }
        }
        else if ((this.context.isModule || this.context.await) && token.type === TokenKind.Identifier && token.value === 'await') {
            this.tolerateUnexpectedToken(token);
        }

        return this.finalize(node, new Identifier(as_string(token.value)));
    }

    parseVariableDeclaration(options: DeclarationOptions): VariableDeclarator {
        const node = this.createMarker();

        const params: RawToken[] = [];
        const id = this.parsePattern(params, 'var');

        if (this.context.strict && is_identifier(id)) {
            if (this.scanner.isRestrictedWord(id.name)) {
                this.tolerateError(Messages.StrictVarName);
            }
        }

        let init: Expression | null = null;
        if (this.match('=')) {
            this.nextToken();
            init = this.isolateCoverGrammar(this.parseAssignmentExpression);
        }
        else if (id.type !== Syntax.Identifier && !options.inFor) {
            this.expect('=');
        }

        return this.finalize(node, new VariableDeclarator(id, init));
    }

    parseVariableDeclarationList(options: { inFor: boolean }): VariableDeclarator[] {
        const opt: DeclarationOptions = { inFor: options.inFor };

        const list: VariableDeclarator[] = [];
        list.push(this.parseVariableDeclaration(opt));
        while (this.match(',')) {
            this.nextToken();
            list.push(this.parseVariableDeclaration(opt));
        }

        return list;
    }

    parseVariableStatement(): VariableDeclaration {
        const node = this.createMarker();
        this.expectKeyword('var');
        const declarations = this.parseVariableDeclarationList({ inFor: false });
        this.consumeSemicolon();

        return this.finalize(node, new VariableDeclaration(declarations, 'var'));
    }

    // https://tc39.github.io/ecma262/#sec-empty-statement

    parseEmptyStatement(): EmptyStatement {
        const node = this.createMarker();
        this.expect(';');
        return this.finalize(node, new EmptyStatement());
    }

    // https://tc39.github.io/ecma262/#sec-expression-statement

    parseExpressionStatement(): ExpressionStatement {
        const node = this.createMarker();
        const expr = this.parseExpression();
        this.consumeSemicolon();
        return this.finalize(node, new ExpressionStatement(expr));
    }

    // https://tc39.github.io/ecma262/#sec-if-statement

    parseIfClause(): Statement {
        if (this.context.strict && this.matchKeyword('function')) {
            this.tolerateError(Messages.StrictFunction);
        }
        return this.parseStatement();
    }

    parseIfStatement(): IfStatement {
        const node = this.createMarker();
        let consequent: Statement;
        let alternate: Statement | null = null;

        this.expectKeyword('if');
        this.expect('(');
        const test = this.parseExpression();

        if (!this.match(')') && this.config.tolerant) {
            this.tolerateUnexpectedToken(this.nextToken());
            consequent = this.finalize(this.createMarker(), new EmptyStatement());
        }
        else {
            this.expect(')');
            consequent = this.parseIfClause();
            if (this.matchKeyword('else')) {
                this.nextToken();
                alternate = this.parseIfClause();
            }
        }

        return this.finalize(node, new IfStatement(test, consequent, alternate));
    }

    // https://tc39.github.io/ecma262/#sec-do-while-statement

    parseDoWhileStatement(): DoWhileStatement {
        const node = this.createMarker();
        this.expectKeyword('do');

        const previousInIteration = this.context.inIteration;
        this.context.inIteration = true;
        const body = this.parseStatement();
        this.context.inIteration = previousInIteration;

        this.expectKeyword('while');
        this.expect('(');
        const test = this.parseExpression();

        if (!this.match(')') && this.config.tolerant) {
            this.tolerateUnexpectedToken(this.nextToken());
        }
        else {
            this.expect(')');
            if (this.match(';')) {
                this.nextToken();
            }
        }

        return this.finalize(node, new DoWhileStatement(body, test));
    }

    // https://tc39.github.io/ecma262/#sec-while-statement

    parseWhileStatement(): WhileStatement {
        const node = this.createMarker();
        let body;

        this.expectKeyword('while');
        this.expect('(');
        const test = this.parseExpression();

        if (!this.match(')') && this.config.tolerant) {
            this.tolerateUnexpectedToken(this.nextToken());
            body = this.finalize(this.createMarker(), new EmptyStatement());
        }
        else {
            this.expect(')');

            const previousInIteration = this.context.inIteration;
            this.context.inIteration = true;
            body = this.parseStatement();
            this.context.inIteration = previousInIteration;
        }

        return this.finalize(node, new WhileStatement(test, body));
    }

    // https://tc39.github.io/ecma262/#sec-for-statement
    // https://tc39.github.io/ecma262/#sec-for-in-and-for-of-statements

    parseForStatement(): ForStatement | ForInStatement | ForOfStatement {
        let init: VariableDeclaration | SequenceExpression | Expression | null = null;
        let test: Expression | null = null;
        let update: Expression | null = null;
        let forIn = true;
        let left: Expression | undefined;
        let right: Expression | undefined;

        const node = this.createMarker();
        this.expectKeyword('for');
        this.expect('(');

        if (this.match(';')) {
            this.nextToken();
        }
        else {
            if (this.matchKeyword('var')) {
                const marker = this.createMarker();
                this.nextToken();

                const previousAllowIn = this.context.allowIn;
                this.context.allowIn = false;
                const declarations = this.parseVariableDeclarationList({ inFor: true });
                this.context.allowIn = previousAllowIn;

                if (declarations.length === 1 && this.matchKeyword('in')) {
                    const decl = declarations[0];
                    if (decl.init && (decl.id.type === Syntax.ArrayPattern || decl.id.type === Syntax.ObjectPattern || this.context.strict)) {
                        this.tolerateError(Messages.ForInOfLoopInitializer, 'for-in');
                    }
                    left = this.finalize(marker, new VariableDeclaration(declarations, 'var'));
                    this.nextToken();
                    right = this.parseExpression();
                }
                else if (declarations.length === 1 && declarations[0].init === null && this.matchContextualKeyword('of')) {
                    left = this.finalize(marker, new VariableDeclaration(declarations, 'var'));
                    this.nextToken();
                    right = this.parseAssignmentExpression();
                    forIn = false;
                }
                else {
                    init = this.finalize(marker, new VariableDeclaration(declarations, 'var'));
                    this.expect(';');
                }
            }
            else if (this.matchKeyword('const') || this.matchKeyword('let')) {
                const marker = this.createMarker();
                const kind = this.nextToken().value as 'const' | 'let';

                if (!this.context.strict && this.lookahead.value === 'in') {
                    const temp = this.finalize(marker, new Identifier(kind));
                    this.nextToken();
                    left = temp;
                    right = this.parseExpression();
                    init = null;
                }
                else {
                    const previousAllowIn = this.context.allowIn;
                    this.context.allowIn = false;
                    const declarations = this.parseBindingList(kind, { inFor: true });
                    this.context.allowIn = previousAllowIn;

                    if (declarations.length === 1 && declarations[0].init === null && this.matchKeyword('in')) {
                        init = this.finalize(marker, new VariableDeclaration(declarations, kind));
                        this.nextToken();
                        left = init;
                        right = this.parseExpression();
                        init = null;
                    }
                    else if (declarations.length === 1 && declarations[0].init === null && this.matchContextualKeyword('of')) {
                        init = this.finalize(marker, new VariableDeclaration(declarations, kind));
                        this.nextToken();
                        left = init;
                        right = this.parseAssignmentExpression();
                        init = null;
                        forIn = false;
                    }
                    else {
                        this.consumeSemicolon();
                        init = this.finalize(marker, new VariableDeclaration(declarations, kind));
                    }
                }
            }
            else {
                const initStartToken = this.lookahead;
                const previousAllowIn = this.context.allowIn;
                this.context.allowIn = false;
                const assignExpr = this.inheritCoverGrammar(this.parseAssignmentExpression);
                this.context.allowIn = previousAllowIn;

                if (this.matchKeyword('in')) {
                    if (!this.context.isAssignmentTarget || is_assignment_expression(assignExpr)) {
                        this.tolerateError(Messages.InvalidLHSInForIn);
                    }

                    this.nextToken();
                    left = this.reinterpretExpressionAsPattern(assignExpr);
                    right = this.parseExpression();
                }
                else if (this.matchContextualKeyword('of')) {
                    if (!this.context.isAssignmentTarget || is_assignment_expression(init)) {
                        this.tolerateError(Messages.InvalidLHSInForLoop);
                    }

                    this.nextToken();
                    left = this.reinterpretExpressionAsPattern(assignExpr);
                    right = this.parseAssignmentExpression();
                    forIn = false;
                }
                else {
                    if (this.match(',')) {
                        const initSeq: Expression[] = [assignExpr];
                        while (this.match(',')) {
                            this.nextToken();
                            initSeq.push(this.isolateCoverGrammar(this.parseAssignmentExpression));
                        }
                        init = this.finalize(this.tokenMarker(initStartToken), new SequenceExpression(initSeq));
                    }
                    else {
                        init = assignExpr;
                    }
                    this.expect(';');
                }
            }
        }

        if (typeof left === 'undefined') {
            if (!this.match(';')) {
                test = this.parseExpression();
            }
            this.expect(';');
            if (!this.match(')')) {
                update = this.parseExpression();
            }
        }

        const body = this.parseBodyStatement();

        return (typeof left === 'undefined') ?
            this.finalize(node, new ForStatement(init, test, update, body)) :
            forIn ? this.finalize(node, new ForInStatement(left, right as Expression, body)) :
                this.finalize(node, new ForOfStatement(left, right as Expression, body));
    }

    parseBodyStatement(): Statement {
        if (!this.match(')') && this.config.tolerant) {
            this.tolerateUnexpectedToken(this.nextToken());
            return this.finalize(this.createMarker(), new EmptyStatement());
        }
        else {
            this.expect(')');

            const previousInIteration = this.context.inIteration;
            this.context.inIteration = true;
            try {
                return this.isolateCoverGrammar(this.parseStatement);
            }
            finally {
                this.context.inIteration = previousInIteration;
            }
        }
    }

    // https://tc39.github.io/ecma262/#sec-continue-statement

    parseContinueStatement(): ContinueStatement {
        const node = this.createMarker();
        this.expectKeyword('continue');

        let label: Identifier | null = null;
        if (this.lookahead.type === TokenKind.Identifier && !this.hasLineTerminator) {
            const id = this.parseVariableIdentifier();
            label = id;

            const key = '$' + id.name;
            if (!Object.prototype.hasOwnProperty.call(this.context.labelSet, key)) {
                this.throwError(Messages.UnknownLabel, id.name);
            }
        }

        this.consumeSemicolon();
        if (label === null && !this.context.inIteration) {
            this.throwError(Messages.IllegalContinue);
        }

        return this.finalize(node, new ContinueStatement(label));
    }

    // https://tc39.github.io/ecma262/#sec-break-statement

    parseBreakStatement(): BreakStatement {
        const node = this.createMarker();
        this.expectKeyword('break');

        let label: Identifier | null = null;
        if (this.lookahead.type === TokenKind.Identifier && !this.hasLineTerminator) {
            const id = this.parseVariableIdentifier();

            const key = '$' + id.name;
            if (!Object.prototype.hasOwnProperty.call(this.context.labelSet, key)) {
                this.throwError(Messages.UnknownLabel, id.name);
            }
            label = id;
        }

        this.consumeSemicolon();
        if (label === null && !this.context.inIteration && !this.context.inSwitch) {
            this.throwError(Messages.IllegalBreak);
        }

        return this.finalize(node, new BreakStatement(label));
    }

    // https://tc39.github.io/ecma262/#sec-return-statement

    parseReturnStatement(): ReturnStatement {
        if (!this.context.inFunctionBody) {
            this.tolerateError(Messages.IllegalReturn);
        }

        const node = this.createMarker();
        this.expectKeyword('return');

        const hasArgument = !this.match(';') && !this.match('}') &&
            !this.hasLineTerminator && this.lookahead.type !== TokenKind.EOF;
        const argument = hasArgument ? this.parseExpression() : null;
        this.consumeSemicolon();

        return this.finalize(node, new ReturnStatement(argument));
    }

    // https://tc39.github.io/ecma262/#sec-with-statement

    parseWithStatement(): WithStatement {
        if (this.context.strict) {
            this.tolerateError(Messages.StrictModeWith);
        }

        const node = this.createMarker();
        let body;

        this.expectKeyword('with');
        this.expect('(');
        const object = this.parseExpression();

        if (!this.match(')') && this.config.tolerant) {
            this.tolerateUnexpectedToken(this.nextToken());
            body = this.finalize(this.createMarker(), new EmptyStatement());
        }
        else {
            this.expect(')');
            body = this.parseStatement();
        }

        return this.finalize(node, new WithStatement(object, body));
    }

    // https://tc39.github.io/ecma262/#sec-switch-statement

    parseSwitchCase(): SwitchCase {
        const node = this.createMarker();

        let test;
        if (this.matchKeyword('default')) {
            this.nextToken();
            test = null;
        }
        else {
            this.expectKeyword('case');
            test = this.parseExpression();
        }
        this.expect(':');

        const consequent: StatementListItem[] = [];
        // eslint-disable-next-line no-constant-condition
        while (true) {
            if (this.match('}') || this.matchKeyword('default') || this.matchKeyword('case')) {
                break;
            }
            consequent.push(this.parseStatementListItem());
        }

        return this.finalize(node, new SwitchCase(test, consequent));
    }

    parseSwitchStatement(): SwitchStatement {
        const node = this.createMarker();
        this.expectKeyword('switch');

        this.expect('(');
        const discriminant = this.parseExpression();
        this.expect(')');

        const previousInSwitch = this.context.inSwitch;
        this.context.inSwitch = true;

        const cases: SwitchCase[] = [];
        let defaultFound = false;
        this.expect('{');
        // eslint-disable-next-line no-constant-condition
        while (true) {
            if (this.match('}')) {
                break;
            }
            const clause = this.parseSwitchCase();
            if (clause.test === null) {
                if (defaultFound) {
                    this.throwError(Messages.MultipleDefaultsInSwitch);
                }
                defaultFound = true;
            }
            cases.push(clause);
        }
        this.expect('}');

        this.context.inSwitch = previousInSwitch;

        return this.finalize(node, new SwitchStatement(discriminant, cases));
    }

    // https://tc39.github.io/ecma262/#sec-labelled-statements

    parseLabelledStatement(): LabeledStatement | ExpressionStatement {
        const node = this.createMarker();
        const expr = this.parseExpression();

        let statement: ExpressionStatement | LabeledStatement;
        if (is_identifier(expr) && this.match(':')) {
            this.nextToken();

            const id = expr as Identifier;
            const key = '$' + id.name;
            if (Object.prototype.hasOwnProperty.call(this.context.labelSet, key)) {
                this.throwError(Messages.Redeclaration, 'Label', id.name);
            }

            this.context.labelSet[key] = true;
            let body: Statement;
            if (this.matchKeyword('class')) {
                this.tolerateUnexpectedToken(this.lookahead);
                body = this.parseClassDeclaration();
            }
            else if (this.matchKeyword('function')) {
                const token = this.lookahead;
                const declaration = this.parseFunctionDeclaration();
                if (this.context.strict) {
                    this.tolerateUnexpectedToken(token, Messages.StrictFunction);
                }
                else if (declaration.generator) {
                    this.tolerateUnexpectedToken(token, Messages.GeneratorInLegacyContext);
                }
                body = declaration;
            }
            else {
                body = this.parseStatement();
            }
            delete this.context.labelSet[key];

            statement = new LabeledStatement(id, body);
        }
        else {
            this.consumeSemicolon();
            statement = new ExpressionStatement(expr);
        }

        return this.finalize(node, statement);
    }

    // https://tc39.github.io/ecma262/#sec-throw-statement

    parseThrowStatement(): ThrowStatement {
        const node = this.createMarker();
        this.expectKeyword('throw');

        if (this.hasLineTerminator) {
            this.throwError(Messages.NewlineAfterThrow);
        }

        const argument = this.parseExpression();
        this.consumeSemicolon();

        return this.finalize(node, new ThrowStatement(argument));
    }

    // https://tc39.github.io/ecma262/#sec-try-statement

    parseCatchClause(): CatchClause {
        const node = this.createMarker();

        this.expectKeyword('catch');

        this.expect('(');
        if (this.match(')')) {
            this.throwUnexpectedToken(this.lookahead);
        }

        const params: RawToken[] = [];
        const param = this.parsePattern(params);
        const paramMap: { [key: string]: boolean } = {};
        for (let i = 0; i < params.length; i++) {
            const key = '$' + params[i].value;
            if (Object.prototype.hasOwnProperty.call(paramMap, key)) {
                this.tolerateError(Messages.DuplicateBinding, params[i].value);
            }
            paramMap[key] = true;
        }

        if (this.context.strict && is_identifier(param)) {
            if (this.scanner.isRestrictedWord((param as Identifier).name)) {
                this.tolerateError(Messages.StrictCatchVariable);
            }
        }

        this.expect(')');
        const body = this.parseBlock();

        return this.finalize(node, new CatchClause(param, body));
    }

    parseFinallyClause(): BlockStatement {
        this.expectKeyword('finally');
        return this.parseBlock();
    }

    parseTryStatement(): TryStatement {
        const node = this.createMarker();
        this.expectKeyword('try');

        const block = this.parseBlock();
        const handler = this.matchKeyword('catch') ? this.parseCatchClause() : null;
        const finalizer = this.matchKeyword('finally') ? this.parseFinallyClause() : null;

        if (!handler && !finalizer) {
            this.throwError(Messages.NoCatchOrFinally);
        }

        return this.finalize(node, new TryStatement(block, handler, finalizer));
    }

    // https://tc39.github.io/ecma262/#sec-debugger-statement

    parseDebuggerStatement(): DebuggerStatement {
        const node = this.createMarker();
        this.expectKeyword('debugger');
        this.consumeSemicolon();
        return this.finalize(node, new DebuggerStatement());
    }

    // https://tc39.github.io/ecma262/#sec-ecmascript-language-statements-and-declarations

    parseStatement(): Statement {
        switch (this.lookahead.type) {
            case TokenKind.BooleanLiteral:
            case TokenKind.NullLiteral:
            case TokenKind.NumericLiteral:
            case TokenKind.StringLiteral:
            case TokenKind.Template:
            case TokenKind.RegularExpression: {
                return this.parseExpressionStatement();
            }
            case TokenKind.Punctuator: {
                const value = this.lookahead.value;
                if (value === '{') {
                    return this.parseBlock();
                }
                else if (value === '(') {
                    return this.parseExpressionStatement();
                }
                else if (value === ';') {
                    return this.parseEmptyStatement();
                }
                else {
                    return this.parseExpressionStatement();
                }
            }
            case TokenKind.Identifier: return this.matchAsyncFunction() ? this.parseFunctionDeclaration() : this.parseLabelledStatement();

            case TokenKind.Keyword:
                switch (this.lookahead.value) {
                    case 'break':
                        return this.parseBreakStatement();
                    case 'continue':
                        return this.parseContinueStatement();
                    case 'debugger':
                        return this.parseDebuggerStatement();
                    case 'do':
                        return this.parseDoWhileStatement();
                    case 'for':
                        return this.parseForStatement();
                    case 'function':
                        return this.parseFunctionDeclaration();
                    case 'if':
                        return this.parseIfStatement();
                    case 'return':
                        return this.parseReturnStatement();
                    case 'switch':
                        return this.parseSwitchStatement();
                    case 'throw':
                        return this.parseThrowStatement();
                    case 'try':
                        return this.parseTryStatement();
                    case 'var':
                        return this.parseVariableStatement();
                    case 'while':
                        return this.parseWhileStatement();
                    case 'with':
                        return this.parseWithStatement();
                    default:
                        return this.parseExpressionStatement();
                }
            default:
                return this.throwUnexpectedToken(this.lookahead);
        }
    }

    // https://tc39.github.io/ecma262/#sec-function-definitions

    parseFunctionSourceElements(): BlockStatement {
        const node = this.createMarker();

        this.expect('{');
        const body = this.parseDirectivePrologues();

        const previousLabelSet = this.context.labelSet;
        const previousInIteration = this.context.inIteration;
        const previousInSwitch = this.context.inSwitch;
        const previousInFunctionBody = this.context.inFunctionBody;

        this.context.labelSet = {};
        this.context.inIteration = false;
        this.context.inSwitch = false;
        this.context.inFunctionBody = true;

        while (this.lookahead.type !== TokenKind.EOF) {
            if (this.match('}')) {
                break;
            }
            body.push(this.parseStatementListItem());
        }

        this.expect('}');

        this.context.labelSet = previousLabelSet;
        this.context.inIteration = previousInIteration;
        this.context.inSwitch = previousInSwitch;
        this.context.inFunctionBody = previousInFunctionBody;

        return this.finalize(node, new BlockStatement(body));
    }

    validateParam(options: FormalParameters, param: RawToken, name: string) {
        const key = '$' + name;
        if (this.context.strict) {
            if (this.scanner.isRestrictedWord(name)) {
                options.stricted = param;
                options.message = Messages.StrictParamName;
            }
            if (Object.prototype.hasOwnProperty.call(options.paramSet, key)) {
                options.stricted = param;
                options.message = Messages.StrictParamDupe;
            }
        }
        else if (!options.firstRestricted) {
            if (this.scanner.isRestrictedWord(name)) {
                options.firstRestricted = param;
                options.message = Messages.StrictParamName;
            }
            else if (this.scanner.isStrictModeReservedWord(name)) {
                options.firstRestricted = param;
                options.message = Messages.StrictReservedWord;
            }
            else if (Object.prototype.hasOwnProperty.call(options.paramSet, key)) {
                options.stricted = param;
                options.message = Messages.StrictParamDupe;
            }
        }

        /* istanbul ignore next */
        if (typeof Object.defineProperty === 'function') {
            Object.defineProperty(options.paramSet, key, { value: true, enumerable: true, writable: true, configurable: true });
        }
        else {
            options.paramSet[key] = true;
        }
    }

    parseRestElement(params: RawToken[]): RestElement {
        const node = this.createMarker();

        this.expect('...');
        const arg = this.parsePattern(params);
        if (this.match('=')) {
            this.throwError(Messages.DefaultRestParameter);
        }
        if (!this.match(')')) {
            this.throwError(Messages.ParameterAfterRestParameter);
        }

        return this.finalize(node, new RestElement(arg));
    }

    parseFormalParameter(options: FormalParameters) {
        /**
         * 
         */
        const params: RawToken[] = [];
        const param: FunctionParameter = this.match('...') ? this.parseRestElement(params) : this.parsePatternWithDefault(params);
        for (let i = 0; i < params.length; i++) {
            this.validateParam(options, params[i], as_string(params[i].value));
        }
        options.simple = options.simple && is_identifier(param);
        options.params.push(param);
    }

    parseFormalParameters(firstRestricted?: RawToken): FormalParameters {

        const options: FormalParameters = {
            simple: true,
            params: [],
            paramSet: {},
            firstRestricted: firstRestricted
        };

        this.expect('(');
        if (!this.match(')')) {
            options.paramSet = {};
            while (this.lookahead.type !== TokenKind.EOF) {
                this.parseFormalParameter(options);
                if (this.match(')')) {
                    break;
                }
                this.expect(',');
                if (this.match(')')) {
                    break;
                }
            }
        }
        this.expect(')');

        return {
            simple: options.simple,
            params: options.params,
            paramSet: options.paramSet,
            stricted: options.stricted,
            firstRestricted: options.firstRestricted,
            message: options.message
        };
    }

    matchAsyncFunction(): boolean {
        let match = this.matchContextualKeyword('async');
        if (match) {
            const state = this.scanner.saveState();
            this.scanner.scanComments();
            const next = this.scanner.lex();
            this.scanner.restoreState(state);

            match = (state.lineNumber === next.lineNumber) && (next.type === TokenKind.Keyword) && (next.value === 'function');
        }

        return match;
    }

    parseFunctionDeclaration(identifierIsOptional?: boolean): FunctionDeclaration {
        const previousIsAssignmentTarget = this.context.isAssignmentTarget;
        const previousIsBindingElement = this.context.isBindingElement;
        this.context.isAssignmentTarget = true;
        this.context.isBindingElement = true;
        try {
            const node = this.createMarker();

            const isAsync = this.matchContextualKeyword('async');
            if (isAsync) {
                this.nextToken();
            }

            this.expectKeyword('function');

            const isGenerator = isAsync ? false : this.match('*');
            if (isGenerator) {
                this.nextToken();
            }

            let message;
            let id: Identifier | null = null;
            let firstRestricted: RawToken | undefined;

            if (!identifierIsOptional || !this.match('(')) {
                const token = this.lookahead;
                id = this.parseVariableIdentifier();
                if (this.context.strict) {
                    if (this.scanner.isRestrictedWord(token.value as string)) {
                        this.tolerateUnexpectedToken(token, Messages.StrictFunctionName);
                    }
                }
                else {
                    if (this.scanner.isRestrictedWord(token.value as string)) {
                        firstRestricted = token;
                        message = Messages.StrictFunctionName;
                    }
                    else if (this.scanner.isStrictModeReservedWord(token.value as string)) {
                        firstRestricted = token;
                        message = Messages.StrictReservedWord;
                    }
                }
            }

            const previousAllowAwait = this.context.await;
            const previousAllowYield = this.context.allowYield;
            this.context.await = isAsync;
            this.context.allowYield = !isGenerator;

            const formalParameters = this.parseFormalParameters(firstRestricted);
            const params = formalParameters.params;
            const stricted = formalParameters.stricted;
            firstRestricted = formalParameters.firstRestricted;
            if (formalParameters.message) {
                message = formalParameters.message;
            }

            const previousStrict = this.context.strict;
            const previousAllowStrictDirective = this.context.allowStrictDirective;
            this.context.allowStrictDirective = formalParameters.simple;
            const body = this.parseFunctionSourceElements();
            if (this.context.strict && firstRestricted) {
                this.throwUnexpectedToken(firstRestricted, message);
            }
            if (this.context.strict && stricted) {
                this.tolerateUnexpectedToken(stricted, message);
            }

            this.context.strict = previousStrict;
            this.context.allowStrictDirective = previousAllowStrictDirective;
            this.context.await = previousAllowAwait;
            this.context.allowYield = previousAllowYield;

            return this.finalize(node, new FunctionDeclaration(id, params, body, isGenerator, isAsync));
        }
        finally {
            this.context.isAssignmentTarget = previousIsAssignmentTarget;
            this.context.isBindingElement = previousIsBindingElement;
        }
    }

    parseFunctionExpression(): FunctionExpression {
        const node = this.createMarker();

        const isAsync = this.matchContextualKeyword('async');
        if (isAsync) {
            this.nextToken();
        }

        this.expectKeyword('function');

        const isGenerator = isAsync ? false : this.match('*');
        if (isGenerator) {
            this.nextToken();
        }

        let message;
        let id: Identifier | null = null;
        let firstRestricted;

        const previousAllowAwait = this.context.await;
        const previousAllowYield = this.context.allowYield;
        this.context.await = isAsync;
        this.context.allowYield = !isGenerator;

        if (!this.match('(')) {
            const token = this.lookahead;
            id = (!this.context.strict && !isGenerator && this.matchKeyword('yield')) ? this.parseIdentifierName() : this.parseVariableIdentifier();
            if (this.context.strict) {
                if (this.scanner.isRestrictedWord(token.value as string)) {
                    this.tolerateUnexpectedToken(token, Messages.StrictFunctionName);
                }
            }
            else {
                if (this.scanner.isRestrictedWord(token.value as string)) {
                    firstRestricted = token;
                    message = Messages.StrictFunctionName;
                }
                else if (this.scanner.isStrictModeReservedWord(token.value as string)) {
                    firstRestricted = token;
                    message = Messages.StrictReservedWord;
                }
            }
        }

        const formalParameters = this.parseFormalParameters(firstRestricted);
        const params = formalParameters.params;
        const stricted = formalParameters.stricted;
        firstRestricted = formalParameters.firstRestricted;
        if (formalParameters.message) {
            message = formalParameters.message;
        }

        const previousStrict = this.context.strict;
        const previousAllowStrictDirective = this.context.allowStrictDirective;
        this.context.allowStrictDirective = formalParameters.simple;
        const body = this.parseFunctionSourceElements();
        if (this.context.strict && firstRestricted) {
            this.throwUnexpectedToken(firstRestricted, message);
        }
        if (this.context.strict && stricted) {
            this.tolerateUnexpectedToken(stricted, message);
        }
        this.context.strict = previousStrict;
        this.context.allowStrictDirective = previousAllowStrictDirective;
        this.context.await = previousAllowAwait;
        this.context.allowYield = previousAllowYield;

        return this.finalize(node, new FunctionExpression(id, params, body, isGenerator, isAsync));
    }

    // https://tc39.github.io/ecma262/#sec-directive-prologues-and-the-use-strict-directive

    parseDirective(): Directive | ExpressionStatement {
        const token = this.lookahead;

        const node = this.createMarker();
        const expr = this.parseExpression();
        const directive = is_literal(expr) ? this.getTokenRaw(token).slice(1, -1) : null;
        this.consumeSemicolon();

        return this.finalize(node, directive ? new Directive(expr, directive) : new ExpressionStatement(expr));
    }

    parseDirectivePrologues(): Statement[] {
        let firstRestricted: RawToken | null = null;

        const body: Statement[] = [];
        // eslint-disable-next-line no-constant-condition
        while (true) {
            const token = this.lookahead;
            if (token.type !== TokenKind.StringLiteral) {
                break;
            }

            const statement = this.parseDirective();
            body.push(statement);
            const directive = (statement as Directive).directive;
            if (typeof directive !== 'string') {
                break;
            }

            if (directive === 'use strict') {
                this.context.strict = true;
                if (firstRestricted) {
                    this.tolerateUnexpectedToken(firstRestricted, Messages.StrictOctalLiteral);
                }
                if (!this.context.allowStrictDirective) {
                    this.tolerateUnexpectedToken(token, Messages.IllegalLanguageModeDirective);
                }
            }
            else {
                if (!firstRestricted && token.octal) {
                    firstRestricted = token;
                }
            }
        }

        return body;
    }

    // https://tc39.github.io/ecma262/#sec-method-definitions

    qualifiedPropertyName(token: RawToken): boolean {
        switch (token.type) {
            case TokenKind.Identifier:
            case TokenKind.StringLiteral:
            case TokenKind.BooleanLiteral:
            case TokenKind.NullLiteral:
            case TokenKind.NumericLiteral:
            case TokenKind.Keyword:
                return true;
            case TokenKind.Punctuator:
                return token.value === '[';
            default:
                break;
        }
        return false;
    }

    parseGetterMethod(): FunctionExpression {
        const node = this.createMarker();

        const isGenerator = false;
        const previousAllowYield = this.context.allowYield;
        this.context.allowYield = false;
        const formalParameters = this.parseFormalParameters();
        if (formalParameters.params.length > 0) {
            this.tolerateError(Messages.BadGetterArity);
        }
        const method = this.parsePropertyMethod(formalParameters);
        this.context.allowYield = previousAllowYield;

        return this.finalize(node, new FunctionExpression(null, formalParameters.params, method, isGenerator, false));
    }

    parseSetterMethod(): FunctionExpression {
        const node = this.createMarker();

        const isGenerator = false;
        const previousAllowYield = this.context.allowYield;
        this.context.allowYield = false;
        const formalParameters = this.parseFormalParameters();
        if (formalParameters.params.length !== 1) {
            this.tolerateError(Messages.BadSetterArity);
        }
        else if (formalParameters.params[0] instanceof RestElement) {
            this.tolerateError(Messages.BadSetterRestParameter);
        }
        const method = this.parsePropertyMethod(formalParameters);
        this.context.allowYield = previousAllowYield;

        return this.finalize(node, new FunctionExpression(null, formalParameters.params, method, isGenerator, false));
    }

    parseGeneratorMethod(): FunctionExpression {
        const node = this.createMarker();

        const isGenerator = true;
        const previousAllowYield = this.context.allowYield;

        this.context.allowYield = true;
        const params = this.parseFormalParameters();
        this.context.allowYield = false;
        const method = this.parsePropertyMethod(params);
        this.context.allowYield = previousAllowYield;

        return this.finalize(node, new FunctionExpression(null, params.params, method, isGenerator, false));
    }

    // https://tc39.github.io/ecma262/#sec-generator-function-definitions

    isStartOfExpression(): boolean {
        let start = true;

        const value = this.lookahead.value;
        switch (this.lookahead.type) {
            case TokenKind.Punctuator:
                start = (value === '[') || (value === '(') || (value === '{') ||
                    (value === '+') || (value === '-') ||
                    (value === '!') || (value === '~') ||
                    (value === '++') || (value === '--') ||
                    (value === '/') || (value === '/=');  // regular expression literal
                break;

            case TokenKind.Keyword:
                start = (value === 'class') || (value === 'delete') ||
                    (value === 'function') || (value === 'let') || (value === 'new') ||
                    (value === 'super') || (value === 'this') || (value === 'typeof') ||
                    (value === 'void') || (value === 'yield');
                break;

            default:
                break;
        }

        return start;
    }

    parseYieldExpression(): YieldExpression {
        const node = this.createMarker();
        this.expectKeyword('yield');

        let argument: Expression | null = null;
        let delegate = false;
        if (!this.hasLineTerminator) {
            const previousAllowYield = this.context.allowYield;
            this.context.allowYield = false;
            delegate = this.match('*');
            if (delegate) {
                this.nextToken();
                argument = this.parseAssignmentExpression();
            }
            else if (this.isStartOfExpression()) {
                argument = this.parseAssignmentExpression();
            }
            this.context.allowYield = previousAllowYield;
        }

        return this.finalize(node, new YieldExpression(argument, delegate));
    }

    // https://tc39.github.io/ecma262/#sec-class-definitions

    parseClassElement(hasConstructor: { value: boolean }): Property | MethodDefinition {
        let token = this.lookahead;
        const node = this.createMarker();

        let kind: string = '';
        let key: Identifier | Literal | Expression | null = null;
        let value: FunctionExpression | null = null;
        let computed = false;
        let method = false;
        let isStatic = false;
        let isAsync = false;

        if (this.match('*')) {
            this.nextToken();
        }
        else {
            computed = this.match('[');
            key = this.parseObjectPropertyKey();
            const id = key as Identifier;
            if (id.name === 'static' && (this.qualifiedPropertyName(this.lookahead) || this.match('*'))) {
                token = this.lookahead;
                isStatic = true;
                computed = this.match('[');
                if (this.match('*')) {
                    this.nextToken();
                }
                else {
                    key = this.parseObjectPropertyKey();
                }
            }
            if ((token.type === TokenKind.Identifier) && !this.hasLineTerminator && (token.value === 'async')) {
                const punctuator = this.lookahead.value;
                if (punctuator !== ':' && punctuator !== '(' && punctuator !== '*') {
                    isAsync = true;
                    token = this.lookahead;
                    key = this.parseObjectPropertyKey();
                    if (token.type === TokenKind.Identifier) {
                        if (token.value === 'get' || token.value === 'set') {
                            this.tolerateUnexpectedToken(token);
                        }
                        else if (token.value === 'constructor') {
                            this.tolerateUnexpectedToken(token, Messages.ConstructorIsAsync);
                        }
                    }
                }
            }
        }

        const lookaheadPropertyKey = this.qualifiedPropertyName(this.lookahead);
        if (token.type === TokenKind.Identifier) {
            if (token.value === 'get' && lookaheadPropertyKey) {
                kind = 'get';
                computed = this.match('[');
                key = this.parseObjectPropertyKey();
                this.context.allowYield = false;
                value = this.parseGetterMethod();
            }
            else if (token.value === 'set' && lookaheadPropertyKey) {
                kind = 'set';
                computed = this.match('[');
                key = this.parseObjectPropertyKey();
                value = this.parseSetterMethod();
            }
        }
        else if (token.type === TokenKind.Punctuator && token.value === '*' && lookaheadPropertyKey) {
            kind = 'init';
            computed = this.match('[');
            key = this.parseObjectPropertyKey();
            value = this.parseGeneratorMethod();
            method = true;
        }

        if (!kind && key && this.match('(')) {
            kind = 'init';
            value = isAsync ? this.parsePropertyMethodAsyncFunction() : this.parsePropertyMethodFunction();
            method = true;
        }

        if (!kind) {
            this.throwUnexpectedToken(this.lookahead);
        }

        if (kind === 'init') {
            kind = 'method';
        }

        if (!computed) {
            if (isStatic && this.isPropertyKey(key, 'prototype')) {
                this.throwUnexpectedToken(token, Messages.StaticPrototype);
            }
            if (!isStatic && this.isPropertyKey(key, 'constructor')) {
                if (kind !== 'method' || !method || (value && value.generator)) {
                    this.throwUnexpectedToken(token, Messages.ConstructorSpecialMethod);
                }
                if (hasConstructor.value) {
                    this.throwUnexpectedToken(token, Messages.DuplicateConstructor);
                }
                else {
                    hasConstructor.value = true;
                }
                kind = 'constructor';
            }
        }

        return this.finalize(node, new MethodDefinition(key, computed, value, kind, isStatic));
    }

    parseClassElementList(): (Property | MethodDefinition)[] {
        const body: (Property | MethodDefinition)[] = [];
        const hasConstructor = { value: false };

        this.expect('{');
        while (!this.match('}')) {
            if (this.match(';')) {
                this.nextToken();
            }
            else {
                body.push(this.parseClassElement(hasConstructor));
            }
        }
        this.expect('}');

        return body;
    }

    parseClassBody(): ClassBody {
        const node = this.createMarker();
        const elementList = this.parseClassElementList();

        return this.finalize(node, new ClassBody(elementList));
    }

    parseClassDeclaration(identifierIsOptional?: boolean): ClassDeclaration {
        const previousIsAssignmentTarget = this.context.isAssignmentTarget;
        const previousIsBindingElement = this.context.isBindingElement;
        this.context.isAssignmentTarget = true;
        this.context.isBindingElement = true;
        try {
            const node = this.createMarker();

            const previousStrict = this.context.strict;
            this.context.strict = true;
            this.expectKeyword('class');

            const id = (identifierIsOptional && (this.lookahead.type !== TokenKind.Identifier)) ? null : this.parseVariableIdentifier();
            let superClass: Identifier | null = null;
            if (this.matchKeyword('extends')) {
                this.nextToken();
                superClass = assert_identifier(this.isolateCoverGrammar(this.parseLeftHandSideExpressionAllowCall));
            }
            const classBody = this.parseClassBody();
            this.context.strict = previousStrict;

            return this.finalize(node, new ClassDeclaration(id, superClass, classBody));
        }
        finally {
            this.context.isAssignmentTarget = previousIsAssignmentTarget;
            this.context.isBindingElement = previousIsBindingElement;
        }
    }

    parseClassExpression(): ClassExpression {
        const node = this.createMarker();

        const previousStrict = this.context.strict;
        this.context.strict = true;
        this.expectKeyword('class');
        const id = (this.lookahead.type === TokenKind.Identifier) ? this.parseVariableIdentifier() : null;
        let superClass: Identifier | null = null;
        if (this.matchKeyword('extends')) {
            this.nextToken();
            superClass = assert_identifier(this.isolateCoverGrammar(this.parseLeftHandSideExpressionAllowCall));
        }
        const classBody = this.parseClassBody();
        this.context.strict = previousStrict;

        return this.finalize(node, new ClassExpression(id, superClass, classBody));
    }

    // https://tc39.github.io/ecma262/#sec-scripts
    // https://tc39.github.io/ecma262/#sec-modules

    parseModule(): Program {
        this.context.strict = true;
        this.context.isModule = true;
        const node = this.createMarker();
        const body = this.parseDirectivePrologues();
        while (this.lookahead.type !== TokenKind.EOF) {
            body.push(this.parseStatementListItem());
        }
        return this.finalize(node, new Program(body, 'module'));
    }

    parseScript(): Program {
        const node = this.createMarker();
        const body = this.parseDirectivePrologues();
        while (this.lookahead.type !== TokenKind.EOF) {
            body.push(this.parseStatementListItem());
        }
        return this.finalize(node, new Program(body, 'script'));
    }

    // https://tc39.github.io/ecma262/#sec-imports

    parseModuleSpecifier(): Literal {
        const node = this.createMarker();

        if (this.lookahead.type !== TokenKind.StringLiteral) {
            this.throwError(Messages.InvalidModuleSpecifier);
        }

        const token = this.nextToken();
        const raw = this.getTokenRaw(token);
        return this.finalize(node, new Literal(token.value as string, raw));
    }

    // import {<foo as bar>} ...;
    parseImportSpecifier(): ImportSpecifier {
        const node = this.createMarker();

        let imported: Identifier;
        let local: Identifier;
        if (this.lookahead.type === TokenKind.Identifier) {
            imported = this.parseVariableIdentifier();
            local = imported;
            if (this.matchContextualKeyword('as')) {
                this.nextToken();
                local = this.parseVariableIdentifier();
            }
        }
        else {
            imported = this.parseIdentifierName();
            local = imported;
            if (this.matchContextualKeyword('as')) {
                this.nextToken();
                local = this.parseVariableIdentifier();
            }
            else {
                this.throwUnexpectedToken(this.nextToken());
            }
        }

        return this.finalize(node, new ImportSpecifier(local, imported));
    }

    // {foo, bar as bas}
    parseNamedImports(): ImportSpecifier[] {
        this.expect('{');
        const specifiers: ImportSpecifier[] = [];
        while (!this.match('}')) {
            specifiers.push(this.parseImportSpecifier());
            if (!this.match('}')) {
                this.expect(',');
            }
        }
        this.expect('}');

        return specifiers;
    }

    // import <foo> ...;
    parseImportDefaultSpecifier(): ImportDefaultSpecifier {
        const node = this.createMarker();
        const local = this.parseIdentifierName();
        return this.finalize(node, new ImportDefaultSpecifier(local));
    }

    // import <* as foo> ...;
    parseImportNamespaceSpecifier(): ImportNamespaceSpecifier {
        const node = this.createMarker();

        this.expect('*');
        if (!this.matchContextualKeyword('as')) {
            this.throwError(Messages.NoAsAfterImportNamespace);
        }
        this.nextToken();
        const local = this.parseIdentifierName();

        return this.finalize(node, new ImportNamespaceSpecifier(local));
    }

    parseImportDeclaration(): ImportDeclaration {
        if (this.context.inFunctionBody) {
            this.throwError(Messages.IllegalImportDeclaration);
        }

        const node = this.createMarker();
        this.expectKeyword('import');

        let src: Literal;
        let specifiers: ImportDeclarationSpecifier[] = [];
        if (this.lookahead.type === TokenKind.StringLiteral) {
            // import 'foo';
            src = this.parseModuleSpecifier();
        }
        else {
            if (this.match('{')) {
                // import {bar}
                specifiers = specifiers.concat(this.parseNamedImports());
            }
            else if (this.match('*')) {
                // import * as foo
                specifiers.push(this.parseImportNamespaceSpecifier());
            }
            else if (this.isIdentifierName(this.lookahead) && !this.matchKeyword('default')) {
                // import foo
                specifiers.push(this.parseImportDefaultSpecifier());
                if (this.match(',')) {
                    this.nextToken();
                    if (this.match('*')) {
                        // import foo, * as foo
                        specifiers.push(this.parseImportNamespaceSpecifier());
                    }
                    else if (this.match('{')) {
                        // import foo, {bar}
                        specifiers = specifiers.concat(this.parseNamedImports());
                    }
                    else {
                        this.throwUnexpectedToken(this.lookahead);
                    }
                }
            }
            else {
                this.throwUnexpectedToken(this.nextToken());
            }

            if (!this.matchContextualKeyword('from')) {
                const message = this.lookahead.value ? Messages.UnexpectedToken : Messages.MissingFromClause;
                this.throwError(message, this.lookahead.value);
            }
            this.nextToken();
            src = this.parseModuleSpecifier();
        }
        this.consumeSemicolon();

        return this.finalize(node, new ImportDeclaration(specifiers, src));
    }

    // https://tc39.github.io/ecma262/#sec-exports

    parseExportSpecifier(): ExportSpecifier {
        const node = this.createMarker();

        const local = this.parseIdentifierName();
        let exported = local;
        if (this.matchContextualKeyword('as')) {
            this.nextToken();
            exported = this.parseIdentifierName();
        }

        return this.finalize(node, new ExportSpecifier(local, exported));
    }

    parseExportDeclaration(): ExportDeclaration {
        if (this.context.inFunctionBody) {
            this.throwError(Messages.IllegalExportDeclaration);
        }

        const node = this.createMarker();
        this.expectKeyword('export');

        let exportDeclaration;
        if (this.matchKeyword('default')) {
            // export default ...
            this.nextToken();
            if (this.matchKeyword('function')) {
                // export default function foo () {}
                // export default function () {}
                const declaration = this.parseFunctionDeclaration(true);
                exportDeclaration = this.finalize(node, new ExportDefaultDeclaration(declaration));
            }
            else if (this.matchKeyword('class')) {
                // export default class foo {}
                const declaration = this.parseClassDeclaration(true);
                exportDeclaration = this.finalize(node, new ExportDefaultDeclaration(declaration));
            }
            else if (this.matchContextualKeyword('async')) {
                // export default async function f () {}
                // export default async function () {}
                // export default async x => x
                const declaration = this.matchAsyncFunction() ? this.parseFunctionDeclaration(true) : this.parseAssignmentExpression();
                exportDeclaration = this.finalize(node, new ExportDefaultDeclaration(declaration));
            }
            else {
                if (this.matchContextualKeyword('from')) {
                    this.throwError(Messages.UnexpectedToken, this.lookahead.value);
                }
                // export default {};
                // export default [];
                // export default (1 + 2);
                const declaration = this.match('{') ? this.parseObjectInitializer() :
                    this.match('[') ? this.parseArrayInitializer() : this.parseAssignmentExpression();
                this.consumeSemicolon();
                exportDeclaration = this.finalize(node, new ExportDefaultDeclaration(declaration));
            }

        }
        else if (this.match('*')) {
            // export * from 'foo';
            this.nextToken();
            if (!this.matchContextualKeyword('from')) {
                const message = this.lookahead.value ? Messages.UnexpectedToken : Messages.MissingFromClause;
                this.throwError(message, this.lookahead.value);
            }
            this.nextToken();
            const src = this.parseModuleSpecifier();
            this.consumeSemicolon();
            exportDeclaration = this.finalize(node, new ExportAllDeclaration(src));

        }
        else if (this.lookahead.type === TokenKind.Keyword) {
            // export var f = 1;
            switch (this.lookahead.value) {
                case 'let':
                case 'const': {
                    const declaration = this.parseLexicalDeclaration({ inFor: false });
                    return this.finalize(node, new ExportNamedDeclaration(declaration, [], null));
                }
                case 'var': {
                    const declaration = this.parseVariableStatement();
                    return this.finalize(node, new ExportNamedDeclaration(declaration, [], null));
                }
                case 'class': {
                    const declaration = this.parseClassDeclaration();
                    return this.finalize(node, new ExportNamedDeclaration(declaration, [], null));
                }
                case 'function': {
                    const declaration = this.parseFunctionDeclaration();
                    return this.finalize(node, new ExportNamedDeclaration(declaration, [], null));
                }
                default:
                    this.throwUnexpectedToken(this.lookahead);
            }
        }
        else if (this.matchAsyncFunction()) {
            const declaration = this.parseFunctionDeclaration();
            exportDeclaration = this.finalize(node, new ExportNamedDeclaration(declaration, [], null));

        }
        else {
            const specifiers: ExportSpecifier[] = [];
            let source: Literal | null = null;
            let isExportFromIdentifier = false;

            this.expect('{');
            while (!this.match('}')) {
                isExportFromIdentifier = isExportFromIdentifier || this.matchKeyword('default');
                specifiers.push(this.parseExportSpecifier());
                if (!this.match('}')) {
                    this.expect(',');
                }
            }
            this.expect('}');

            if (this.matchContextualKeyword('from')) {
                // export {default} from 'foo';
                // export {foo} from 'foo';
                this.nextToken();
                source = this.parseModuleSpecifier();
                this.consumeSemicolon();
            }
            else if (isExportFromIdentifier) {
                // export {default}; // missing fromClause
                const message = this.lookahead.value ? Messages.UnexpectedToken : Messages.MissingFromClause;
                this.throwError(message, this.lookahead.value);
            }
            else {
                // export {foo};
                this.consumeSemicolon();
            }
            exportDeclaration = this.finalize(node, new ExportNamedDeclaration(null, specifiers, source));
        }

        return exportDeclaration;
    }
}
