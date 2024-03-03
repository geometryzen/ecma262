import { SourceLocation } from './scanner';

export const enum TokenKind {
    BooleanLiteral = 1,
    EOF,
    Identifier,
    Keyword,
    NullLiteral,
    NumericLiteral,
    Punctuator,
    StringLiteral,
    RegularExpression,
    Template,
    JSXIdentifier,
    JSXText,
    BlockComment,
    LineComment
}

export const TokenName: { [key: number]: string } = {};
TokenName[TokenKind.BooleanLiteral] = 'Boolean';
TokenName[TokenKind.EOF] = '<end>';
TokenName[TokenKind.Identifier] = 'Identifier';
TokenName[TokenKind.Keyword] = 'Keyword';
TokenName[TokenKind.NullLiteral] = 'Null';
TokenName[TokenKind.NumericLiteral] = 'Numeric';
TokenName[TokenKind.Punctuator] = 'Punctuator';
TokenName[TokenKind.StringLiteral] = 'String';
TokenName[TokenKind.RegularExpression] = 'RegularExpression';
TokenName[TokenKind.Template] = 'Template';
TokenName[TokenKind.JSXIdentifier] = 'JSXIdentifier';
TokenName[TokenKind.JSXText] = 'JSXText';
TokenName[TokenKind.BlockComment] = 'BlockComment';
TokenName[TokenKind.LineComment] = 'LineComment';

// type keywords = 'async' | 'await' | 'class' | 'constructor' | 'delete' | 'for' | 'function' | 'get' | 'if' | 'in' | 'let' | 'new' | 'set' | 'super' | 'target' | 'this' | 'typeof' | 'void' | 'while' | 'with' | 'yield';
// type more = '{' | '(' | '[' | '*' | ':' | ',' | ';' | '=' | '.' | '...' | '*=' | '**=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '>>>=' | '&=' | '^=' | '|=';
// type arithmetic = '+' | '-' | '!' | '~' | '++' | '--' | '/';

/**
 * A string may be a bit imprecise, but we also have to allow for identifiers.
 * A possibility may be to split the value into two fields?
 */
export type ReaderEntry = string | number | null;

export function is_raw_token_value(x: unknown): x is ReaderEntry {
    if (typeof x === 'string') {
        return true;
    }
    else if (typeof x === 'number') {
        return true;
    }
    else {
        return x === null;
    }
}

export function assert_raw_token_value(x: unknown): ReaderEntry {
    if (is_raw_token_value(x)) {
        return x;
    }
    else {
        throw new Error();
    }
}

export function as_string(value: ReaderEntry): string {
    if (typeof value === 'string') {
        return value;
    }
    else {
        throw new Error();
    }
}

/**
 * Raw tokens are produced by the scanner.ts
 */
export interface RawToken {
    type: TokenKind;
    value: ReaderEntry;
    pattern?: string;
    flags?: string;
    regex?: RegExp | null;
    octal?: boolean;
    cooked?: string;
    head?: boolean;
    tail?: boolean;
    lineNumber: number;
    lineStart: number;
    start: number;
    end: number;
}
/*
export const TokenName: { [key: number]: string } = {};
TokenName[TokenKind.BooleanLiteral] = 'Boolean';
TokenName[TokenKind.EOF] = '<end>';
TokenName[TokenKind.Identifier] = 'Identifier';
TokenName[TokenKind.Keyword] = 'Keyword';
TokenName[TokenKind.NullLiteral] = 'Null';
TokenName[TokenKind.NumericLiteral] = 'Numeric';
TokenName[TokenKind.Punctuator] = 'Punctuator';
TokenName[TokenKind.StringLiteral] = 'String';
TokenName[TokenKind.RegularExpression] = 'RegularExpression';
TokenName[TokenKind.Template] = 'Template';
TokenName[TokenKind.JSXIdentifier] = 'JSXIdentifier';
TokenName[TokenKind.JSXText] = 'JSXText';
*/

/**
 * RawToken is converted into TokenEntry by the parser.ts convertToken() function.
 * Hence, this could be called a ParserToken.
 * It was also called BufferEntry in esprima.
 */
export interface TokenEntry {
    type: string;
    value: string;
    regex?: {
        pattern: string;
        flags: string;
    };
    range?: [number, number];
    loc?: SourceLocation;
}
