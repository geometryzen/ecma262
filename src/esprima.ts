import { CommentHandler } from './comment-handler';
import { Node } from './javascript';
import { JSXParser } from './jsx-parser';
import { Program } from './nodes';
import { MetaData, Parser } from './parser';
import { TokenEntry } from './token';
import { Tokenizer, TokenizerConfig } from './tokenizer';

export interface ParseOptions {
    comment?: boolean;
    attachComment?: boolean;
    sourceType?: 'module' | 'script';
    jsx?: boolean;
    range?: boolean;
    loc?: boolean;
    tokens?: boolean;
    tolerant?: boolean;
    source?: string;
}

/**
 * 
 */
export type ParseDelegate = (node: Node, metadata: MetaData) => void;

/**
 * 
 * @param sourceText 
 * @param options 
 * @param delegate 
 * @returns 
 */
export function parse(sourceText: string, options?: ParseOptions, delegate?: ParseDelegate): Program {
    let commentHandler: CommentHandler | null = null;

    const proxyDelegate: ParseDelegate = (node: Node, metadata: MetaData) => {
        delegate ? delegate(node, metadata) : node;
        if (commentHandler) {
            commentHandler.visit(node, metadata);
        }
    };

    let parserDelegate = (typeof delegate === 'function') ? proxyDelegate : void 0;
    let collectComment = false;
    if (options) {
        collectComment = (typeof options.comment === 'boolean' && options.comment);
        const attachComment = (typeof options.attachComment === 'boolean' && options.attachComment);
        if (collectComment || attachComment) {
            commentHandler = new CommentHandler();
            commentHandler.attach = attachComment;
            options.comment = true;
            parserDelegate = proxyDelegate;
        }
    }

    let isModule = false;
    if (options && typeof options.sourceType === 'string') {
        isModule = (options.sourceType === 'module');
    }

    let parser: Parser;
    if (options && typeof options.jsx === 'boolean' && options.jsx) {
        parser = new JSXParser(sourceText, options, parserDelegate);
    }
    else {
        parser = new Parser(sourceText, options, parserDelegate);
    }

    const program = isModule ? parser.parseModule() : parser.parseScript();
    // const ast = program as any;

    if (collectComment && commentHandler) {
        program.comments = commentHandler.comments;
    }
    if (parser.config.tokens) {
        program.tokens = parser.tokens;
    }
    if (parser.config.tolerant) {
        program.errors = parser.errorHandler.errors;
    }

    return program;
}

export function parseModule(code: string, options: ParseOptions = {}, delegate?: ParseDelegate): Program {
    options.sourceType = 'module';
    return parse(code, options, delegate);
}

export function parseScript(code: string, options: ParseOptions = {}, delegate?: ParseDelegate): Program {
    options.sourceType = 'script';
    return parse(code, options, delegate);
}

export function tokenize(sourceText: string, options: TokenizerConfig, hook?: (token: TokenEntry) => TokenEntry): { tokens: TokenEntry[], errors: Error[] } {
    const tokenizer = new Tokenizer(sourceText, options);

    const tokens: TokenEntry[] = [];

    try {
        // eslint-disable-next-line no-constant-condition
        while (true) {
            let token = tokenizer.getNextToken();
            if (!token) {
                break;
            }
            if (hook) {
                token = hook(token);
            }
            tokens.push(token);
        }
    }
    catch (e) {
        tokenizer.errorHandler.tolerate(e);
    }

    if (tokenizer.errorHandler.tolerant) {
        return { tokens, errors: tokenizer.errors() };
    }
    else {
        return { tokens, errors: [] };
    }
}

// Sync with *.json manifests.
export const version = '0.9.9';
