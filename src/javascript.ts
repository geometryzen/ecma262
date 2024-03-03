import { SourceLocation } from "./scanner";

export enum NodeFlags {
    None = 0,
    Let = 1,
    Const = 2,
}

export interface ReadonlyTextRange {
    readonly pos: number;
    readonly end: number;
}

export interface Comment {
    type: 'Line' | 'Block';
    value: string;
    range?: [number, number];
    loc?: SourceLocation;
}

/**
 * Implemented by all ECMAScript nodes.
 */
export interface Node {
    readonly type: string;
    innerComments?: Comment[];
    leadingComments?: Comment[];
    trailingComments?: Comment[];
    range?: [number, number];
    loc?: SourceLocation;
}
