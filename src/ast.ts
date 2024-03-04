/*
export function isDefined(node: Node | null | undefined): node is Node {
    if (node === null) {
        return false;
    }
    else if (typeof Node === 'undefined') {
        return false;
    }
    else {
        return true;
    }
}
*/
/*
export function isUndefined(node: Node | null | undefined): boolean {
    return !isDefined(node);
}
*/

/*
export function isExpression(node: Node | null | undefined): boolean {
    if (isDefined(node)) {
        switch (node.type) {
            case Syntax.ArrayExpression:
            case Syntax.AssignmentExpression:
            case Syntax.BinaryExpression:
            case Syntax.CallExpression:
            case Syntax.ConditionalExpression:
            case Syntax.FunctionExpression:
            case Syntax.Identifier:
            case Syntax.Literal:
            case Syntax.LogicalExpression:
            case Syntax.MemberExpression:
            case Syntax.NewExpression:
            case Syntax.ObjectExpression:
            case Syntax.SequenceExpression:
            case Syntax.ThisExpression:
            case Syntax.UnaryExpression:
            case Syntax.UpdateExpression: {
                return true;
            }
        }
        return false;
    }
    else {
        return false;
    }
}
*/

/*
export function isIterationStatement(node: Node | null | undefined) {
    if (isDefined(node)) {
        switch (node.type) {
            case Syntax.DoWhileStatement:
            case Syntax.ForInStatement:
            case Syntax.ForStatement:
            case Syntax.WhileStatement: {
                return true;
            }
        }
        return false;
    }
    else {
        return false;
    }
}
*/

/*
export function isStatement(node: Node | null | undefined): boolean {
    if (isDefined(node)) {
        switch (node.type) {
            case Syntax.BlockStatement:
            case Syntax.BreakStatement:
            case Syntax.ContinueStatement:
            case Syntax.DebuggerStatement:
            case Syntax.DoWhileStatement:
            case Syntax.EmptyStatement:
            case Syntax.ExpressionStatement:
            case Syntax.ForInStatement:
            case Syntax.ForStatement:
            case Syntax.FunctionDeclaration:
            case Syntax.IfStatement:
            case Syntax.LabeledStatement:
            case Syntax.ReturnStatement:
            case Syntax.SwitchStatement:
            case Syntax.ThrowStatement:
            case Syntax.TryStatement:
            case Syntax.VariableDeclaration:
            case Syntax.WhileStatement:
            case Syntax.WithStatement: {
                return true;
            }
        }
        return false;
    }
    else {
        return false;
    }
}
*/

/*
export function isSourceElement(node: Node | null | undefined) {
    return isStatement(node) || isDefined(node) && node.type === Syntax.FunctionDeclaration;
}
*/
/*
export function trailingStatement(node: Node): Statement | null {
    switch (node.type) {
        case Syntax.IfStatement: {
            if (is_if_statement(node)) {
                if (node.alternate != null) {
                    return node.alternate;
                }
                return node.consequent;
            }
            break;
        }
        case Syntax.LabeledStatement:
        case Syntax.ForStatement:
        case Syntax.ForInStatement:
        case Syntax.WhileStatement:
        case Syntax.WithStatement: {
            const specific = node as WhileStatement;
            return specific.body;
        }
    }
    return null;
}
*/
/*
export function isProblematicIfStatement(node: Node): boolean {
    let current: Statement | null;

    if (is_if_statement(node)) {
        if (isUndefined(node.alternate)) {
            return false;
        }
        current = node.consequent;
        do {
            if (is_if_statement(current)) {
                if (isUndefined(current.alternate)) {
                    return true;
                }
            }
            current = trailingStatement(current);
        } while (current);

        return false;
    }
    else {
        return false;
    }
}
*/
