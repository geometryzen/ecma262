import { BaseNode, Expression, Literal } from './nodes';
import { Syntax } from './syntax';

export type JSXAttributeName = JSXIdentifier | JSXNamespacedName;
export type JSXAttributeValue = Literal | JSXElement | JSXSpreadAttribute | JSXExpressionContainer;
export type JSXChild = JSXElement | JSXExpressionContainer | JSXText;
export type JSXElementAttribute = JSXAttribute | JSXSpreadAttribute;
export type JSXElementName = JSXIdentifier | JSXNamespacedName | JSXMemberExpression;

export class JSXClosingElement extends BaseNode {
    readonly name: JSXElementName;
    constructor(name: JSXElementName) {
        super(Syntax.JSXClosingElement);
        this.name = name;
    }
}

export class JSXElement extends BaseNode {
    readonly openingElement: JSXOpeningElement;
    readonly children: JSXChild[];
    readonly closingElement: JSXClosingElement | null;
    constructor(openingElement: JSXOpeningElement, children: JSXChild[], closingElement: JSXClosingElement | null) {
        super(Syntax.JSXElement);
        this.openingElement = openingElement;
        this.children = children;
        this.closingElement = closingElement;
    }
}

export class JSXEmptyExpression extends BaseNode {
    constructor() {
        super(Syntax.JSXEmptyExpression);
    }
}

export class JSXExpressionContainer extends BaseNode {
    readonly expression: Expression | JSXEmptyExpression;
    constructor(expression: Expression | JSXEmptyExpression) {
        super(Syntax.JSXExpressionContainer);
        this.expression = expression;
    }
}

export class JSXIdentifier extends BaseNode {
    readonly name: string;
    constructor(name: string) {
        super(Syntax.JSXIdentifier);
        this.name = name;
    }
}

export class JSXMemberExpression extends BaseNode {
    readonly object: JSXMemberExpression | JSXIdentifier;
    readonly property: JSXIdentifier;
    constructor(object: JSXMemberExpression | JSXIdentifier, property: JSXIdentifier) {
        super(Syntax.JSXMemberExpression);
        this.object = object;
        this.property = property;
    }
}

export class JSXAttribute extends BaseNode {
    readonly name: JSXAttributeName;
    readonly value: JSXAttributeValue | null;
    constructor(name: JSXAttributeName, value: JSXAttributeValue | null) {
        super(Syntax.JSXAttribute);
        this.name = name;
        this.value = value;
    }
}

export class JSXNamespacedName extends BaseNode {
    readonly namespace: JSXIdentifier;
    readonly name: JSXIdentifier;
    constructor(namespace: JSXIdentifier, name: JSXIdentifier) {
        super(Syntax.JSXNamespacedName);
        this.namespace = namespace;
        this.name = name;
    }
}

export class JSXOpeningElement extends BaseNode {
    readonly name: JSXElementName;
    readonly selfClosing: boolean;
    readonly attributes: JSXElementAttribute[];
    constructor(name: JSXElementName, selfClosing: boolean, attributes: JSXElementAttribute[]) {
        super(Syntax.JSXOpeningElement);
        this.name = name;
        this.selfClosing = selfClosing;
        this.attributes = attributes;
    }
}

export class JSXSpreadAttribute extends BaseNode {
    readonly argument: Expression;
    constructor(argument: Expression) {
        super(Syntax.JSXSpreadAttribute);
        this.argument = argument;
    }
}

export class JSXText extends BaseNode {
    readonly value: string;
    readonly raw: string;
    constructor(value: string, raw: string) {
        super(Syntax.JSXText);
        this.value = value;
        this.raw = raw;
    }
}
