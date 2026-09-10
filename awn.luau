-- awn.luau, 09/09/2026
-- Bidirectional inference with semantic subtyping and normalization.

local caw = require("./caw")

type ErrorType = {
    read Kind: "error",
}

type NeverType = {
    read Kind: "never",
}

type UnknownType = {
    read Kind: "unknown",
}

type NilType = {
    read Kind: "Primitive",
    read Primitive: "nil",
}

type BooleanType = {
    read Kind: "Primitive",
    read Primitive: "boolean",
}

type NumberType = {
    read Kind: "Primitive",
    read Primitive: "number",
}

type StringType = {
    read Kind: "Primitive",
    read Primitive: "string",
}

type PrimitiveType =
    | NilType
    | BooleanType
    | NumberType
    | StringType

type UnionType = {
    read Kind: "Union",
    read Components: { read Type },
}

type IntersectionType = {
    read Kind: "Intersection",
    read Components: { read Type },
}

type Type =
    | ErrorType
    | NeverType
    | UnknownType
    | PrimitiveType
    | UnionType
    | IntersectionType

type NormalizedType = {
    Error:   boolean,
    Top:     boolean,
    Nil:     boolean,
    Boolean: boolean,
    Number:  boolean,
    String:  boolean,
}

local function unreachable(x: never): never
    error(`Unexpected branch with value '{ x }' ({ typeof(x) }), this is an internal error.`)
end

local function typeToString(ty: Type): string
    if ty.Kind == "error" then
        return "*error*"
    elseif ty.Kind == "never" then
        return "never"
    elseif ty.Kind == "unknown" then
        return "unknown"
    elseif ty.Kind == "Primitive" then
        return ty.Primitive
    elseif ty.Kind == "Union" then
        local tys = table.create<<string>>(#ty.Components)

        for i, c in ty.Components do
            local ts = typeToString(c)
            if c.Kind == "Union" or c.Kind == "Intersection" then
                tys[i] = `({ ts })`
            else
                tys[i] = ts
            end
        end

        return table.concat(tys, " | ")
    elseif ty.Kind == "Intersection" then
        local tys = table.create<<string>>(#ty.Components)

        for i, c in ty.Components do
            local ts = typeToString(c)
            if c.Kind == "Union" or c.Kind == "Intersection" then
                tys[i] = `({ ts })`
            else
                tys[i] = ts
            end
        end

        return table.concat(tys, " & ")
    else
        unreachable(ty.Kind)
        error(`Unreachable.`)
    end
end

local function errorType(): Type
    return {
        Kind = "error",
    }
end

local function neverType(): Type
    return {
        Kind = "never",
    }
end

local function unknownType(): Type
    return {
        Kind = "unknown",
    }
end

local function primitiveType(primitive: index<PrimitiveType, "Primitive">): Type
    return {
        Kind = "Primitive",
        Primitive = primitive,
    }
end

local function errorReport(message, position): Type
    print(`{ position.Line }:{ position.Character }: { message }`)
    return errorType()
end

local function unionNormalizedType(n: NormalizedType, with: NormalizedType)
    n.Error = n.Error or with.Error
    n.Top = n.Top or with.Top
    n.Nil = n.Nil or with.Nil
    n.Boolean = n.Boolean or with.Boolean
    n.Number = n.Number or with.Number
    n.String = n.String or with.String
end

local function normalize(ty: Type): NormalizedType
    local n: NormalizedType = {
        Top = false,
        Error = false,
        Nil = false,
        Boolean = false,
        Number = false,
        String = false,
    }

    if ty.Kind == "error" then
        n.Error = true
    elseif ty.Kind == "never" then
    elseif ty.Kind == "unknown" then
        n.Top = true
    elseif ty.Kind == "Primitive" then
        if ty.Primitive == "nil" then
            n.Nil = true
        elseif ty.Primitive == "boolean" then
            n.Boolean = true
        elseif ty.Primitive == "number" then
            n.Number = true
        elseif ty.Primitive == "string" then
            n.String = true
        else
            unreachable(ty.Primitive)
            error(`Unreachable.`)
        end
    elseif ty.Kind == "Union" then
        for i, c in ty.Components do
            local inner = normalize(c)
            unionNormalizedType(n, inner)
        end
    elseif ty.Kind == "Intersection" then
        error(`Not yet.`)
    else
        unreachable(ty.Kind)
        error(`Unreachable.`)
    end

    return n
end

local function isSubtypeOf(a: NormalizedType, b: NormalizedType): boolean
    if a.Top and not b.Top then
        return false
    end

    if a.Error or b.Error then
        return true
    end

    if a.Nil and b.Nil then
        return true
    end

    if a.Boolean and b.Boolean then
        return true
    end

    if a.Number and b.Number then
        return true
    end

    if a.String and b.String then
        return true
    end

    return false
end

-- Maps type names to types.
local typeEnv: { [string]: Type? } = {
    never = neverType(),
    unknown = unknownType(),
    ["nil"] = primitiveType("nil"),
    boolean = primitiveType("boolean"),
    number = primitiveType("number"),
    string = primitiveType("string"),
}
-- Maps declaration names to types.
local typeAssociations: { [string]: Type? } = {
    nsn = {
        Kind = "Intersection",
        Components = {
            {
                Kind = "Union",
                Components = {
                    primitiveType("string"),
                    primitiveType("nil"),
                }
            },
            {
                Kind = "Union",
                Components = {
                    primitiveType("number"),
                    primitiveType("nil"),
                }
            }
        }
    }
}
local inferenceCache: { [caw.Expression]: Type? } = {}
local annotationToType: { [caw.TypeAnnotation]: Type? } = {}

local function infer(expr: caw.Expression): Type
    if expr.Type == "Constant" then
        return primitiveType(expr.Constant.Type)

    elseif expr.Type == "Declaration" then
        local ty = infer(expr.Declaration.Value)
        typeAssociations[expr.Declaration.Name] = ty

        return ty
    elseif expr.Type == "Reference" then
        local name = expr.Reference
        local referred = typeAssociations[name]

        if referred then
            return referred
        else
            return errorReport(`Invalid reference '{ name }'.`, expr.Position)
        end
    else
        error(`Cannot infer { expr.Type }.`)
    end
end

local function isSyntacticSubtypeOf(a: Type, b: Type): boolean
    if a == b then
        return true
    end

    if a.Kind == "error" or b.Kind == "error" then
        return true
    elseif a.Kind == "Union" then
        for i, c in a.Components do
            if not isSyntacticSubtypeOf(c, b) then
                return false
            end
        end

        return true
    elseif a.Kind == "Intersection" then
        for i, c in a.Components do
            if isSyntacticSubtypeOf(c, b) then
                return true
            end
        end

        return false
    elseif b.Kind == "Union" then
        for i, c in b.Components do
            if isSyntacticSubtypeOf(a, c) then
                return true
            end
        end

        return false
    elseif b.Kind == "Intersection" then
        for i, c in b.Components do
            if not isSyntacticSubtypeOf(a, c) then
                return false
            end
        end

        return true
    elseif a.Kind == "never" then
        return true
    elseif a.Kind == "unknown" then
        return b.Kind == "unknown"
    elseif a.Kind == "Primitive" then
        return b.Kind == "unknown" or (b.Kind == "Primitive" and b.Primitive == a.Primitive)
    else
        unreachable(a.Kind)
        error(`Unreachable.`)
    end
end

local function typeAnnotationToType(position: caw.Position, annotation: caw.TypeAnnotation): Type
    if annotation.Kind == "Error" then
        return errorType()
    elseif annotation.Kind == "Reference" then
        local ty = typeEnv[annotation.Reference]

        if ty then
            return ty
        else
            return errorReport(`Unknown type '{ annotation.Reference }'`, position)
        end
    elseif annotation.Kind == "Union" then
        local components = table.create<<Type>>(#annotation.Components)

        for i, c in annotation.Components do
            components[i] = typeAnnotationToType(position, c)
        end

        return {
            Kind = "Union",
            Components = components,
        }
    elseif annotation.Kind == "Intersection" then
        local components = table.create<<Type>>(#annotation.Components)

        for i, c in annotation.Components do
            components[i] = typeAnnotationToType(position, c)
        end

        return {
            Kind = "Intersection",
            Components = components,
        }
    else
        unreachable(annotation.Kind)
        error(`Unreachable.`)
    end
end

local function check(expr: caw.Expression, annotation: caw.TypeAnnotation): (boolean, Type, Type)
    if annotation.Kind == "Error" then
        return true, inferenceCache[expr] or infer(expr), errorType()
    end

    local inferred = inferenceCache[expr] or infer(expr)
    inferenceCache[expr] = inferred

    local annotated = annotationToType[annotation] or typeAnnotationToType(expr.Position, annotation)
    annotationToType[annotation] = annotated

    return isSubtypeOf(normalize(inferred), normalize(annotated)), inferred, annotated
end

local function bidisemi(expressions: { read caw.Expression }, options: { read LogInference: boolean })
    -- logInference = options.LogInference

    for _, expr in expressions do
        local ty = infer(expr)
        print(typeToString(ty))

        if expr.Type == "Declaration" and expr.Declaration.Annotation then
            local matches, a, b  = check(expr, expr.Declaration.Annotation)

            if not matches then
                print(`Type does not match annotation: { typeToString(a) } is not a subtype of { typeToString(b) }.`)
            end
        end
    end
end

return bidisemi
