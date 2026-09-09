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

type PrimitiveType = {
    read Kind: "Primitive",
    read Primitive: "nil" | "boolean" | "number" | "string",
}

type Type =
    | ErrorType
    | NeverType
    | UnknownType
    | PrimitiveType

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

-- Maps type names to types.
local typeEnv: { [string]: Type? } = {
    never = neverType(),
    unknown = unknownType(),
    boolean = primitiveType("boolean"),
    number = primitiveType("number"),
    string = primitiveType("string"),
}
-- Maps declaration names to types.
local typeAssociations: { [string]: Type? } = {}
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
    if a.Kind == "error" then
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

    return isSyntacticSubtypeOf(inferred, annotated), inferred, annotated
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
