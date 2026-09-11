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

type FunctionType = {
    read Kind: "Function",
    read Parameters: { read Type },
    read Returns: Type?,
}

type UnionType = {
    read Kind: "Union",
    read Components: { read Type },
}

type IntersectionType = {
    read Kind: "Intersection",
    read Components: { read Type },
}

type FreeType = {
    read Kind: "Free",
    Name: string?,
    Bound: Type,
}

type Type =
    | ErrorType
    | NeverType
    | UnknownType
    | PrimitiveType
    | UnionType
    | IntersectionType
    | FreeType

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

local function primitiveType(primitive: index<PrimitiveType, "Primitive">): PrimitiveType
    return {
        Kind = "Primitive",
        -- TODO luau cast
        Primitive = primitive :: "nil",
    }
end

local function freeType(): Type
    return {
        Kind = "Free",
        Bound = unknownType(),
    }
end

local function errorReport(message, position): Type
    print(`{ position.Line }:{ position.Character }: { message }`)
    return errorType()
end

local function makeIntersection(a: Type, b: Type): Type
    return {
        Kind = "Intersection",
        Components = { a, b },
    }
end

local function toNever(n: NormalizedType)
    n.Error = false
    n.Top = false
    n.Nil = false
    n.Boolean = false
    n.Number = false
    n.String = false
end

local function toUnknown(n: NormalizedType)
    n.Error = false
    n.Top = true
    n.Nil = true
    n.Boolean = true
    n.Number = true
    n.String = true
end

local function isNever(n: NormalizedType)
    return not n.Top and not n.Nil and not n.Boolean
        and not n.Number and not n.String
end

local function isUnknown(n: NormalizedType)
    return n.Top
end

local function normalizationToString(n: NormalizedType): string
    local t = {}

    if n.Error then
        table.insert(t, "*error*")
    end

    if n.Top then
        table.insert(t, "unknown")
    end

    if n.Nil then
        table.insert(t, "nil")
    end

    if n.Boolean then
        table.insert(t, "boolean")
    end

    if n.Number then
        table.insert(t, "number")
    end

    if n.String then
        table.insert(t, "string")
    end

    if #t == 0 then
        return "never"
    end

    return table.concat(t, " | ")
end

local function unionNormalizedType(n: NormalizedType, with: NormalizedType)
    n.Error = n.Error or with.Error
    n.Top = n.Top or with.Top
    n.Nil = n.Nil or with.Nil
    n.Boolean = n.Boolean or with.Boolean
    n.Number = n.Number or with.Number
    n.String = n.String or with.String
end

local function intersectNormalizedType(n: NormalizedType, with: NormalizedType)
    n.Error = n.Error or with.Error
    n.Top = n.Top and with.Top
    n.Nil = n.Nil and with.Nil
    n.Boolean = n.Boolean and with.Boolean
    n.Number = n.Number and with.Number
    n.String = n.String and with.String
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
        toNever(n)
    elseif ty.Kind == "unknown" then
        toUnknown(n)
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
        -- Unions grow from `never`.
        toNever(n)

        for i, c in ty.Components do
            local inner = normalize(c)
            unionNormalizedType(n, inner)
        end
    elseif ty.Kind == "Intersection" then
        -- Intersections are carved out from `unknown`.
        toUnknown(n)

        for i, c in ty.Components do
            local inner = normalize(c)
            intersectNormalizedType(n, inner)
        end
    elseif ty.Kind == "Free" then
        toUnknown(n)
        n.Error = true
        print(`Cannot normalize free types.`)
    else
        unreachable(ty.Kind)
        error(`Unreachable.`)
    end

    return n
end

local function isSubtypeOf(a: NormalizedType, b: NormalizedType): boolean
    if a.Error or b.Error then
        return true
    end

    if b.Top then
        return true
    end

    if a.Nil and not b.Nil then
        return false
    end

    if a.Boolean and not b.Boolean then
        return false
    end

    if a.Number and not b.Number then
        return false
    end

    if a.String and not b.String then
        return false
    end

    return true
end

local freeTypeNameCount = 0

local function typeToString(ty: Type): string
    local function freeTypeName(tyvar: FreeType)
        if tyvar.Name then
            return tyvar.Name
        else
            local name = `'{ string.char((freeTypeNameCount + 19) % 26 + 97) }`

            if freeTypeNameCount > 25 then
                name ..= freeTypeNameCount // 25 + 1
            end

            tyvar.Name = name
            freeTypeNameCount += 1

            return name
        end
    end

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
    elseif ty.Kind == "Free" then
        local name = freeTypeName(ty)
        local str = ""

        if not isNever(normalize(ty.LowerBound)) then
            str = `{ typeToString(ty.LowerBound) } <: `
        end

        str ..= name

        if not isUnknown(normalize(ty.UpperBound)) then
            str = ` <: { typeToString(ty.UpperBound) }`
        end

        return str
    else
        unreachable(ty.Kind)
        error(`Unreachable.`)
    end
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
local typeAssociations: { [string]: { Expression: caw.Expression, Type: Type }? } = {}

local typeAnnotationToType
local check

local function infer(expr: caw.Expression): Type
    if expr.Type == "Constant" then
        return primitiveType(expr.Constant.Type)

    elseif expr.Type == "Declaration" then
        local ty = if expr.Declaration.Value then infer(expr.Declaration.Value) else primitiveType("nil")
        local anno = if expr.Declaration.Annotation then typeAnnotationToType(expr.Position, expr.Declaration.Annotation) else nil

        -- TODO luau type error on `ty` when `anno or` is removed
        typeAssociations[expr.Declaration.Name] = {
            Expression = expr,
            Type = anno or ty
        }

        if anno then
            check(ty, anno, if expr.Declaration.Value then expr.Declaration.Value.Position else expr.Position)
        end

        return ty

    elseif expr.Type == "Reference" then
        local name = expr.Reference
        local referred = typeAssociations[name]

        if referred then
            return referred.Type
        else
            return errorReport(`Invalid reference '{ name }'.`, expr.Position)
        end

    elseif expr.Type == "BinaryOperation" then
        local variable = expr.Left
        local value = expr.Right

        if expr.Operator == "=" then
            assert(variable.Type == "Reference")
            local ref = typeAssociations[variable.Reference]

            if not ref then
                return errorReport(`Undefined variable '{ variable.Reference }'`, variable)
            end
            assert(ref.Expression.Type == "Declaration")

            local variableTy = infer(variable)
            local valueTy = infer(value)
            local annoTy = if ref.Expression.Declaration.Annotation then typeAnnotationToType(ref.Expression.Position, ref.Expression.Declaration.Annotation) else nil

            if annoTy then
                check(valueTy, annoTy, value.Position)
            elseif ref.Expression.Declaration.Value then
                check(valueTy, variableTy, value.Position)
            else
                typeAssociations[variable.Reference] = {
                    Expression = ref.Expression,
                    Type = valueTy,
                }
            end

            return annoTy or valueTy
        else
            error(`Cannot infer '{ expr.Operator }' operations.`)
        end
    elseif expr.Type == "Function" then
        for _, param in expr.Function.Parameters do
            typeAssociations[param.Name] = {
                Expression = expr,
                Type = if param.Annotation then typeAnnotationToType(expr.Position, param.Annotation) else freeType(),
            }
        end



        for _, param in expr.Function.Parameters do
            typeAssociations[param.Name] = nil
        end
    else
        error(`Cannot infer { expr.Type }.`)
    end
end

function typeAnnotationToType(position: caw.Position, annotation: caw.TypeAnnotation): Type
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

local function reportMismatch(a: Type, na: NormalizedType, b: Type, nb: NormalizedType, position: caw.Position)
    local err = `{ position.Line }:{ position.Character }: Type error: `

    local as = typeToString(a)
    local nas = normalizationToString(na)

    err ..= `'{ as }' `

    if nas ~= as then
        err ..= `(aka '{ nas }') `
    end

    err ..= "is not a subtype of "

    local bs = typeToString(b)
    local nbs = normalizationToString(nb)

    err ..= `'{ bs }'`

    if nbs ~= bs then
        err ..= ` (aka '{ nbs }')`
    end

    err ..= "."

    print(err)
end

function check(inferred: Type, annotation: Type, position: caw.Position)
    if annotation.Kind == "Error" then
        return
    end

    local normalizedInference = normalize(inferred)
    local normalizedAnnotation = normalize(annotation)

    if not isSubtypeOf(normalizedInference, normalizedAnnotation) then
        reportMismatch(inferred, normalizedInference, annotation, normalizedAnnotation, position)
    end
end

local function bidisemi(expressions: { read caw.Expression }, options: { read LogInference: boolean })
    -- logInference = options.LogInference

    for _, expr in expressions do
        infer(expr)
    end

    for k, t in typeAssociations do
        print(`{ k }: { typeToString(t.Type) }`)
    end
end

return bidisemi
