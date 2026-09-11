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

type GenericType = {
    read Kind: "Generic",
    read Name: string,
}

type FunctionType = {
    read Kind: "Function",
    read Generics: { read GenericType },
    read Parameters: { read Type },
    read Variadic: Type?,
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
    | GenericType
    | FunctionType
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

local function genericType(name: string): GenericType
    return {
        Kind = "Generic",
        Name = name,
    }
end

local function functionType(generics: { read GenericType }, parameters: { read Type }, variadic: Type?, returns: Type?): Type
    return {
        Kind = "Function",
        Generics = generics,
        Parameters = parameters,
        Variadic = variadic,
        Returns = returns,
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
    elseif ty.Kind == "Generic" then
        toUnknown(n)
        n.Error = true
        print(`Cannot normalize generics.`)
    elseif ty.Kind == "Function" then
        toUnknown(n)
        n.Error = true
        print(`Cannot normalize functions.`)
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
    elseif ty.Kind == "Generic" then
        return ty.Name
    elseif ty.Kind == "Function" then
        local str = ""

        if #ty.Generics > 0 then
            local gens = table.create<<string>>(#ty.Generics)

            for i, gen in ty.Generics do
                gens[i] = gen.Name
            end

            str = `<{ table.concat(gens, ", ") }>`
        end

        local params = table.create<<string>>(#ty.Parameters)

        for i, param in ty.Parameters do
            params[i] = typeToString(param)
        end

        str ..= `({ table.concat(params, ", ") }) -> `
        str ..= if ty.Returns then typeToString(ty.Returns) else "()"

        return str

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
        local str = freeTypeName(ty)

        if not isUnknown(normalize(ty.Bound)) then
            str ..= ` <: { typeToString(ty.Bound) }`
        end

        return str
    else
        unreachable(ty.Kind)
        error(`Unreachable.`)
    end
end

local function normalizationToType(n: NormalizedType): Type
    local t = {}

    if n.Error then
        table.insert(t, errorType())
    end

    if isUnknown(n) then
        table.insert(t, unknownType())
    else
        if n.Nil then
            table.insert(t, primitiveType("nil"))
        end

        if n.Boolean then
            table.insert(t, primitiveType("boolean"))
        end

        if n.Number then
            table.insert(t, primitiveType("number"))
        end

        if n.String then
            table.insert(t, primitiveType("string"))
        end
    end

    if #t == 0 then
        return neverType()
    end

    return {
        Kind = "Union",
        Components = t,
    }
end

local function normalizationToString(n: NormalizedType): string
    return typeToString(normalizationToType(n))
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

type InferenceState = {
    ReturnType: Type? | true,
}

local function infer(state: InferenceState, expr: caw.Expression): Type
    if expr.Type == "Constant" then
        return primitiveType(expr.Constant.Type)

    elseif expr.Type == "Declaration" then
        local ty = if expr.Declaration.Value then infer(state, expr.Declaration.Value) else primitiveType("nil")
        local anno = if expr.Declaration.Annotation then typeAnnotationToType(expr.Position, expr.Declaration.Annotation) else nil

        if ty.Kind == "Free" and anno then
            ty.Bound = makeIntersection(ty.Bound, anno)
        end

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

            if ref.Expression.Type == "Function" then -- Function parameter.
                if ref.Type.Kind == "Free" then
                    local ty = makeIntersection(ref.Type.Bound, infer(state, value))
                    ref.Type.Bound = ty

                    return ty
                else
                    return ref.Type
                end
            end

            assert(ref.Expression.Type == "Declaration", ref.Expression.Type)

            local variableTy = infer(state, variable)
            local valueTy = infer(state, value)
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
        local gens = table.create<<GenericType>>(#expr.Function.Parameters)
        local params = table.create<<Type>>(#expr.Function.Parameters)

        for i, param in expr.Function.Parameters do
            local ty = if param.Annotation then typeAnnotationToType(expr.Position, param.Annotation) else freeType()
            params[i] = ty

            typeAssociations[param.Name] = {
                Expression = expr,
                Type = ty,
            }
        end

        local prevRetTy = state.ReturnType
        state.ReturnType = true

        for _, expr in expr.Function.Body do
            infer(state, expr)
        end

        local retTy = state.ReturnType
        state.ReturnType = prevRetTy

        for i, param in expr.Function.Parameters do
            typeAssociations[param.Name] = nil

            local paramTy = params[i]

            if paramTy.Kind == "Free" then
                if isUnknown(normalize(paramTy.Bound)) then
                    local gen = genericType(string.char((#gens + 19) % 26 + 65))
                    params[i] = gen

                    if retTy == paramTy then
                        retTy = gen
                    end

                    table.insert(gens, gen)
                else
                    local bound = normalizationToType(normalize(paramTy.Bound))
                    params[i] = bound

                    if retTy == paramTy then
                        retTy = bound
                    end
                end
            end
        end

        if retTy == true then
            retTy = nil
        end

        return functionType(gens, params, nil, retTy)
    elseif expr.Type == "Return" then
        local retTy = state.ReturnType

        if retTy == true then
            state.ReturnType = infer(state, expr.Return or primitiveType("nil"))
        elseif retTy ~= nil then
            assert(retTy.Kind == "Free")
            state.ReturnType = makeIntersection(retTy.Bound, infer(state, expr.Return or primitiveType("nil")))

            return retTy
        else
            return errorReport(`Cannot 'return' outside of functions.`, expr.Position)
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

    local state: InferenceState = {
        ReturnType = nil
    }

    for _, expr in expressions do
        infer(state, expr)
    end

    for k, t in typeAssociations do
        print(`{ k }: { typeToString(t.Type) }`)
    end
end

return bidisemi
