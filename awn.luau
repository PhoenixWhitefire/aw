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

type FunctionTopType = {
    read Kind: "Primitive",
    read Primitive: "function",
}

type PrimitiveType =
    | NilType
    | BooleanType
    | NumberType
    | StringType
    | FunctionTopType

type SingletonType = {
    read Kind: "Singleton",
    read Value: string | boolean,
}

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
    read IsTop: boolean?,
}

type GenericInstantiationType = {
    read Kind: "GenericInstantiation",
    read Name: string,
    read Arguments: { read Type },
}

type NegationType = {
    read Kind: "Negation",
    read Negated: Type,
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
    UpperBound: Type,
    LowerBound: Type,
}

type TypeData =
    | ErrorType
    | NeverType
    | UnknownType
    | PrimitiveType
    | SingletonType
    | GenericType
    | FunctionType
    | GenericInstantiationType
    | NegationType
    | UnionType
    | IntersectionType
    | FreeType

type Type = { AliasName: string? } & TypeData

type NormalizedFunctionComponentType = {
    Parameters: { NormalizedType },
    Return: NormalizedType?,
}

type NormalizedFunctionType = {
    -- `nil` = top 'function' type.
    Components: { NormalizedFunctionComponentType }?
}

type NormalizedFreeType = {
    Free: Type,
    UpperBound: NormalizedType,
    LowerBound: NormalizedType,
}

type NormalizedType = {
    Error:   boolean,
    Top:     boolean,
    Nil:     boolean,
    -- `nil` = not present.
    Boolean: {
        -- `nil` = top `boolean`, otherwise singleton.
        Value: boolean?,
    }?,
    Number:  boolean,
    -- `nil` = not present.
    String:  {
        -- If this is `nil`, we're the top 'string'.
        Singletons: { string }?,
        -- Whether we're a union of negated singletons.
        -- (took this term directly from Luau :)
        Cofinite: boolean,
    }?,
    -- `nil` = not present.
    Function: NormalizedFunctionType?,
    -- Union of generic types.
    Generic: { string }?,
    -- Union of free types.
    Free: { NormalizedFreeType }?,
}

local function unreachable(x: never): never
    error(`Unexpected branch with value '{ x }' ({ typeof(x) }), this is an internal error.`)
end

local function makeType(ty: TypeData): Type
    local t = ty :: Type
    return t
end

local function errorType()
    return makeType{
        Kind = "error",
    }
end

local function neverType()
    return makeType{
        Kind = "never",
    }
end

local function unknownType()
    return makeType{
        Kind = "unknown",
    }
end

local function primitiveType(primitive: index<PrimitiveType, "Primitive">)
    return makeType{
        Kind = "Primitive",
        -- TODO luau cast
        Primitive = primitive :: "nil",
    }
end

local function singletonType(value)
    return makeType{
        Kind = "Singleton",
        Value = value,
    }
end

local function genericType(name: string)
    return makeType{
        Kind = "Generic",
        Name = name,
    }
end

local function functionType(generics: { read GenericType }, parameters: { read Type }, variadic: Type, returns: Type?)
    return makeType{
        Kind = "Function",
        Generics = generics,
        Parameters = parameters,
        Variadic = variadic,
        Returns = returns,
    }
end

local function genericInstantationType(name: string, arguments: { read Type })
    return makeType{
        Kind = "GenericInstantiation",
        Name = name,
        Arguments = arguments,
    }
end

local function unionType(components: { read Type })
    return makeType{
        Kind = "Union",
        Components = components,
    }
end

local function intersectionType(components: { read Type })
    return makeType{
        Kind = "Intersection",
        Components = components,
    }
end

local function freeType()
    return makeType{
        Kind = "Free",
        UpperBound = unknownType(),
        LowerBound = neverType(),
    }
end

local function errorReport(message, position: caw.Position): Type
    print(`{ position.Line }:{ position.Character }: { message }`)
    return errorType()
end

local function makeUnion(a: Type, b: Type)
    local components = {}

    if a.Kind == "Union" then
        table.move(a.Components, 1, #a.Components, 1, components)
    else
        components[1] = a
    end

    if b.Kind == "Union" then
        table.move(b.Components, 1, #b.Components, 1, components)
    else
        table.insert(components, b)
    end

    return makeType{
        Kind = "Union",
        Components = components,
    }
end

local function makeIntersection(a: Type, b: Type)
    local components = {}

    if a.Kind == "Intersection" then
        table.move(a.Components, 1, #a.Components, 1, components)
    else
        components[1] = a
    end

    if b.Kind == "Intersection" then
        table.move(b.Components, 1, #b.Components, 1, components)
    else
        table.insert(components, b)
    end

    return makeType{
        Kind = "Intersection",
        Components = components,
    }
end

local function normalizedUnknown(): NormalizedType
    return {
        Error = false,
        Top = true,
        Nil = true,
        Boolean = {},
        Number = true,
        String = { Cofinite = false },
        Function = {},
        Generic = {},
        Free = {},
    }
end

local function normalizedNever(): NormalizedType
    return {
        Error = false,
        Top = false,
        Nil = false,
        Boolean = nil,
        Number = false,
        String = nil,
        Function = nil,
    }
end

local function toNever(n: NormalizedType)
    n.Error = false
    n.Top = false
    n.Nil = false
    n.Boolean = nil
    n.Number = false
    n.String = nil
    n.Function = nil
    n.Generic = nil
end

local function toUnknown(n: NormalizedType)
    n.Error = false
    n.Top = true
    n.Nil = true
    n.Boolean = {}
    n.Number = true
    n.String = { Cofinite = false }
    n.Function = {}
    n.Generic = {}
    n.Free = {}
end

-- TODO string singleton
local function isNever(n: NormalizedType)
    return not n.Top and not n.Nil and not n.Boolean
        and not n.Number and not n.String and not n.Function
        and not n.Generic and not n.Free
end

local function isUnknown(n: NormalizedType)
    return n.Top
end

local function dumpNormalization(n: NormalizedType)
    local t = {}

    if n.Error then
        table.insert(t, "*error*")
    end

    if n.Top then
        table.insert(t, "unknown")
        return table.concat(t, " | ")
    end

    if n.Nil then
        table.insert(t, "nil")
    end

    if n.Boolean then
        if n.Boolean.Value ~= nil then
            table.insert(t, tostring(n.Boolean.Value))
        else
            table.insert(t, "boolean")
        end
    end

    if n.Number then
        table.insert(t, "number")
    end

    if n.String then
        if n.String.Singletons then
            local s = table.create<<string>>(#n.String.Singletons)

            for i, str in n.String.Singletons do
                s[i] = if n.String.Cofinite then `~"{ str }"` else `"{ str }"`
            end

            if #s == 1 then
                table.insert(t, s[1])
            else
                table.insert(t, `({ table.concat(s, if n.String.Cofinite then " & " else " | ") })`)
            end
        else
            table.insert(t, "string")
        end
    end

    if n.Function then
        if n.Function.Components then
            local fs = table.create<<string>>(#n.Function.Components)

            for i, f in n.Function.Components do
                local params = table.create<<string>>(#f.Parameters)

                for i, param in f.Parameters do
                    params[i] = dumpNormalization(param)
                end

                fs[i] = `({ table.concat(params, ", ") }) -> { if f.Return then dumpNormalization(f.Return) else "()" }`
            end

            table.insert(t, table.concat(fs, " & "))
        else
            table.insert(t, "function")
        end
    end

    if n.Generic then
        if #n.Generic == 0 then
            table.insert(t, "*G")
        else
            table.insert(t, table.concat(n.Generic, " | "))
        end
    end

    if n.Free then
        if #n.Free == 0 then
            table.insert(t, "'*F")
        else
            local frees = table.create<<string>>(#n.Free)

            for i, free in n.Free do
                frees[i] = `t{i}` -- <: { dumpNormalization(free.Bound) }`
            end

            table.insert(t, table.concat(frees, " | "))
        end
    end

    if #t == 0 then
        return "never"
    else
        return table.concat(t, " | ")
    end
end

local function unionNormalizedType(n: NormalizedType, with: NormalizedType)
    n.Error = n.Error or with.Error
    n.Top = n.Top or with.Top
    n.Nil = n.Nil or with.Nil
    n.Number = n.Number or with.Number

    if n.Boolean and with.Boolean then
        local myBoolean = n.Boolean.Value
        local withBoolean = with.Boolean.Value

        if not myBoolean and withBoolean then
            n.Boolean.Value = nil -- Become top `boolean`.
        elseif (myBoolean == false and withBoolean == true)
            or (myBoolean == true and withBoolean == false)
        then
            n.Boolean.Value = nil -- Become top `boolean`.
        end
    else
        n.Boolean = n.Boolean or with.Boolean
    end

    if n.String and with.String then
        local mySingles = n.String.Singletons
        local withSingles = with.String.Singletons

        if mySingles and withSingles then
            if n.String.Cofinite and not with.String.Cofinite then
                for i, s in withSingles do
                    local foundIndex = table.find(mySingles, s)
                    if foundIndex then
                        table.remove(mySingles, foundIndex)
                    end
                end
            else
                table.move(withSingles, 1, #withSingles, #mySingles + 1, mySingles)
            end
        elseif mySingles and not withSingles then
            n.String = { Cofinite = false } -- Union singletons with top `string`.
        end
    else
        n.String = n.String or with.String
    end

    if not n.Function and with.Function then
        n.Function = table.clone(with.Function)
    elseif n.Function and with.Function and not with.Function.Components and n.Function.Components then
        n.Function = {}
    elseif n.Function and with.Function and n.Function.Components and with.Function.Components then
        table.move(with.Function.Components, 1, #with.Function.Components, #n.Function.Components + 1, n.Function.Components)
    end

    if n.Generic and #n.Generic > 0 and with.Generic and #with.Generic > 0 then
        table.move(with.Generic, 1, #with.Generic, #n.Generic + 1, n.Generic)
    end

    if not n.Free and with.Free then
        n.Free = with.Free
    elseif n.Free and with.Free then
        table.move(with.Free, 1, #with.Free, #n.Free + 1, n.Free)
    end
end

local function intersectNormalizedType(n: NormalizedType, with: NormalizedType)
    n.Error = n.Error or with.Error
    n.Top = n.Top and with.Top
    n.Nil = n.Nil and with.Nil
    n.Boolean = n.Boolean and with.Boolean
    n.Number = n.Number and with.Number

    if n.String and with.String then
        local mySingles = n.String.Singletons
        local withSingles = with.String.Singletons

        if mySingles and withSingles then
            local removed = false

            repeat
                removed = false

                for i, s in mySingles do
                    local found = table.find(withSingles, s) ~= nil

                    if not found and not with.String.Cofinite and not n.String.Cofinite then
                        table.remove(mySingles, i)
                        removed = true
                    end
                end
            until not removed

            if n.String.Cofinite and with.String.Cofinite then
                for i, s in withSingles do
                    local found = table.find(mySingles, s) ~= nil

                    if not found then
                        table.insert(mySingles, s)
                    end
                end
            end
        elseif not mySingles and withSingles then
            n.String.Singletons = table.clone(withSingles) -- `string & "a" = "a"`.
            n.String.Cofinite = with.String.Cofinite
        end
    else
        n.String = nil
    end

    if n.Function and with.Function and with.Function.Components then
        if n.Function.Components then
            table.move(with.Function.Components, 1, #with.Function.Components, #n.Function.Components + 1, n.Function.Components)
        else
            n.Function.Components = with.Function.Components
        end
    elseif n.Function and not with.Function then
        n.Function = nil
    end

    if n.Generic and not with.Generic then
        n.Generic = nil
    elseif n.Generic and #n.Generic == 0 and with.Generic and #with.Generic > 0 then
        n.Generic = table.clone(with.Generic)
    end

    if n.Free and not with.Free then
        n.Free = nil
    elseif n.Free and #n.Free == 0 and with.Free and #with.Free > 0 then
        n.Free = table.clone(with.Free)
    end
end

local function normalize(ty: Type): NormalizedType
    local n = normalizedNever()

    if ty.Kind == "error" then
        n.Error = true
    elseif ty.Kind == "never" then
        -- Default state is `never`.
    elseif ty.Kind == "unknown" then
        toUnknown(n)
    elseif ty.Kind == "Primitive" then
        if ty.Primitive == "nil" then
            n.Nil = true
        elseif ty.Primitive == "boolean" then
            n.Boolean = {}
        elseif ty.Primitive == "number" then
            n.Number = true
        elseif ty.Primitive == "string" then
            n.String = { Cofinite = false }
        elseif ty.Primitive == "function" then
            n.Function = {}
        else
            unreachable(ty.Primitive)
            error(`Unreachable.`)
        end
    elseif ty.Kind == "Singleton" then
        if typeof(ty.Value) == "boolean" then
            n.Boolean = { Value = ty.Value }
        elseif typeof(ty.Value) == "string" then
            n.String = { Singletons = { ty.Value }, Cofinite = false }
        else
            error(`Unsupported singleton kind '{ typeof(ty.Value) }'.`)
        end
    elseif ty.Kind == "Generic" then
        toUnknown(n)
        n.Generic = { ty.Name }

    elseif ty.Kind == "Function" then
        if ty.IsTop then
            n.Function = {}
        else
            local params = table.create<<NormalizedType>>(#ty.Parameters)

            for i, param in ty.Parameters do
                params[i] = normalize(param)
            end

            n.Function = {
                Components = {
                    {
                        Parameters = params,
                        Return = if ty.Returns then normalize(ty.Returns) else nil,
                    },
                }
            }
        end

    elseif ty.Kind == "GenericInstantiation" then
        toUnknown(n)
        n.Error = true
        print(`Cannot normalize generic instantiations.`)

    elseif ty.Kind == "Negation" then
        n = normalize(ty.Negated)

        if n.Top then
            toNever(n)
        elseif isNever(n) then
            toUnknown(n)
        else
            n.Nil = not n.Nil

            if n.Boolean then
                if n.Boolean.Value then
                    n.Boolean.Value = not n.Boolean.Value
                else
                    n.Boolean = nil
                end
            else
                n.Boolean = {}
            end

            n.Number = not n.Number

            if n.String then
                if n.String.Singletons then
                    n.String.Cofinite = not n.String.Cofinite
                else
                    n.String = nil
                end
            else
                n.String = { Cofinite = false }
            end

            if n.Function and n.Function.Components or n.Generic or n.Free then
                print(`Cannot negate non-top function, generic, or free types.`)
                n.Error = true
            end

            n.Function = if n.Function then nil else {}
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
        n.Free = {
            {
                Free = ty,
                UpperBound = normalize(ty.UpperBound),
                LowerBound = normalize(ty.LowerBound),
            }
        }
    else
        unreachable(ty.Kind)
        error(`Unreachable.`)
    end

    return n
end

local isSubtypeOf
local isSupertypeOf

type SubtypingResult =
    | {
        read Holds: true,
        read FailedPath: nil,
      }
    | {
        read Holds: false,
        read FailedPath: string,
      }

local function subtypeSuccess(): SubtypingResult
    return { Holds = true }
end

local function subtypeFailure(failed): SubtypingResult
    return { Holds = false, FailedPath = failed }
end

local function normalizedNil()
    local n = normalizedNever()
    n.Nil = true

    return n
end

local function normalizedBoolean(singleton: boolean?)
    local n = normalizedNever()
    local boolean = {}

    if singleton ~= nil then
        boolean.Value = singleton
    end

    n.Boolean = boolean :: any -- TODO luau cast
    return n
end

local function normalizedString(singleton: string?)
    local n = normalizedNever()
    local stringData = {}

    if singleton ~= nil then
        stringData.Singletons = { singleton }
    end

    n.String = stringData :: any -- TODO luau cast
    return n
end

-- TODO luau any
local function deepCopy(tab: any): any
    assert(typeof(tab) == "table")
    local t = table.clone(tab)

    for k, v in t do -- TODO luau, `intersect<>` should always reduce methinks? only occurs when function signature is `local function deepCopy<T>(tab: T & {}): T`
        if typeof(v) == "table" then
            t[k] = deepCopy(v)
        else
            t[k] = v
        end
    end

    return t
end

local function intersectApplicableReturns(components: { NormalizedFunctionComponentType }, argument: NormalizedType): NormalizedType?
    local applicable = {}

    for _, c in components do
        if #c.Parameters == 1 and isSubtypeOf(argument, c.Parameters[1]).Holds then
            table.insert(applicable, c.Return)
        end
    end

    if #applicable == 0 then
        return nil
    end

    local returned: NormalizedType = deepCopy(applicable[1])

    for i = 2, #applicable do
        intersectNormalizedType(returned, applicable[i])
    end

    return returned
end

local function decomposeToSingletons(n: NormalizedType): { NormalizedType }?
    if n.Top or n.Number or n.Function or n.Generic or n.Free then
        return nil
    end

    local singles = {}

    if n.Nil then
        table.insert(singles, normalizedNil())
    end

    if n.Boolean then
        if n.Boolean.Value == nil then
            table.insert(singles, normalizedBoolean(true))
            table.insert(singles, normalizedBoolean(false))
        else
            table.insert(singles, normalizedBoolean(n.Boolean.Value))
        end
    end

    if n.String then
        if n.String.Singletons then
            for _, s in n.String.Singletons do
                table.insert(singles, normalizedString(s))
            end
        else
            -- Top `string`.
            return nil
        end
    end

    return singles
end

local function isFunctionSubtypeOf(a: NormalizedFunctionType, b: NormalizedFunctionType): SubtypingResult
    -- `((A) -> B) & ((C) -> D) & ... <: function`
    if not b.Components then
        return subtypeSuccess()
    end

    -- `function </: ((A) -> B) & ((C) -> D) & ...`
    if not a.Components then
        return subtypeFailure(`Unexpected 'top' function type.`)
    end

    for i, cb in b.Components do
        local singles = nil

        if #cb.Parameters == 1 then
            singles = decomposeToSingletons(cb.Parameters[1])
        end

        if singles then
            for _, single in singles do
                local car = intersectApplicableReturns(a.Components, single)

                if not car then
                    return subtypeFailure(`No overload accepts '{ dumpNormalization(single) }'`)
                end

                if cb.Return then
                    local result = isSubtypeOf(car, cb.Return)

                    if not result.Holds then
                        return subtypeFailure(`With '{ dumpNormalization(single) }': { result.FailedPath }`)
                    end
                end
            end
        else
            local found = false
            local failed = {}

            for ica, ca in a.Components do
                local paramStrings = table.create<<string>>(#ca.Parameters)

                for i, param in ca.Parameters do
                    paramStrings[i] = dumpNormalization(param)
                end

                local signature = `({ table.concat(paramStrings, ", ") }) -> { if ca.Return then dumpNormalization(ca.Return) else "()" }`

                if #ca.Parameters ~= #cb.Parameters then
                    table.insert(failed, `'{ signature }' has a differing number of parameters.`)
                    continue
                end

                local paramsMatched = true

                for i, param in ca.Parameters do
                    local paramResult = isSupertypeOf(param, cb.Parameters[i])

                    if not paramResult.Holds then
                        table.insert(
                            failed,
                            `'{ signature }' does not accept parameter #{ i } '{ dumpNormalization(cb.Parameters[i]) }'.`
                        )
                        paramsMatched = false
                        continue
                    end
                end

                if not paramsMatched then
                    continue
                end

                if ca.Return and cb.Return then
                    local returnResult = isSubtypeOf(ca.Return, cb.Return)

                    if not returnResult.Holds then
                        table.insert(
                            failed,
                            `'{ signature }' does not return '{ dumpNormalization(cb.Return) }'.`
                        )
                        continue
                    end
                end

                found = true
                break
            end

            if not found then
                local paramStrings = table.create<<string>>(#cb.Parameters)

                for i, param in cb.Parameters do
                    paramStrings[i] = dumpNormalization(param)
                end

                local signature = `({ table.concat(paramStrings, ", ") }) -> { if cb.Return then dumpNormalization(cb.Return) else "()" }`

                return subtypeFailure(`No compatible overload was found for #{ i } '{ signature }':\n* { table.concat(failed, "\n* ") }`)
            end
        end
    end

    return subtypeSuccess()
end

function isSubtypeOf(a: NormalizedType, b: NormalizedType): SubtypingResult
    if a.Error or b.Error then
        return subtypeSuccess()
    end

    if b.Top then
        return subtypeSuccess()
    end

    if a.Nil and not b.Nil then
        return subtypeFailure("Unexpected `nil`.")
    end

    if a.Boolean and not b.Boolean then
        return subtypeFailure("Unexpected `boolean`.")
    -- `boolean </: true` etc.
    elseif a.Boolean and a.Boolean.Value == nil and b.Boolean and b.Boolean.Value ~= nil then
        return subtypeFailure(`Expected exactly '{ b.Boolean.Value }', got 'boolean' instead.`)
    elseif a.Boolean and a.Boolean.Value ~= nil and b.Boolean and b.Boolean.Value ~= nil then
        -- `false </: true`
        if a.Boolean.Value ~=  b.Boolean.Value then
            return subtypeFailure(`Expected '{ b.Boolean.Value }', got '{ a.Boolean.Value }' instead.`)
        end
    end

    if a.Number and not b.Number then
        return subtypeFailure("Unexpected `number`.")
    end

    if a.String and not b.String then
        return subtypeFailure("Unexpected 'string'.")
    -- `string </: "a" | "b" | ...`
    elseif a.String and not a.String.Singletons and b.String and b.String.Singletons then
        if b.String.Cofinite then
            return subtypeFailure(`Top 'string' includes negated types like "{ b.String.Singletons[1] }".`)
        else
            return subtypeFailure(`Expected string singletons such as "{ b.String.Singletons[1] }", got top 'string' instead.`)
        end

    elseif a.String and a.String.Singletons and b.String and b.String.Singletons then
        for _, as in a.String.Singletons do
            local found = table.find(b.String.Singletons, as) ~= nil

            if found and b.String.Cofinite then
                return subtypeFailure(`'"{ as }"' is excluded by '~"{ as }"'.`)
            elseif not found and not b.String.Cofinite then
                return subtypeFailure(`Unexpected '"{ as }"', expected singleton like '"{ b.String.Singletons[1] }"'.`)
            end
        end
    end

    if a.Function and not b.Function then
        return subtypeFailure("Unexpected function.")
    -- `function </: ((A) -> B) & ((C) -> D) & ...`
    elseif a.Function and b.Function then
        local result = isFunctionSubtypeOf(a.Function, b.Function)

        if not result.Holds then
            return result
        end
    end

    if a.Free then
        for _, free in a.Free do
            assert(isSubtypeOf(free.LowerBound, free.UpperBound))

            local ubResult = isSubtypeOf(free.UpperBound, b)

            if not ubResult.Holds then
                return ubResult
            end

            local lbResult = isSupertypeOf(free.LowerBound, b)

            if not lbResult.Holds then
                return lbResult
            end
        end
    end

    return subtypeSuccess()
end

function isSupertypeOf(a: NormalizedType, b: NormalizedType)
    return isSubtypeOf(b, a)
end

local freeTypeNameCount = 0
local instantiateType

local function typeToString(ty: Type, alias: string?): string
    if ty.AliasName and ty.AliasName ~= alias then
        return ty.AliasName
    end

    local function freeTypeName(tyvar: FreeType)
        if tyvar.Name then
            return tyvar.Name
        else
            local name = `'{ string.char((freeTypeNameCount + 19) % 26 + 78) }`

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
    elseif ty.Kind == "Singleton" then
        if typeof(ty.Value) == "string" then
            return `"{ ty.Value }"`
        else
            return tostring(ty.Value)
        end
    elseif ty.Kind == "Generic" then
        return ty.Name
    elseif ty.Kind == "Function" then
        if ty.IsTop then
            return "function"
        end

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

    elseif ty.Kind == "GenericInstantiation" then
        local arguments = table.create<<string>>(#ty.Arguments)

        for i, a in ty.Arguments do
            arguments[i] = typeToString(a)
        end

        return `{ ty.Name }<{ table.concat(arguments, ", ") }> => { typeToString(instantiateType(ty)) }`

    elseif ty.Kind == "Negation" then
        local inner = typeToString(ty.Negated)

        if ty.Negated.Kind == "Union" or ty.Negated.Kind == "Intersection" then
            return `~({ inner })`
        else
            return `~{ inner }`
        end

    elseif ty.Kind == "Union" then
        local tys = table.create<<string>>(#ty.Components)
        local length = 0

        for i, c in ty.Components do
            local ts = typeToString(c)
            length += #ts

            if c.Kind == "Union" or c.Kind == "Intersection" then
                tys[i] = `({ ts })`
            else
                tys[i] = ts
            end
        end

        return table.concat(tys, if length < 24 then " | " else "\n  | ")
    elseif ty.Kind == "Intersection" then
        local tys = table.create<<string>>(#ty.Components)
        local length = 0

        for i, c in ty.Components do
            local ts = typeToString(c)
            length += #ts

            if c.Kind == "Union" or c.Kind == "Intersection" then
                tys[i] = `({ ts })`
            else
                tys[i] = ts
            end
        end

        return table.concat(tys, if length < 24 then " & " else "\n  & ")
    elseif ty.Kind == "Free" then
        local str = freeTypeName(ty)

        if ty.UpperBound.Kind ~= "unknown" then
            str ..= ` <: { typeToString(ty.UpperBound) }`
        end

        if ty.LowerBound.Kind ~= "never" then
            str = `{ typeToString(ty.LowerBound) } <: ` .. str
        end

        return str
    else
        unreachable(ty.Kind)
        error(`Unreachable.`)
    end
end

local function normalizationToType(n: NormalizedType): Type
    local t: { Type } = {}

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
            if n.Boolean.Value ~= nil then
                table.insert(t, singletonType(n.Boolean.Value))
            else
                table.insert(t, primitiveType("boolean"))
            end
        end

        if n.Number then
            table.insert(t, primitiveType("number"))
        end

        if n.String then
            if n.String.Singletons then
                local strs = {}

                for _, s in n.String.Singletons do
                    local sty = singletonType(s)

                    if n.String.Cofinite then
                        table.insert(strs, makeType{ Kind = "Negation", Negated = sty })
                    else
                        table.insert(strs, sty)
                    end
                end

                if n.String.Cofinite then
                    table.insert(t, intersectionType(strs))
                else
                    table.insert(t, unionType(strs))
                end
            else
                table.insert(t, primitiveType("string"))
            end
        end

        if n.Function then
            if n.Function.Components then
                local cs = table.create<<Type>>(#n.Function.Components)

                for i, f in n.Function.Components do
                    local params = table.create<<Type>>(#f.Parameters)

                    for i, param in f.Parameters do
                        params[i] = normalizationToType(param)
                    end

                    cs[i] = functionType({}, params, neverType(), if f.Return then normalizationToType(f.Return) else nil)
                end

                if #cs == 1 then
                    table.insert(t, cs[1])
                else
                    table.insert(t, intersectionType(cs))
                end
            else
                local f: FunctionType = {
                    Kind = "Function",
                    Generics = {},
                    Parameters = {},
                    IsTop = true,
                }

                table.insert(t, makeType(f))
            end
        end

        if n.Generic then
            if #n.Generic == 0 then
                print("Top generic type??")
            else
                local gens = table.create<<Type>>(#n.Generic)

                for i, gen in n.Generic do
                    gens[i] = genericType(gen)
                end

                table.insert(t, unionType(gens))
            end
        end

        if n.Free then
            if #n.Free == 0 then
                print("Top free type??")
            else
                local frees = table.create<<Type>>(#n.Free)

                for i, free in n.Free do
                    frees[i] = free.Free
                end

                table.insert(t, unionType(frees))
            end
        end
    end

    if #t == 0 then
        return neverType()
    elseif #t == 1 then
        return t[1]
    end

    return unionType(t)
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
local typeAssociations: { [string]: { Expression: caw.Expression, Type: Type }? } = {
    valuenever = {
        Expression = {
            Type = "Constant",
            Constant = {
                Type = "nil"
            },
            Position = {
                Line = 0,
                Character = 0,
                Offset = 0,
            }
        },
        Type = neverType()
    },
}
local typeEnvOrdered = {}
local typeAssociationsOrdered = {}

local typeAnnotationToType
local check

-- `Target <: Super`. Can modify `Target.Bound` if `Target` is Free.
-- Otherwise report error if relation does not hold.
-- TODO: Free type lower bounds: If `Super` is Free... then... we can do lower bounds I guess?
type SubtypeConstraint = {
    read Position: caw.Position,
    read Target: Type,
    read Super: Type,
}

local function subtypeConstraint(position, target, super): SubtypeConstraint
    return {
        Position = position,
        Target = target,
        Super = super,
    }
end

local function dispatchSubtypeConstraints(constraints: { SubtypeConstraint })
    for _, sc in constraints do
        if sc.Target.Kind == "Free" then
            if sc.Target ~= sc.Super then
                sc.Target.UpperBound = makeIntersection(sc.Target.UpperBound, sc.Super)
            end
        elseif sc.Super.Kind == "Free" then
            if sc.Target ~= sc.Super then
                sc.Super.LowerBound = makeIntersection(sc.Super.LowerBound, sc.Target)
            end
        else
            check(sc.Target, sc.Super, sc.Position)
        end
    end

    table.clear(constraints)
end

type InferenceState = {
    ReturnType: Type? | true,
    SubtypeConstraints: { SubtypeConstraint },
}

local function infer(state: InferenceState, expr: caw.Expression, expected: Type?): Type?
    if expr.Type == "Constant" then
        if expr.Constant.Type == "boolean" or expr.Constant.Type == "string" then
            return singletonType(expr.Constant.Value)
        else
            return primitiveType(expr.Constant.Type)
        end

    elseif expr.Type == "Declaration" then
        local anno = if expr.Declaration.Annotation then typeAnnotationToType(expr.Position, expr.Declaration.Annotation) else nil
        local ty = if expr.Declaration.Value then infer(state, expr.Declaration.Value, anno) else primitiveType("nil")

        if not ty then
            errorReport(`Trying to assign '{ expr.Declaration.Name }' to an expression returning no values.`, expr.Position)
        end

        -- TODO luau type error on `ty` when `anno or` is removed
        typeAssociations[expr.Declaration.Name] = {
            Expression = expr,
            Type = anno or ty or errorType()
        }
        table.insert(typeAssociationsOrdered, expr.Declaration.Name)

        if anno and ty then
            local position = if expr.Declaration.Value then expr.Declaration.Value.Position else expr.Position
            table.insert(state.SubtypeConstraints, subtypeConstraint(position, ty, anno))
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
                return errorReport(`Undefined variable '{ variable.Reference }'`, variable.Position)
            end

            if ref.Expression.Type == "Function" then -- Function parameter.
                if ref.Type.Kind == "Free" then
                    local ty = makeIntersection(ref.Type.UpperBound, infer(state, value))
                    ref.Type.UpperBound = ty

                    return ty
                else
                    return ref.Type
                end
            end

            assert(ref.Expression.Type == "Declaration", ref.Expression.Type)

            local valueTy = infer(state, value)
            local annoTy = if ref.Expression.Declaration.Annotation then typeAnnotationToType(ref.Expression.Position, ref.Expression.Declaration.Annotation) else nil

            if annoTy then
                table.insert(state.SubtypeConstraints, subtypeConstraint(value.Position, valueTy, annoTy))
            else
                typeAssociations[variable.Reference] = {
                    Expression = ref.Expression,
                    Type = makeUnion(ref.Type, valueTy),
                }
            end

            return annoTy or valueTy
        elseif expr.Operator == "+" or expr.Operator == "-" or expr.Operator == "*"
            or expr.Operator == "/" or expr.Operator == "//"
        then
            local leftTy = infer(state, expr.Left)
            local rightTy = infer(state, expr.Right)

            if not leftTy then
                return errorReport(`LHS is expression returning no value.`, expr.Left.Position)
            end

            if not rightTy then
                return errorReport(`RHS is expression returning no value.`, expr.Right.Position)
            end

            table.insert(state.SubtypeConstraints, subtypeConstraint(expr.Left.Position, leftTy, primitiveType("number")))
            table.insert(state.SubtypeConstraints, subtypeConstraint(expr.Left.Position, rightTy, primitiveType("number")))

            return primitiveType("number")

        elseif expr.Operator == ".." then
            local leftTy = infer(state, expr.Left)
            local rightTy = infer(state, expr.Right)

            if not leftTy then
                return errorReport(`LHS is expression returning no value.`, expr.Left.Position)
            end

            if not rightTy then
                return errorReport(`RHS is expression returning no value.`, expr.Right.Position)
            end

            table.insert(state.SubtypeConstraints, subtypeConstraint(expr.Left.Position, leftTy, primitiveType("string")))
            table.insert(state.SubtypeConstraints, subtypeConstraint(expr.Left.Position, rightTy, primitiveType("string")))

            return primitiveType("string")

        elseif expr.Operator == "<" or expr.Operator == "<=" or expr.Operator == ">"
            or expr.Operator == ">="
        then
            local leftTy = infer(state, expr.Left)
            local rightTy = infer(state, expr.Right)

            if not leftTy then
                return errorReport(`LHS is expression returning no value.`, expr.Left.Position)
            end

            if not rightTy then
                return errorReport(`RHS is expression returning no value.`, expr.Right.Position)
            end

            table.insert(state.SubtypeConstraints, subtypeConstraint(expr.Left.Position, leftTy, primitiveType("number")))
            table.insert(state.SubtypeConstraints, subtypeConstraint(expr.Left.Position, rightTy, primitiveType("number")))

            return primitiveType("boolean")

        elseif expr.Operator == "==" or expr.Operator == "~=" then
            local leftTy = infer(state, expr.Left)
            local rightTy = infer(state, expr.Right)

            if not leftTy then
                return errorReport(`LHS is expression returning no value.`, expr.Left.Position)
            end

            if not rightTy then
                return errorReport(`RHS is expression returning no value.`, expr.Right.Position)
            end

            -- TODO: This currently results in a recursive normalization.
            -- table.insert(state.SubtypeConstraints, subtypeConstraint(expr.Left.Position, leftTy, rightTy))
            -- table.insert(state.SubtypeConstraints, subtypeConstraint(expr.Left.Position, rightTy, leftTy))
            return primitiveType("boolean")
        else
            unreachable(expr.Operator)
            error(`Cannot infer '{ expr.Operator }' operations.`)
        end
    elseif expr.Type == "Function" then
        local gens = table.create<<GenericType>>(#expr.Function.Parameters)
        local params = table.create<<Type>>(#expr.Function.Parameters)

        for i, param in expr.Function.Parameters do
            local ty = if param.Annotation then typeAnnotationToType(expr.Position, param.Annotation)
                        else if expected and expected.Kind == "Function" then expected.Parameters[i] or freeType()
                        else freeType()

            params[i] = ty

            typeAssociations[param.Name] = {
                Expression = expr,
                Type = ty,
            }
        end

        if expr.Function.Name then
            typeAssociations[expr.Function.Name] = {
                Type = functionType({}, params, neverType(), freeType()),
                Expression = expr,
            }

            table.insert(typeAssociationsOrdered, expr.Function.Name)
        end

        local prevRetTy = state.ReturnType
        state.ReturnType = if expected and expected.Kind == "Function" then expected.Returns else true

        for _, expr in expr.Function.Body do
            infer(state, expr)
        end

        dispatchSubtypeConstraints(state.SubtypeConstraints)

        local retTy = state.ReturnType
        state.ReturnType = prevRetTy

        local freeTypeSubstitutions: { [Type]: Type } = {}

        for i, param in expr.Function.Parameters do
            typeAssociations[param.Name] = nil

            local paramTy = params[i]

            if paramTy.Kind == "Free" then
                if paramTy.LowerBound.Kind == "never" and paramTy.UpperBound.Kind == "unknown" then
                    local gen = genericType(string.char((#gens + 19) % 26 + 65)) :: GenericType
                    freeTypeSubstitutions[paramTy] = gen :: Type

                    table.insert(gens, gen)
                else
                    local replacement = if paramTy.LowerBound.Kind ~= "never" then normalize(paramTy.LowerBound) else normalize(paramTy.UpperBound)
                    local bound = normalizationToType(replacement)
                    freeTypeSubstitutions[paramTy] = bound

                    if retTy == paramTy then
                        retTy = bound
                    end
                end
            end
        end

        local function substituteAllIn(ty: Type): Type
            if ty.Kind == "error" or ty.Kind == "never" or ty.Kind == "unknown"
                or ty.Kind == "Primitive" or ty.Kind == "Singleton" or ty.Kind == "Generic"
            then
                return ty
            elseif ty.Kind == "Free" then
                if freeTypeSubstitutions[ty] then
                    return freeTypeSubstitutions[ty]
                end

                if ty.LowerBound.Kind ~= "never" and ty.UpperBound.Kind == "unknown" then
                    return substituteAllIn(ty.LowerBound)
                else
                    return makeType{
                        Kind = "Free",
                        UpperBound = substituteAllIn(ty.UpperBound),
                        LowerBound = substituteAllIn(ty.LowerBound),
                    }
                end
            elseif ty.Kind == "Function" then
                local params = table.create<<Type>>(#ty.Parameters)

                for i, param in ty.Parameters do
                    params[i] = substituteAllIn(param)
                end

                local rets = if ty.Returns then substituteAllIn(ty.Returns) else nil
                return functionType(ty.Generics, params, neverType(), rets)

            elseif ty.Kind == "Negation" then
                return makeType{
                    Kind = "Negation",
                    Negated = substituteAllIn(ty.Negated),
                }

            elseif ty.Kind == "Union" then
                local components = table.create<<Type>>(#ty.Components)

                for i, c in ty.Components do
                    components[i] = substituteAllIn(c)
                end

                return unionType(components)

            elseif ty.Kind == "Intersection" then
                local components = table.create<<Type>>(#ty.Components)

                for i, c in ty.Components do
                    components[i] = substituteAllIn(c)
                end

                return intersectionType(components)

            elseif ty.Kind == "GenericInstantiation" then
                local arguments = table.create<<Type>>(#ty.Arguments)

                for i, a in ty.Arguments do
                    arguments[i] = substituteAllIn(a)
                end

                return genericInstantationType(ty.Name, arguments)
            else
                unreachable(ty.Kind)
                error(`Unreachable.`)
            end
        end

        for i, param in params do
            params[i] = substituteAllIn(param)
        end

        if typeof(retTy) == "table" then
            retTy = substituteAllIn(retTy)
        end

        if retTy == true then
            retTy = nil
        end

        local fty = functionType(gens, params, neverType(), retTy)

        if expr.Function.Name then
            typeAssociations[expr.Function.Name] = {
                Type = fty,
                Expression = expr,
            }
        end

        return fty

    elseif expr.Type == "Return" then
        local retTy = state.ReturnType

        if retTy == true then
            local inferred = expr.Return and infer(state, expr.Return)

            if inferred then
                state.ReturnType = makeType{
                    Kind = "Free",
                    UpperBound = unknownType(),
                    LowerBound = inferred,
                }
            else
                state.ReturnType = nil
            end

            assert(state.ReturnType ~= true)
            return state.ReturnType
        elseif retTy ~= nil then
            local thisRet

            if expr.Return then
                thisRet = infer(state, expr.Return)
            end

            if thisRet then
                if retTy.Kind == "Free" then
                    state.ReturnType = makeUnion(retTy.LowerBound, thisRet)

                    assert(state.ReturnType ~= true)
                    return assert(state.ReturnType)
                else
                    table.insert(state.SubtypeConstraints, subtypeConstraint(expr.Position, thisRet, retTy))
                    return retTy
                end
            end

            return nil
        else
            return errorReport(`Cannot 'return' outside of functions.`, expr.Position)
        end
    elseif expr.Type == "Call" then
        local fn = typeAssociations[expr.Call.Identifier]

        if not fn then
            return errorReport(`Unknown function '{ expr.Call.Identifier }'.`, expr.Position)
        end

        local fnTy = fn.Type

        local arguments = table.create<<Type>>(#expr.Call.Arguments)

        for i, a in expr.Call.Arguments do
            arguments[i] = infer(state, a, if fnTy.Kind == "Function" then fnTy.Parameters[i] else nil)
        end

        local checkFnTy = functionType({}, arguments, neverType(), expected or genericType("R"))
        table.insert(state.SubtypeConstraints, subtypeConstraint(expr.Position, fnTy, checkFnTy))

        dispatchSubtypeConstraints(state.SubtypeConstraints)

        local normFn = if fnTy.Kind == "Free" then normalize(fnTy.UpperBound) else normalize(fnTy)

        if not normFn.Function then
            return errorReport(`'{ normalizationToString(normFn) }' is not callable.`, expr.Position)
        end

        if not normFn.Function.Components then
            return errorReport(`The top 'function' type is not safe to call.`, expr.Position)
        end

        if #normFn.Function.Components == 1 then
            local retTy = normFn.Function.Components[1].Return
            return if retTy then normalizationToType(retTy) else nil
        end

        if #arguments ~= 1 then
            return errorReport(`Cannot perform overload resolution for functions not accepting exactly 1 parameter.`, expr.Position)
        end

        local normArgs = table.create<<NormalizedType>>(#arguments)

        for i, a in arguments do
            normArgs[i] = normalize(a)
        end

        local normReturn = intersectApplicableReturns(normFn.Function.Components, normArgs[1])

        if not normReturn then
            local argStrings = table.create<<string>>(#arguments)

            for i, a in arguments do
                argStrings[i] = typeToString(a)
            end

            return errorReport(`No overload of '{ typeToString(fnTy) }' accepts parameters '({ table.concat(argStrings, ", ") })'`, expr.Position)
        end

        return normalizationToType(normReturn)

    elseif expr.Type == "IfCondition" :: "IfCondition" then -- TODO luau cast?
        local result = nil

        for _, v in expr.IfBody do
            result = infer(state, v)
        end

        if expr.ElseBody then
            local last

            for _, v in expr.ElseBody do
                last = infer(state, v)
            end

            if last and result then
                result = makeUnion(result, last)
            end
        end

        if result and expected then
            table.insert(state.SubtypeConstraints, subtypeConstraint(expr.Position, result, expected))
        end

        return result

    elseif expr.Type == "TypeAliasDeclaration" then
        local ty = typeAnnotationToType(expr.Position, expr.Value)
        ty.AliasName = expr.Name

        typeEnv[expr.Name] = ty
        table.insert(typeEnvOrdered, expr.Name)
        return errorType()
    else
        -- unreachable(expr.Type)
        error(`Cannot infer { expr.Type }s.`)
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
    elseif annotation.Kind == "Singleton" then
        return singletonType(annotation.Value)

    elseif annotation.Kind == "Function" then
        local params = table.create<<Type>>(#annotation.Parameters)

        for i, param in annotation.Parameters do
            params[i] = typeAnnotationToType(position, param)
        end

        return functionType({}, params, neverType(), if annotation.Returns then typeAnnotationToType(position, annotation.Returns) else nil)

    elseif annotation.Kind == "GenericInstantiation" then
        local arguments = table.create<<Type>>(#annotation.Arguments)

        for i, a in annotation.Arguments do
            arguments[i] = typeAnnotationToType(position, a)
        end

        return genericInstantationType(annotation.Name, arguments)

    elseif annotation.Kind == "Negation" then
        return makeType{
            Kind = "Negation",
            Negated = typeAnnotationToType(position, annotation.Negated),
        }

    elseif annotation.Kind == "Union" then
        local components = table.create<<Type>>(#annotation.Components)

        for i, c in annotation.Components do
            components[i] = typeAnnotationToType(position, c)
        end

        return unionType(components)

    elseif annotation.Kind == "Intersection" then
        local components = table.create<<Type>>(#annotation.Components)

        for i, c in annotation.Components do
            components[i] = typeAnnotationToType(position, c)
        end

        return intersectionType(components)
    else
        unreachable(annotation.Kind)
        error(`Unreachable.`)
    end
end

local function reportMismatch(a: Type, na: NormalizedType, b: Type, nb: NormalizedType, position: caw.Position, reason: string)
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

    err ..= `. { reason }`

    print(err)
end

function instantiateType(ty: Type): Type
    local inited: Type

    if ty.Kind == "error" or ty.Kind == "never" or ty.Kind == "unknown"
        or ty.Kind == "Primitive" or ty.Kind == "Singleton" or ty.Kind == "Generic"
        or ty.Kind == "Free" or ty.Kind == "Function"
    then
        inited = ty
    elseif ty.Kind == "GenericInstantiation" then
        if ty.Name == "issubtypeof" then
            if #ty.Arguments ~= 2 then
                inited = errorReport(`Expected exactly 2 type arguments to 'issubtypeof' type, got { #ty.Arguments } instead.`, { Line = 1, Character = 1, Offset = 1 })
            else
                local a = ty.Arguments[1]
                local b = ty.Arguments[2]

                local na = normalize(a)
                local nb = normalize(b)

                local result = isSubtypeOf(na, nb)

                -- if result.Holds then
                --     print(`'{ a.AliasName }' is a subtype of '{ b.AliasName }'.`)
                -- else
                --     print(`'{ a.AliasName }' is not a subtype of '{ b.AliasName }': { result.FailedPath }`)
                -- end

                inited = singletonType(result.Holds)
            end
        else
            inited = errorReport(`Tried to instantiate invalid type alias '{ ty.Name }'.`, { Line = 1, Character = 1, Offset = 1 })
        end
    elseif ty.Kind == "Negation" then
        return makeType{
            Kind = "Negation",
            Negated = instantiateType(ty.Negated),
        }

    elseif ty.Kind == "Union" then
        local components = table.create<<Type>>(#ty.Components)
        local changed = false

        for i, c in ty.Components do
            local new = instantiateType(c)
            components[i] = new

            if new ~= c then
                changed = true
            end
        end

        if changed then
            inited = unionType(components)
        else
            inited = ty
        end
    elseif ty.Kind == "Intersection" then
        local components = table.create<<Type>>(#ty.Components)
        local changed = false

        for i, c in ty.Components do
            local new = instantiateType(c)
            components[i] = new

            if new ~= c then
                changed = true
            end
        end

        if changed then
            inited = intersectionType(components)
        else
            inited = ty
        end
    else
        unreachable(ty.Kind)
        error(`Unreachable.`)
    end

    inited.AliasName = ty.AliasName
    return inited
end

function check(inferred: Type, annotation: Type, position: caw.Position, printNote: boolean?)
    if annotation.Kind == "Error" then
        return
    end

    local normalizedInference = normalize(instantiateType(inferred))
    local normalizedAnnotation = normalize(instantiateType(annotation))

    local subtypeResult = isSubtypeOf(normalizedInference, normalizedAnnotation)

    if not subtypeResult.Holds then
        reportMismatch(inferred, normalizedInference, annotation, normalizedAnnotation, position, subtypeResult.FailedPath)
    elseif printNote then
        print(`'{ typeToString(inferred) }' is a subtype of '{ typeToString(annotation) }'.`)
    end
end

local function bidisemi(expressions: { read caw.Expression }, options: { read LogInference: boolean })
    -- logInference = options.LogInference

    local state: InferenceState = {
        ReturnType = nil,
        SubtypeConstraints = {},
    }

    local builtins = {}

    for k, _ in typeEnv do
        builtins[k] = true
    end

    for _, expr in expressions do
        infer(state, expr)
    end

    dispatchSubtypeConstraints(state.SubtypeConstraints)

    for _, k in typeAssociationsOrdered do
        local t = assert(typeAssociations[k])
        print(`{ k }: { typeToString(t.Type) }`)
    end

    print("")

    for _, k in typeEnvOrdered do
        if not builtins[k] then
            local a = assert(typeEnv[k])
            print(`type { k } = { typeToString(a, k) }`)
        end
    end
end

return bidisemi
