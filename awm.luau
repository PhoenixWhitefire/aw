-- awm.luau, detached from aw.luau on 08/09/2026
-- Hindley-Milner style inference and checking.

local caw = require("./caw")
local logInference = false

-- https://github.com/7sharp9/write-you-an-inference-in-fsharp/blob/master/HMBasic/HMBasic.fs

type TypeVariable = {
    read Kind: "Variable",
    Name: string?,
    Instance: Type?,
}

type TypeOperator = {
    read Kind: "Operator",
    read Name: "*error*" | "void" | index<caw.Value, "Type">,
    read Types: { read Type },
}

type Type =
    | TypeVariable
    | TypeOperator

local function unreachable(x: never): never
    error(`Unexpected branch with value '{ x }' ({ typeof(x) }), this is an internal error.`)
end

local function primitiveType(prim): TypeOperator
    return {
        Kind = "Operator",
        Name = prim,
        Types = {},
    }
end

local function voidType()
    return primitiveType("void")
end

local function nilType()
    return primitiveType("nil")
end

local function booleanType()
    return primitiveType("boolean")
end

local function numberType()
    return primitiveType("number")
end

local function stringType()
    return primitiveType("string")
end

local function functionType(params: { read Type }, variadic: Type, returns: Type): TypeOperator
    local types = table.create<<Type>>(#params + 2)

    for i, p in params do
        types[i] = p
    end

    table.insert(types, variadic or voidType())

    if returns then
        table.insert(types, returns)
    else
        table.insert(types, voidType())
    end

    return {
        Kind = "Operator",
        Name = "Function",
        Types = types,
    }
end

local function listType(contains: Type): Type
    return {
        Kind = "Operator",
        Name = "List",
        Types = { contains },
    }
end

local function errorType()
    return primitiveType("*error*")
end

local function typeVariable(): TypeVariable
    return {
        Kind = "Variable",
    }
end

local function exprToStringShallow(expr: caw.Expression)
    if expr.Type == "Declaration" then
        return `let { expr.Declaration.Name } = { exprToStringShallow(expr.Declaration.Value) }`
    elseif expr.Type == "BinaryOperation" then
        return `{ exprToStringShallow(expr.Left) } { expr.Operator } { exprToStringShallow(expr.Right) }`
    elseif expr.Type == "UnaryOperation" then
        return `{ expr.Operator }{ exprToStringShallow(expr.Operand) }`
    elseif expr.Type == "Call" then
        return `{ expr.Call.Identifier }(`
    elseif expr.Type == "Constant" then
        if expr.Constant.Type == "string" then
            return `"{ expr.Constant.Value }"`
        else
            return tostring(expr.Constant.Value)
        end
    elseif expr.Type == "Function" then
        return `function{ if expr.Function.Name then " " .. expr.Function.Name else "" }(`
    elseif expr.Type == "Reference" then
        return expr.Reference
    elseif expr.Type == "IfCondition" then
        return `if { exprToStringShallow(expr.Condition) }`
    elseif expr.Type == "Return" then
        return `return{ if expr.Return then " " .. exprToStringShallow(expr.Return) else "" }`
    elseif expr.Type == "List" then
        if #expr.List >= 1 then
            local first = exprToStringShallow(expr.List[1])

            if #expr.List == 1 then
                return `\{ { first } \}`
            else
                return `\{ { first }`
            end
        else
            return "{}"
        end
    elseif expr.Type == "ReadIndex" then
        return `{ exprToStringShallow(expr.List) }[{ exprToStringShallow(expr.Index) }] }`
    elseif expr.Type == "WriteIndex" then
        return `{ exprToStringShallow(expr.List) }[{ exprToStringShallow(expr.Index) }] } = { exprToStringShallow(expr.Value) }`
    elseif expr.Type == "VariadicArguments" then
        return "..."
    elseif expr.Type == "VariadicSelection" then
        return `...[{ exprToStringShallow(expr.Selection) }]`
    elseif expr.Type == "VariadicLength" then
        return `#...`
    elseif expr.Type == "InvalidSyntax" then
        return expr.Token
    else
        unreachable(expr.Type)
        error(`Unreachable.`)
    end
end

local function errorReport(msg, expr: caw.Expression)
    print(`{ expr.Position.Line }:{ expr.Position.Character } - { exprToStringShallow(expr) }: { msg }`)
    return errorType()
end

local function prune(ty: Type): Type
    if ty.Kind == "Variable" and ty.Instance then
        local newInstance = prune(ty.Instance)
        ty.Instance = newInstance -- TODO luau this assignment makes `newInstance` be inferred optional without the return annotation?

        return newInstance
    else
        return ty
    end
end

local typeVariableNameCount = 0

local function typeToString(ty: Type): string
    local function variableName(tyvar: TypeVariable)
        if tyvar.Name then
            return tyvar.Name
        else
            local name = string.char((typeVariableNameCount + 19) % 26 + 65)

            if typeVariableNameCount > 25 then
                name ..= typeVariableNameCount // 25 + 1
            end

            tyvar.Name = name
            typeVariableNameCount += 1

            return name
        end
    end

    local function toString(ty: Type)
        if ty.Kind == "Variable" then
            if ty.Instance then
                local str = typeToString(ty.Instance)

                if logInference and ty.Instance.Kind == "Variable" then
                    return `'{str}`
                else
                    return str
                end
            else
                return variableName(ty)
            end
        elseif ty.Kind == "Operator" then
            if #ty.Types == 0 then
                return ty.Name
            elseif ty.Name == "Function" then
                local params = table.create<<string>>(#ty.Types)
                local ret = nil

                for i, t in ty.Types do
                    if i == #ty.Types - 1 then
                        if prune(t).Name ~= "void" then
                            params[i] = `...{ typeToString(t) }`
                        end
                    elseif i == #ty.Types then
                        ret = typeToString(t)
                    else
                        params[i] = typeToString(t)
                    end
                end

                assert(ret)
                return `({ table.concat(params, ", ") }) -> { ret }`
            else
                local stringified = table.create<<string>>(#ty.Types)

                for i, t in ty.Types do
                    stringified[i] = typeToString(t)
                end

                return `{ ty.Name }<{ table.concat(stringified, ", ") }>`
            end
        else
            unreachable(ty.Kind)
            error(`Unreachable.`)
        end
    end

    return toString(ty)
end

local occursIn

local function occursInType(v: Type, t2: Type)
    t2 = prune(t2)

    if t2 == v then
        return true
    elseif t2.Kind == "Operator" then
        return occursIn(v, t2.Types)
    else
        return false
    end
end

function occursIn(t: Type, types: { read Type })
    for _, ty in types do
        if occursInType(t, ty) then
            return true
        end
    end

    return false
end

local function isVoid(ty: Type)
    local pr = prune(ty)
    return pr.Kind == "Operator" and pr.Name == "void"
end

local function isGeneric(v: Type, nonGenerics: { [Type]: true? })
    local types = {}

    for ng, _ in nonGenerics do
        table.insert(types, ng)
    end

    return not occursIn(v, types)
end

local function fresh(t: Type, nonGenerics: { [Type]: true? })
    local seen = {}

    local function tp(t: Type): Type
        t = prune(t)

        if t.Kind == "Variable" then
            if isGeneric(t, nonGenerics) then
                if seen[t] then
                    return seen[t]
                else
                    local newVar = typeVariable()
                    seen[t] = newVar

                    return newVar
                end
            else
                return t
            end
        elseif t.Kind == "Operator" then
            local newTypes = table.create<<Type>>(#t.Types)

            for i, ty in t.Types do
                newTypes[i] = tp(ty)
            end

            return { Kind = "Operator", Name = t.Name, Types = newTypes }
        else
            unreachable(t.Kind)
            error(`Unreachable.`)
        end
    end

    return tp(t)
end

local function isErrorSuppressing(ty: Type)
    if ty.Kind ~= "Operator" then
        return false
    end

    if ty.Name == "*error*" then
        return true
    end

    for _, t in ty.Types do
        if isErrorSuppressing(prune(t)) then
            return true
        end
    end

    return false
end

local function deepCopy<T>(tab: T & {}): T
    assert(typeof(tab) == "table")
    local t = table.clone(tab)

    for k, v in t do -- TODO luau, `intersect<>` should always reduce methinks?
        if typeof(v) == "table" then
            t[k] = deepCopy(v)
        else
            t[k] = v
        end
    end

    return t
end

type UnificationStep = {
    read A: Type,
    read B: Type,
    read Source: caw.Expression,
    Succeeded: boolean,
    read Children: { UnificationStep },
}

type InferenceStep = {
    read Source: caw.Expression,
    read Unifications: { UnificationStep },
    read Children: { InferenceStep },
}

local totalUnificationSteps = 0
local inferenceStep: InferenceStep? = nil
local unificationStep: UnificationStep? = nil

local function unify(a: Type, b: Type, expr: caw.Expression, depth: number?): boolean
    depth = depth or 1
    local prevStep = unificationStep

    if logInference then
        totalUnificationSteps += 1

        -- Ensure type variable names are generated.
        typeToString(a)
        typeToString(b)

        local step: UnificationStep = {
            A = deepCopy(a),
            B = deepCopy(b),
            Source = expr,
            Succeeded = true,
            Children = {}
        }

        if unificationStep then
            table.insert(unificationStep.Children, step)
        else
            assert(inferenceStep)
            table.insert(inferenceStep.Unifications, step)
        end

        unificationStep = step
    end

    local success = true

    a = prune(a)
    b = prune(b)

    if a.Kind == "Variable" then
        if a ~= b then
            if occursInType(a, b) then
                a.Instance = errorReport(`Recursive unification, { typeToString(b) } contains { typeToString(a) }.`, expr)
                success = false
            else
                a.Instance = b
            end
        end

    elseif a.Kind == "Operator" and b.Kind == "Variable" then
        success = unify(b, a, expr, depth + 1)

    elseif a.Kind == "Operator" and b.Kind == "Operator" then
        -- `unknown` always unifies
        if a.Name ~= b.Name or #a.Types ~= #b.Types then
            success = false
            local reportMismatch = true

            if a.Name == "Function" and b.Name == "Function" then
                if not isVoid(a.Types[#a.Types - 1]) and #b.Types > #a.Types - 1 and b.Types[#b.Types - 1].Kind == "Variable" then
                    success = true

                    -- `a` has a variadic, unify against `b` arguments
                    for i, at in a.Types do
                        if i == #a.Types - 1 then
                            -- variadic

                            for j = i, #b.Types - 2 do
                                success = success and unify(at, b.Types[j], expr, depth + 1)
                            end
                        elseif i == #a.Types then
                            -- return
                            success = success and unify(at, b.Types[#b.Types], expr, depth + 1)
                        else
                            success = success and unify(at, b.Types[i], expr, depth + 1)
                        end

                        if not success then
                            break
                        end
                    end

                elseif not isVoid(b.Types[#b.Types - 1]) then
                    -- try swapping them
                    success = unify(b, a, expr, depth + 1)
                    reportMismatch = false -- inner expr already reported mismatch
                end
            end

            if not success and reportMismatch then
                if not isErrorSuppressing(a) and not isErrorSuppressing(b) then
                    errorReport(`Mismatching types:`, expr)
                else
                    success = true
                end
            end
        else
            for i, t1 in a.Types do
                if not unify(t1, b.Types[i], expr, depth + 1) then
                    success = false
                    break
                end
            end
        end
    end

    if logInference then
        assert(inferenceStep, "inferenceStep is nil.")
        assert(unificationStep, "unificationStep is nil")

        unificationStep.Succeeded = success
        unificationStep = prevStep
    end

    if not success then
        -- Leaving the unification trail as diagnostics is bad.
        if depth > 1 then
            print(`{ expr.Position.Line }:{ expr.Position.Character }: { typeToString(a) } is not { typeToString(b) }, so`)
        else
            print(`{ expr.Position.Line }:{ expr.Position.Character }: { typeToString(a) } is not { typeToString(b) }.`)
        end
    end

    return success
end

local genericT = typeVariable()

local typeEnv: { [string]: Type? } = {
    -- HMLIMIT: The type of `print`, `(...unknown) -> void`, is inexpressible, because `unknown` would have to be a "top type" that
    -- "unifies" "successfully" against everything, and does not instantiate.
    print = functionType({}, genericT, voidType()),
    typeof = functionType({ genericT }, voidType(), stringType()),
    tostring = functionType({ genericT }, voidType(), stringType()),
    tonumber = functionType({ numberType() }, voidType(), stringType()),

    List_clear = functionType({ listType(genericT), numberType(), genericT }, voidType(), voidType()),
}

local builtin = {}

for bi, _ in typeEnv do
    builtin[bi] = true
end

type InferenceState = {
    nonGeneric: { [Type]: true? },
    expectingRetTy: Type? | false,
    expectingVariadicTy: Type?,
}

local totalInferenceSteps = 0
local rootInferenceSteps: { InferenceStep } = {}

local infer

local function infer_(expr: caw.Expression, state: InferenceState): Type
    if logInference then
        totalInferenceSteps += 1

        local step: InferenceStep = {
            Source = expr,
            Unifications = {},
            Children = {},
        }

        if inferenceStep then
            table.insert(inferenceStep.Children, step)
        end

        inferenceStep = step
    end

    if expr.Type == "Declaration" then
        local ty = infer(expr.Declaration.Value, state)

        if isVoid(ty) then
            ty = errorReport(`Tried to assign '{ expr.Declaration.Name }' to an expression that returns 'void'.`, expr)
        end

        typeEnv[expr.Declaration.Name] = ty
        return ty

    elseif expr.Type == "Constant" then
        if expr.Constant.Type == "nil" then
            return nilType()
        elseif expr.Constant.Type == "boolean" then
            return booleanType()
        elseif expr.Constant.Type == "number" then
            return numberType()
        elseif expr.Constant.Type == "string" then
            return stringType()
        else
            unreachable(expr.Constant.Type)
            error(`Unreachable.`)
        end
    elseif expr.Type == "Reference" then
        local fromEnv = typeEnv[expr.Reference]

        if fromEnv then
            return fresh(fromEnv, state.nonGeneric)
        else
            return errorReport(`Unknown symbol '{ expr.Reference }'.`, expr)
        end
    elseif expr.Type == "BinaryOperation" then
        local left = infer(expr.Left, state)
        local right = infer(expr.Right, state)

        if expr.Operator == "+" or expr.Operator == "-" or expr.Operator == "*"
            or expr.Operator == "/" or expr.Operator == "//"
        then
            unify(left, numberType(), expr.Left)
            unify(right, numberType(), expr.Right)

            return numberType()
        elseif expr.Operator == ">"
            or expr.Operator == ">=" or expr.Operator == "<" or expr.Operator == "<="
        then
            unify(left, numberType(), expr.Left)
            unify(right, numberType(), expr.Right)

            return booleanType()
        elseif expr.Operator == ".." then
            unify(left, stringType(), expr.Left)
            unify(right, stringType(), expr.Right)

            return stringType()
        elseif expr.Operator == "==" or expr.Operator == "~=" then
            return booleanType()
        elseif expr.Operator == "=" then
            if expr.Left.Type == "Reference" then
                local prev = typeEnv[expr.Left.Reference]

                if not prev then
                    return errorReport(`Cannot mutate '{ expr.Left.Reference }' because it was not defined.`, expr)
                end

                local now = infer(expr.Right, state)

                if now.Kind == "Operator" and now.Name == "void" then
                    return errorReport(`Tried to mutate '{ expr.Left.Reference }' to void.`, expr)
                end

                -- TODO luau, soundness bug?
                unify(now, prev, expr.Right)
                return prev
            else
                return errorType() -- Error would've already been reported by parser
            end
        else
            unreachable(expr.Operator)
            error(`Unreachable.`)
        end
    elseif expr.Type == "UnaryOperation" then
        local operand = infer(expr.Operand, state)

        if expr.Operator == "-" then
            unify(operand, numberType(), expr.Operand)

            return numberType()
        elseif expr.Operator == "#" then
            -- HMLIMIT: The constraint on the type would be that it's either `string` or `List<T>`, which HM cannot express.
            return numberType()
        else
            unreachable(expr.Operator)
            error(`Unreachable.`)
        end
    elseif expr.Type == "Return" then
        if state.expectingRetTy == nil then
            return errorReport(`Unexpected 'return' outside of function body.`, expr)
        elseif state.expectingRetTy == false then
            state.expectingRetTy = typeVariable()
        end

        assert(state.expectingRetTy)

        local here = voidType()

        if expr.Return then
            here = infer(expr.Return, state)
        end

        unify(here, state.expectingRetTy, expr)
        return voidType()

    elseif expr.Type == "IfCondition" then
        local condition = infer(expr.Condition, state)
        unify(condition, booleanType(), expr.Condition)

        local result = typeVariable()

        for _, v in expr.IfBody do
            result = infer(v, state)
        end

        if expr.ElseBody then
            local last

            for _, v in expr.ElseBody do
                last = infer(v, state)
            end

            if last then
                unify(last, result, expr.ElseBody[#expr.ElseBody])
            end
        end

        return result

    elseif expr.Type == "Call" then
        local fnTy = typeEnv[expr.Call.Identifier]

        if not fnTy then
            return errorReport(`Unknown function '{ expr.Call.Identifier }'.`, expr)
        end

        fnTy = fresh(fnTy, state.nonGeneric)
        local argTys = table.create<<Type>>(#expr.Call.Arguments)

        for i, a in expr.Call.Arguments do
            argTys[i] = infer(a, state)
        end

        local retTy = typeVariable()

        if not unify(functionType(argTys, typeVariable(), retTy), fnTy, expr) then
            retTy = errorType() -- Do not leak the type variable.
        end

        return retTy

    elseif expr.Type == "Function" then
        local paramTys = table.create<<Type>>(#expr.Function.Parameters + 1)
        local variadic = voidType()

        local shadowed = {}

        for i, p in expr.Function.Parameters do
            local tv = typeVariable()
            paramTys[i] = tv
            state.nonGeneric[tv] = true
            shadowed[p] = typeEnv[p]
            typeEnv[p] = tv
        end

        if expr.Function.Variadic then
            variadic = typeVariable()
            state.nonGeneric[variadic] = true
        end

        local fnTy = functionType(paramTys, variadic, typeVariable())

        if expr.Function.Name then
            shadowed[expr.Function.Name] = typeEnv[expr.Function.Name]
            typeEnv[expr.Function.Name] = fnTy
        end

        local prevVariTy = state.expectingVariadicTy
        state.expectingVariadicTy = variadic

        local prevRetTy = state.expectingRetTy
        state.expectingRetTy = false

        for _, b in expr.Function.Body do
            infer(b, state)
        end

        local retTy = voidType()

        if state.expectingRetTy then
            retTy = state.expectingRetTy
        end

        state.expectingVariadicTy = prevVariTy
        state.expectingRetTy = prevRetTy

        for i, p in paramTys do
            state.nonGeneric[p] = nil

            local param = expr.Function.Parameters[i]
            typeEnv[param] = shadowed[param]
        end

        state.nonGeneric[variadic] = nil

        if expr.Function.Name then
            typeEnv[expr.Function.Name] = shadowed[expr.Function.Name]
        end

        fnTy = functionType(paramTys, variadic, retTy)

        if expr.Function.Name then
            typeEnv[expr.Function.Name] = fnTy
        end

        return fnTy

    elseif expr.Type == "List" then
        local itemTy = typeVariable()

        if #expr.List > 0 then
            local first = infer(expr.List[1], state)

            if #expr.List > 1 then
                for i = 2, #expr.List do
                    local item = expr.List[i]
                    unify(infer(item, state), first, item)
                end
            end

            itemTy = first
        end

        return {
            Kind = "Operator",
            Name = "List",
            Types = { itemTy },
        }

    elseif expr.Type == "ReadIndex" then
        local valueTy = typeVariable()

        local listTy = infer(expr.List, state)
        unify(listTy, listType(valueTy), expr.List)

        local indexTy = infer(expr.Index, state)
        unify(indexTy, numberType(), expr.Index)

        return valueTy

    elseif expr.Type == "WriteIndex" then
        local valueTy = typeVariable()

        local listTy = infer(expr.List, state)
        unify(listTy, listType(valueTy), expr.List)

        local indexTy = infer(expr.Index, state)
        unify(indexTy, numberType(), expr.Index)

        local writeValueTy = infer(expr.Value, state)
        unify(valueTy, writeValueTy, expr.Value)

        return valueTy

    elseif expr.Type == "VariadicArguments" then
        if state.expectingVariadicTy then
            return state.expectingVariadicTy
        else
            return errorReport(`'...' may only be referenced within function bodies.`, expr)
        end

    elseif expr.Type == "VariadicSelection" then
        if state.expectingVariadicTy then
            return state.expectingVariadicTy
        else
            return errorReport(`'...' may only be referenced within function bodies.`, expr)
        end

    elseif expr.Type == "VariadicLength" then
        if state.expectingVariadicTy then
            return numberType()
        else
            return errorReport(`'...' may only be referenced within function bodies.`, expr)
        end

    elseif expr.Type == "InvalidSyntax" then
        return errorType()
    else
        unreachable(expr.Type)
        error(`Unreachable.`)
    end
end

function infer(expr, state)
    local prevStep = inferenceStep

    local result = infer_(expr, state)

    if logInference and prevStep then
        inferenceStep = prevStep
    end

    return result
end

local function writeInferenceLogs()
    print(`\nInference took { totalInferenceSteps } steps with { totalUnificationSteps } unifications.\n`)

    local function logUnificationStep(step: UnificationStep, indent: string)
        print(`{ indent }Unify { exprToStringShallow(step.Source) } ==> { typeToString(step.A) } = { typeToString(step.B) }{ if step.Succeeded then "" else " - Failed" }.`)

        for _, child in step.Children do
            logUnificationStep(child, indent .. "  ")
        end
    end

    local function logInferenceStep(step: InferenceStep, indent: string?)
        indent = indent or ""
        print(`{ indent }Infer { step.Source.Position.Line }:{ step.Source.Position.Character }:{ exprToStringShallow(step.Source) }.`)

        for _, unification in step.Unifications do
            logUnificationStep(unification, indent .. "  ")
        end

        for _, inference in step.Children do
            logInferenceStep(inference, indent .. "  ")
        end
    end

    for _, step in rootInferenceSteps do
        logInferenceStep(step)
    end
end

local function hm(expressions: { read caw.Expression }, options: { read LogInference: boolean })
    print("\n--")

    logInference = options.LogInference

    if logInference then
        table.clear(rootInferenceSteps)
        totalInferenceSteps = 0
        totalUnificationSteps = 0
        inferenceStep = nil
        unificationStep = nil
    end

    for _, v in expressions do
        inferenceStep = nil
        infer(v, { nonGeneric = {} })

        if logInference then
            table.insert(rootInferenceSteps, assert(inferenceStep))
        end
    end

    print("\nInferred types:\n")

    local orderedInferences = {}

    for k, _ in typeEnv do
        table.insert(orderedInferences, k)
    end

    table.sort(orderedInferences)

    for _, k in orderedInferences do
        if not builtin[k] then
            local v = assert(typeEnv[k])

            print(`{ k }: { typeToString(v) }`)
        end
    end

    if logInference then
        writeInferenceLogs()
    end
end

return hm
