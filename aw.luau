-- aw.luau - erin5/@PhoenixWhitefire, 03/09/2026
local caw = require("./caw")

local program = [[

let a_toasting = function(toToast)
    let a_toaster = toToast + 1

    return tostring(toToast)
end

let a_toasted = a_toasting(42)

print(a_toasting, a_toasted) -- HMLIMIT: This is variadic, but there's no way to express that it takes any number of *unique* arguments.

let b_id = function(x)
    return x
end

let b_hello = b_id("hello")

print(b_hello)

let c_wrong = function(wrongAgain)
    let c_q = wrongAgain + 1
    let c_r = wrongAgain .. "World!"
end

function d_fac(n)
    if n > 1 then
        return n * d_fac(n - 1)
    else
        return n
    end
end

print(d_fac(5))

let e_swap = function(lis, in, jn)
    let e_vi = lis[in]
    let e_vj = lis[jn]

    lis[in] = e_vj
    lis[jn] = e_vi

    return e_vj
end

let e_cool_list_full_of_cool_things = { 1, 2, true }
let e_second = e_swap(e_cool_list_full_of_cool_things, 1, 3)
print(e_second)

function f_compose(f, g)
    return function(...)
        return g(f(...))
    end
end

let f_add = function(x)
    return x + 1
end

let f_mul = function(x)
    return x * 2
end

let f_addmul = f_compose(f_add, f_mul) -- HMLIMIT: `f_addmul` is inferred as `(...number) -> number` when it should actually be `(number) -> number`.
let f_result = f_addmul(2)
print(f_result)

]]

local function unreachable(x: never): never
    error(`Unexpected branch with value '{ x }' ({ typeof(x) }), this is an internal error.`)
end

local function prettyPrint(t, indent: string?)
    local initdent = indent or ""
    local s = `\{\n`
    indent = initdent .. "  "

    for k, v in t do
        if typeof(v) == "table" then
            s ..= `{ indent }{ k } = { prettyPrint(v, indent) },\n`
        else
            s ..= `{ indent }{ k } = { v },\n`
        end
    end

    s ..= `{ initdent }}`
    return s
end

local inferers = {
    hm = require("./awm"),
}

local logInference = false
local inference: keyof<typeof(inferers)> = "hm"
local printAst = false

local helpText = [[

aw.luau - erin5/@PhoenixWhitefire.

Arguments:
    --program <A>    - Provide a string to parse, evaluate, and infer.
    --log-inference  - Enable logging for the inference algorithm.
    --inference <A>  - Select the inference algorithm: "hm".
    --ast            - Dump the generated AST of the program.
    --help           - Show this help message and exit.
]]

if select("#", ...) > 0 then
    for i = 1, select("#", ...) do
        local arg = select(i, ...)

        if arg == "--program" then
            local programArg = select(i + 1, ...)

            if programArg then
                program = tostring(programArg)
            else
                print("Missing argument for '--program' command-line option.")

                return false
            end
        elseif arg == "--log-inference" then
            logInference = true

        elseif arg == "--inference" then
            local algoArg = select(i + 1, ...)

            if algoArg then
                local algo = tostring(algoArg)

                if algo == "hm" then
                    inference = "hm" :: "hm" -- TODO luau cast
                else
                    print(`Invalid '--inference' algorithm '{ algo }', please check '--help' for the supported algorithms.`)

                    return false
                end
            else
                print("Missing argument for '--inference' command-line option.")

                return false
            end
        elseif arg == "--ast" then
            printAst = true
        elseif arg == "--help" then
            print(helpText)

            return true
        end
    end
end

local result = caw.parse(program)

if printAst then
    print("AST:\n")
    print(prettyPrint(result.Expressions))
    print("")
end

if result.Success then
    print("Output:")
    local returned = caw.run(result.Expressions)

    if returned then
        print(`= { caw.valueToString(returned) }`)
    end
else
    print("Parse failed.")

    return false
end

if inference == "hm" then
    inferers.hm(result.Expressions, { LogInference = logInference })
else
    unreachable(inference)
    error(`Unreachable.`)
end
