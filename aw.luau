-- aw.luau - erin5/@PhoenixWhitefire, 03/09/2026
--!strict
type Value =
    | {
        Type: "nil",
        Value: nil,
      }
    | {
        Type: "boolean",
        Value: boolean,
      }
    | {
        Type: "number",
        Value: number,
      }
    | {
        Type: "string",
        Value: string,
      }
    | {
        Type: "Function",
        Value:
            | {
                IsLuau: true,
                Function: ({ Value }) -> Value?,
              }
            | {
                IsLuau: false,
                Parameters: { string },
                Variadic: boolean,
                Body: { Expression },
              },
      }
    | {
        Type: "VariadicArguments",
        Value: { read Value },
      }

type BasicToken = {
    read Type: "Equals" | "IsEqualTo" | "OpenParentheses" | "CloseParentheses" | "Comma" | "Ellipsis" | "EndOfFile",
}

type Keyword = "nil" | "true" | "false" | "let" | "function" | "return" | "end" | "if" | "else" | "then"

local operators = {
    ["+"] = true,
    ["-"] = true,
    ["*"] = true,
    ["/"] = true,
    ["//"] = true,
    [".."] = true,
    ["=="] = true,
}

type Operator = keyof<typeof(operators)>

type OperatorToken = {
    read Type: "Operator",
    read Operator: Operator,
}

-- TODO luau why do these have to be `read`
type TokenData =
    | {
        read Type: "Keyword",
        read Keyword: Keyword,
      }
    | {
        read Type: "Identifier",
        read Identifier: string,
      }
    | {
        read Type: "Literal",
        read Literal: string | number,
      }
    | {
        read Type: "InvalidSyntax",
        read Token: string,
      }
    | OperatorToken
    | BasicToken

type Position = { line: number, char: number }
type Token = TokenData & { position: Position }

type ExpressionData =
    | {
        read Type: "Declaration",
        read Declaration: {
            Name: string,
            Value: Expression,
        },
      }
    | {
        read Type: "Constant",
        read Constant: Value,
      }
    | {
        read Type: "Call",
        read Call: {
            read Identifier: string,
            read Arguments: { read Expression },
        },
      }
    | {
        read Type: "Reference",
        read Reference: string,
      }
    | {
        read Type: "BinaryOperation",
        read Operator: Operator,
        read Left: Expression,
        read Right: Expression,
      }
    | {
        read Type: "UnaryOperation",
        read Operator: "-" | "#",
        read Operand: Expression,
      }
    | {
        read Type: "Function",
        read Function: {
            read Parameters: { string },
            read Variadic: boolean,
            read Body: { Expression },
        },
      }
    | {
        read Type: "Return",
        read Return: Expression?,
      }
    | {
        read Type: "VariadicArguments",
      }
    | {
        read Type: "IfCondition",
        read Condition: Expression,
        read IfBody: { read Expression },
        read ElseBody: { read Expression }?,
      }
    | {
        read Type: "InvalidSyntax",
        read Token: string,
      }

type Expression = ExpressionData & { position: Position }

local keywords: { read [string]: Keyword? } = {
    ["nil"] = "nil",
    ["true"] = "true",
    ["false"] = "false",
    let = "let",
    ["function"] = "function",
    ["return"] = "return",
    ["if"] = "if",
    ["else"] = "else",
    ["then"] = "then",
    ["end"] = "end",
}

-- TODO luau
local correspond: { read [string]: index<BasicToken, "Type">? } = {
    ["="] = "Equals",
    ["("] = "OpenParentheses",
    [")"] = "CloseParentheses",
    [","] = "Comma",
    ["..."] = "Ellipsis",
    [""] = "EndOfFile",
}

local function unreachable(x: never): never
    error(`Unexpected branch with value '{ x }' ({ typeof(x) }), this is an internal error.`)
end

local function nextCharacter(str: string, position: number): (string, number)
    if position > #str then
        return "", position + 1
    end

    local c = string.sub(str, position, position)
    position += 1

    while not utf8.len(c) or utf8.len(c) == 0 and position < #str do
        c ..= string.sub(str, position, position)
        position += 1
    end

    return c, position
end

local function isWhitespace(c: string)
    return c == " " or c == "\t" or c == "\r" or c == "\n"
end

local function isEof(c: string)
    return c == ""
end

local function isSeparator(c: string)
    return isEof(c) or isWhitespace(c) or c == ";" or c == "=" or c == "(" or c == ")" or c == "," or c == "^" or c == '"' or c == "#" or operators[c]
end

local function nextToken(state: { position: number, line: number, char: number, program: string }): Token
    local token = ""
    local c

    repeat
        c, state.position = nextCharacter(state.program, state.position)

        if c == "\n" then
            state.line += 1
            state.char = 1
        end
    until not isWhitespace(c)

    state.char += 1
    local start = { line = state.line, char = state.char }

    local function tk(t: TokenData): Token
        local tok: Token = t :: Token
        tok.position = start

        return tok
    end

    if isSeparator(c) then
        local next, pos = nextCharacter(state.program, state.position)

        if next == "=" then
            token = c .. next
            state.position = pos
        else
            token = c
        end
    else
        local before = state.position

        repeat
            token ..= c
            before = state.position
            c, state.position = nextCharacter(state.program, state.position)
        until isSeparator(c) or (tonumber(token) and not tonumber(c) and c ~= ".")

        state.position = before
    end

    if tonumber((nextCharacter(token, 1))) then
        local literal = tonumber(token)

        if not literal then
            error(`Invalid identifier '{ token }'.`)
        end

        return tk{
            Type = "Literal",
            Literal = literal,
        }
    elseif token == '"' then
        local strlit = ""
        local nextC = ""
        nextC, state.position = nextCharacter(state.program, state.position)

        while nextC ~= '"' do
            if isEof(nextC) then
                return tk{
                    Type = "InvalidSyntax",
                    Token = '"' .. strlit,
                }
            end

            strlit ..= nextC
            nextC, state.position = nextCharacter(state.program, state.position)
        end

        return tk{
            -- TODO luau cast
            Type = "Literal" :: "Literal",
            Literal = strlit,
        }
    elseif operators[token :: Operator] or token == "#" then -- TODO luau cast (also not accurate)
        return tk{
            Type = "Operator",
            Operator = token :: Operator,
        }
    elseif correspond[token] then
        return tk{
            -- TODO luau
            Type = assert(correspond[token])
        }
    elseif string.match(nextCharacter(token, 1), "%a") or (string.byte(nextCharacter(token, 1)) and string.byte(nextCharacter(token, 1)) > 127) then
        if keywords[token] then
            return tk{
                Type = "Keyword",
                Keyword = token :: Keyword,
            }
        else
            return tk{
                Type = "Identifier",
                Identifier = token,
            }
        end
    end

    return tk{
        Type = "InvalidSyntax",
        Token = token,
    }
end

type ParseState = {
    program: string,
    position: number,
    line: number,
    char: number,
}

local function ex(expr: ExpressionData, t: { position: Position }): Expression
    local x: Expression = expr :: Expression
    x.position = t.position

    return x
end

local function invalidExpr(state, msg, report): Expression
    local withPos = { position = { line = state.line, char = state.char } }
    report(msg, withPos)

    return ex({
        Type = "InvalidSyntax",
        Token = msg,
    }, withPos)
end

local parseExpression

local function parseBody(state: ParseState, report: (string, { position: Position }) -> (), stopAtElse: boolean?): ({ Expression }, Token)
    local body: { Expression } = {}
    local endTkParseState = table.clone(state)
    local endTk = nextToken(endTkParseState)

    while endTk.Type ~= "Keyword" or (endTk.Keyword ~= "end" and (not stopAtElse or endTk.Keyword ~= "else")) do
        if endTk.Type == "EndOfFile" then
            report(`Unexpected end-of-file before function 'end'.`, endTk)
            break
        end

        local expr = parseExpression(state, report)
        if not expr then
            break
        end

        table.insert(body, expr)
        endTkParseState = table.clone(state)
        endTk = nextToken(endTkParseState)
    end

    state.position = endTkParseState.position
    state.line = endTkParseState.line
    state.char = endTkParseState.char
    return body, endTk
end

local function parseBinaryOperation(state: ParseState, report: (string, { position: Position }) -> (), left: Expression, opTk: OperatorToken): Expression
    return ex({
        Type = "BinaryOperation",
        Operator = opTk.Operator,
        Left = left,
        Right = parseExpression(state, report) or invalidExpr(state, `Got end-of-file trying to parse '{ opTk.Operator }' expression.`, report),
    }, left)
end

function parseExpression(state: ParseState, report: (string, { position: Position }) -> (), shallow: boolean?): Expression?
    local token = nextToken(state)

    if token.Type == "InvalidSyntax" then
        report(`Invalid syntax '{ token.Token }'.`, token)

        return ex({
            Type = "InvalidSyntax",
            Token = token.Token,
        }, token)
    elseif token.Type == "Keyword" then
        if token.Keyword == "let" then
            local nameTk = nextToken(state)
            local name = "let" -- placeholder name

            if nameTk.Type == "Identifier" then
                name = nameTk.Identifier
            else
                report(`Expected an identifier, got a { nameTk.Type }.`, nameTk)

                return ex({
                    Type = "InvalidSyntax",
                    Token = nameTk.Type,
                }, nameTk)
            end

            local equalsTk = nextToken(state)

            if equalsTk.Type ~= "Equals" then
                report(`Expected an '=' to follow declaration identifier, got a { equalsTk.Type }.`, equalsTk)

                return ex({
                    Type = "InvalidSyntax",
                    Token = equalsTk.Type,
                }, equalsTk)
            end

            local expr = parseExpression(state, report)

            return ex({
                Type = "Declaration",
                Declaration = {
                    Name = name,
                    Value = expr or invalidExpr(state, `Got end-of-file trying to parse declaration.`, report),
                },
            }, token)
        elseif token.Keyword == "nil" then
            return ex({
                Type = "Constant",
                Constant = { Type = "nil" },
            }, token)
        elseif token.Keyword == "true" or token.Keyword == "false" then
            return ex({
                Type = "Constant",
                Constant = { Type = "boolean", Value = token.Keyword == "true" }
            }, token)
        elseif token.Keyword == "function" then
            local openParenTk = nextToken(state)

            if openParenTk.Type ~= "OpenParentheses" then
                report(`Expected opening parentheses after 'function' keyword, got { openParenTk.Type }.`, openParenTk)

                return ex({
                    Type = "InvalidSyntax",
                    Token = openParenTk.Type,
                }, openParenTk)
            end

            local parameters = {}
            local variadic = false
            local paramTk = nextToken(state)

            while paramTk.Type ~= "CloseParentheses" do
                if paramTk.Type == "Identifier" then
                    table.insert(parameters, paramTk.Identifier)
                    paramTk = nextToken(state)

                    if paramTk.Type == "Comma" then
                        paramTk = nextToken(state)
                    end
                elseif paramTk.Type == "Ellipsis" then
                    variadic = true

                    local closeParen = nextToken(state)

                    if closeParen.Type ~= "CloseParentheses" then
                        return invalidExpr(state, `Expected closing parentheses after ellipsis in function parameter list, got { closeParen.Type } instead.`, report)
                    end

                    break
                else
                    report(`Expected function parameter name, got { paramTk.Type }.`, paramTk)
                    break
                end
            end

            local body = parseBody(state, report)

            return ex({
                Type = "Function",
                Function = {
                    Parameters = parameters,
                    Variadic = variadic,
                    Body = body,
                },
            }, token)
        elseif token.Keyword == "return" then
            local nextTk = nextToken(table.clone(state))
            local expr = if nextTk.Type == "Keyword" and nextTk.Keyword == "end" then nil else parseExpression(state, report)

            return ex({
                Type = "Return",
                Return = expr,
            }, token)
        elseif token.Keyword == "end" then
            report(`Unexpected 'end'.`, token)

            return ex({
                Type = "InvalidSyntax",
                Token = token.Keyword,
            }, token)
        elseif token.Keyword == "if" then
            local condition = parseExpression(state, report) or invalidExpr(state, "Reached end-of-file trying to parse 'if' condition.", report)
            local thenTk = nextToken(state)

            if thenTk.Type ~= "Keyword" or thenTk.Keyword ~= "then" then
                return invalidExpr(state, `Expected 'then' keyword following condition, got { thenTk.Type }.`, report)
            end

            local ifBody, endTk = parseBody(state, report, true)
            local elseBody = nil

            if endTk.Type == "Keyword" and endTk.Keyword == "else" then
                elseBody = parseBody(state, report)
            end

            return ex({
                Type = "IfCondition",
                Condition = condition,
                IfBody = ifBody,
                ElseBody = elseBody,
            }, token)
        elseif token.Keyword == "then" or token.Keyword == "else" then
            return invalidExpr(state, `Unexpected '{ token.Keyword }' keyword.`, report)
        else
            unreachable(token.Keyword)
            error(`Unreachable.`)
        end
    elseif token.Type == "Identifier" then
        local reference: Expression = ex({
            Type = "Reference",
            Reference = token.Identifier,
        }, token)

        local callParseState = table.clone(state)
        local nextTk = nextToken(callParseState)

        if nextTk.Type == "OpenParentheses" then
            state.position = callParseState.position
            state.line = callParseState.line

            local argC = nextCharacter(state.program, state.position)
            local args = {}

            while argC ~= ")" do
                local arg = parseExpression(state, report)
                if not arg then
                    return invalidExpr(state, `Got end-of-file trying to parse function argument.`, report)
                end

                table.insert(args, arg)
                argC = nextCharacter(state.program, state.position)
            end

            state.position += 1 -- skip parens

            return ex({
                Type = "Call",
                Call = {
                    Identifier = token.Identifier,
                    Arguments = args,
                },
            }, token)
        elseif not shallow and nextTk.Type == "Operator" then
            state.position = callParseState.position
            state.line = callParseState.line

            return parseBinaryOperation(state, report, reference, nextTk)
        else
            return reference
        end
    elseif token.Type == "Literal" then
        -- TODO luau does not report error when `position` is not attached even though the annotation is explicit
        local constant: Expression = ex({
            Type = "Constant",
            Constant = {
                Type = typeof(token.Literal) :: any,
                Value = token.Literal :: any,
            },
        }, token)

        local operatorParseState = table.clone(state)
        local nextTk = nextToken(operatorParseState)

        if not shallow and nextTk.Type == "Operator" then
            state.position = operatorParseState.position
            state.line = operatorParseState.line

            return parseBinaryOperation(state, report, constant, nextTk)
        else
            return constant
        end
    elseif token.Type == "Comma" then
        -- TODO check
        state.position += 1
        return parseExpression(state, report)
    elseif token.Type == "Operator" then
        if token.Operator == "-" or token.Operator == "#" then
            local inner: Expression

            local parenParseState = table.clone(state)
            local nextTk = nextToken(parenParseState)

            if nextTk.Type == "OpenParentheses" then
                state.position = parenParseState.position
                state.line = parenParseState.line

                inner = parseExpression(state, report) or invalidExpr(state, `Got end-of-file parsing '{ token.Operator }' expression after parentheses.`, report)
                state.position += 1 -- skip closing parens
            else
                inner = parseExpression(state, report, true) or invalidExpr(state, `Got end-of-file parsing '{ token.Operator }' expression.`, report)
            end

            local negation = ex({
                Type = "UnaryOperation",
                Operator = token.Operator,
                Operand = inner,
            }, token)

            local maybeOpParseState = table.clone(state)
            local maybeOpTk = nextToken(maybeOpParseState)

            if not shallow and maybeOpTk.Type == "Operator" then
                state.position = maybeOpParseState.position
                state.line = maybeOpParseState.line

                return parseBinaryOperation(state, report, negation, maybeOpTk)
            else
                return negation
            end
        else
            report(`Unexpected '{ token.Operator }' operator.`, token)

            return ex({
                Type = "InvalidSyntax",
                Token = token.Operator,
            }, token)
        end
    elseif token.Type == "Ellipsis" then
        return ex({
            Type = "VariadicArguments",
        }, token)
    elseif token.Type == "EndOfFile" then
        return nil
    else
        report(`Unexpected { token.Type }.`, token)

        return ex({
            Type = "InvalidSyntax",
            Token = token.Type,
        }, token)
    end
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

local function parse(program: string): { Success: boolean, Expressions: { Expression } }
    local state: ParseState = {
        program = program,
        position = 1,
        line = 1,
        char = 1,
    }

    -- TODO luau extraneous annotation
    local exprs: { Expression } = {}
    local success = true
    local tk = nextToken(table.clone(state))

    while tk.Type ~= "EndOfFile" do
        local expr = parseExpression(state, function(s, e)
            success = false
            print(`{ e.position.line }:{ e.position.char }: { s }`)
        end)
        assert(expr)

        table.insert(exprs, expr)
        tk = nextToken(table.clone(state))
    end

    -- print(prettyPrint(exprs))
    -- print("\n-->\n")

    return { Success = success, Expressions = exprs }
end

local function makeLuauFuncValue(fn: ({ Value }) -> Value?): Value
    return {
        Type = "Function",
        Value = {
            IsLuau = true,
            Function = fn,
        },
    }
end

local function valToString(v: Value)
    return tostring(if v.Type == "Function" then "function" else v.Value)
end

local env: { [string]: Value? } = {
    print = makeLuauFuncValue(function(args)
        local str = ""

        for _, a in args do
            str ..= valToString(a) .. " "
        end

        str = string.sub(str, 1, #str - 1)
        print(str)

        return nil
    end),
    typeof = makeLuauFuncValue(function(args): Value -- TODO luau extraneous annotation
        assert(#args == 1, `'typeof' requires 1 argument.`)

        return { Type = "string", Value = args[1].Type } -- TODO luau weird type error?
    end),
    tostring = makeLuauFuncValue(function(args)
        assert(#args == 1, `'tostring' requires 1 argument.`)

        return { Type = "string", Value = valToString(args[1]) }
    end),
    tonumber = makeLuauFuncValue(function(args): Value -- TODO luau extraneous annotation
        assert(#args == 1, `'tonumber' requires 1 argument.`)

        local v = args[1]
        assert(v.Type == "number" or v.Type == "string", `'tonumber' requires a string or number argument, got { v.Type } instead`)

        local n = tonumber(v.Value)

        if n then
            return { Type = "number", Value = n }
        else
            return { Type = "nil" }
        end
    end),
}

type CallFrame = {
    read VariadicArguments: { read Value },
}

local callFrames: { CallFrame } = {}

local runUnprotected

local function eval(expr: Expression): Value?
    assert(expr.Type ~= "InvalidSyntax")

    local function fail(msg): never
        error(`{ expr.position.line }:{ expr.position.char }: { msg }`)
    end

    local function nilVal(): Value
        return { Type = "nil" }
    end

    if expr.Type == "Declaration" then
        local name = expr.Declaration.Name

        if env[name] then
            fail(`'{ name }' was already declared.`)
        end

        local v = eval(expr.Declaration.Value)
        env[name] = v

        return v
    elseif expr.Type == "Call" then
        local id = expr.Call.Identifier
        local fn = env[id]

        if fn then
            if fn.Type ~= "Function" then
                fail(`Cannot call a { fn.Type }.`)
                error(`Unreachable.`)
            end

            if not fn.Value.IsLuau and #expr.Call.Arguments < #fn.Value.Parameters then
                fail(`Function expects at least { #fn.Value.Parameters } argument(s) but only got { #expr.Call.Arguments }.`)
                error(`Unreachable.`)
            end

            local arguments = table.create<<Value>>(#expr.Call.Arguments)

            for i, a in expr.Call.Arguments do
                if a.Type == "VariadicArguments" then
                    local varargs = callFrames[#callFrames].VariadicArguments

                    for vi, va in varargs do
                        arguments[i + vi - 1] = va
                    end
                else
                    arguments[i] = eval(a)
                end
            end

            if fn.Value.IsLuau then
                table.insert(callFrames, { VariadicArguments = {} })

                local returned = fn.Value.Function(arguments)

                table.remove(callFrames)
                return returned
            else
                local shadowed = {}

                for i, p in fn.Value.Parameters do
                    shadowed[p] = env[p]
                    env[p] = arguments[i]
                end

                local variadicArgs = {}

                if #fn.Value.Parameters < #expr.Call.Arguments and fn.Value.Variadic then
                    for i = #fn.Value.Parameters + 1, #expr.Call.Arguments do
                        table.insert(variadicArgs, eval(expr.Call.Arguments[i]))
                    end
                end

                table.insert(callFrames, { VariadicArguments = variadicArgs })

                local success, data = pcall(runUnprotected, fn.Value.Body)

                table.remove(callFrames)

                local returned: Value?

                if success then
                    returned = data
                elseif typeof(data) == "table" and (data :: any).NormalReturn == true then
                    returned = (data :: any).NormalReturnValue
                else
                    error(data)
                end

                for p, old in shadowed do
                    env[p] = old
                end

                return returned
            end
        else
            fail(`Unknown function '{ expr.Call.Identifier }'.`)
            error(`Unreachable.`)
        end
    elseif expr.Type == "Constant" then
        return expr.Constant
    elseif expr.Type == "Reference" then
        local v = env[expr.Reference]

        if v then
            return v
        else
            fail(`Unknown reference '{ expr.Reference }'.`)
            error(`Unreachable.`)
        end

    elseif expr.Type == "BinaryOperation" then
        local leftEval = eval(expr.Left) or nilVal()
        local rightEval = eval(expr.Right) or nilVal()

        if expr.Operator == "==" then
            return { Type = "boolean", Value = leftEval.Value == rightEval.Value }
        elseif expr.Operator == ".." then
            if leftEval.Type ~= "string" then
                fail(`Expected LHS of '{ expr.Operator }' operation to be a string, got a { leftEval.Type } instead.`)
                error(`Unreachable.`)
            end

            if rightEval.Type ~= "string" then
                fail(`Expected RHS of '{ expr.Operator }' operation to be a string, got a { rightEval.Type } instead.`)
                error(`Unreachable.`)
            end

            return { Type = "string", Value = leftEval.Value .. rightEval.Value }
        else
            if leftEval.Type ~= "number" then
                fail(`Expected LHS of '{ expr.Operator }' operation to be a number, got a { leftEval.Type } instead.`)
                error(`Unreachable.`)
            end

            if rightEval.Type ~= "number" then
                fail(`Expected RHS of '{ expr.Operator }' operation to be a number, got a { rightEval.Type } instead.`)
                error(`Unreachable.`)
            end

            local left = leftEval.Value
            local right = rightEval.Value

            if expr.Operator == "+" then
                return { Type = "number", Value = left + right }
            elseif expr.Operator == "-" then
                return { Type = "number", Value = left - right }
            elseif expr.Operator == "*" then
                return { Type = "number", Value = left * right }
            elseif expr.Operator == "/" then
                return { Type = "number", Value = left / right }
            elseif expr.Operator == "//" then
                return { Type = "number", Value = left // right }
            else
                unreachable(expr.Operator)
                error(`Unreachable.`)
            end
        end
    elseif expr.Type == "UnaryOperation" then
        local operandEval = eval(expr.Operand) or nilVal()

        if expr.Operator == "-" then
            if operandEval.Type ~= "number" then
                fail(`Cannot negate a { operandEval.Type }.`)
                error(`Unreachable.`)
            end

            return { Type = "number", Value = -operandEval.Value }
        elseif expr.Operator == "#" then
            if operandEval.Type ~= "string" then
                fail(`Cannot get the length of a { operandEval.Type }.`)
                error(`Unreachable.`)
            end

            return { Type = "number", Value = #operandEval.Value }
        else
            unreachable(expr.Operator)
            error(`Unreachable.`)
        end
    elseif expr.Type == "Function" then
        return {
            Type = "Function",
            Value = {
                IsLuau = false,
                Parameters = expr.Function.Parameters,
                Variadic = expr.Function.Variadic,
                Body = expr.Function.Body,
            },
        }
    elseif expr.Type == "Return" then
        error({
            NormalReturn = true,
            NormalReturnValue = if expr.Return then eval(expr.Return) else nil,
        })
    elseif expr.Type == "VariadicArguments" then
        return callFrames[#callFrames].VariadicArguments[1]
    elseif expr.Type == "IfCondition" then
        local conditionResult = eval(expr.Condition) or nilVal()

        if conditionResult.Type ~= "boolean" then
            fail(`Expected boolean for condition, got { conditionResult.Type }`)
            error(`Unreachable.`)
        end

        if conditionResult.Value == true then
            return runUnprotected(expr.IfBody)
        elseif expr.ElseBody then
            return runUnprotected(expr.ElseBody)
        else
            return nilVal()
        end
    else
        unreachable(expr.Type)
        error(`Unreachable.`)
    end
end

function runUnprotected(exprs: { read Expression }): Value?
    for _, expr in exprs do
        local result = eval(expr)

        if expr.Type == "Return" then
            return result
        end
    end

    return nil
end

local function run(exprs: { read Expression }): Value?
    table.insert(callFrames, { VariadicArguments = {} })

    local success, data = pcall(runUnprotected, exprs)

    table.remove(callFrames)

    if success then
        return data
    else
        local msg = tostring(data)
        local _, p = string.find(msg, ".luau:%d+:")

        if p then
            --msg = string.sub(msg, p + 2, #msg)
        end

        print(msg)
        return nil
    end
end

local program = [[

let x = then

]]

local result = parse(program)

if result.Success then
    local returned = run(result.Expressions)

    if returned then
        print(`= { returned.Value }`)
    end
else
    print("Compilation failed.")
end

type Type =
    | {
        Kind: "Primitive",
        Primitive: string,
      }
    | {
        Kind: "Error",
      }

local function typeToString(ty: Type)
    if ty.Kind == "Primitive" then
        return ty.Primitive
    elseif ty.Kind == "Error" then
        return "*error*"
    else
        unreachable(ty.Kind)
        error(`Unreachable.`)
    end
end

local function errorReport(msg): Type
    print(msg)
    return { Kind = "Error" }
end

local typeEnv: { [string]: Type? } = {}

local function infer(expr: Expression): Type
    if expr.Type == "Declaration" then
        local ty = infer(expr.Declaration.Value)
        typeEnv[expr.Declaration.Name] = ty

        return ty
    elseif expr.Type == "Constant" then
        return {
            Kind = "Primitive",
            Primitive = expr.Constant.Type,
        }
    elseif expr.Type == "Reference" then
        return typeEnv[expr.Reference] or errorReport(`Unknown declaration '{ expr.Reference }'`)
    elseif expr.Type == "InvalidSyntax" then
        return {
            Kind = "Error",
        }
    else
        unreachable(expr.Type)
        error(`Unreachable.`)
    end
end

for _, v in result.Expressions do
    infer(v)
end

print("\nType environment:\n")

for k, v in typeEnv do
    print(`{ k }: { typeToString(v) }`)
end
