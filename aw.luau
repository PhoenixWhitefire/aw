-- aw.luau - erin5/@PhoenixWhitefire, 03/09/2026
--!strict
type Value =
    | {
        Type: "nil",
        Value: nil,
      }
    | {
        Type: "number",
        Value: number,
      }
    | {
        Type: "string",
        Value: string,
      }

type SingleCharacterToken = {
    read Type: "Equals" | "OpenParentheses" | "CloseParentheses" | "Comma" | "EndOfFile",
}

type Keyword = "let" | "nil"

local operators = {
    ["+"] = true,
    ["-"] = true,
    ["*"] = true,
    ["/"] = true,
    ["//"] = true,
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
    | SingleCharacterToken

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
        read Operator: "-",
        read Operand: Expression,
      }
    | {
        read Type: "InvalidSyntax",
        read Token: string,
      }

type Expression = ExpressionData & { position: Position }

local keywords: { read [string]: Keyword? } = {
    let = "let",
    ["nil"] = "nil",
}

-- TODO luau
local correspond: { read [string]: index<SingleCharacterToken, "Type">? } = {
    ["="] = "Equals",
    ["("] = "OpenParentheses",
    [")"] = "CloseParentheses",
    [","] = "Comma",
    [""] = "EndOfFile",
}

local function unreachable(x: never): never
    error(`Got value '{ x }' ({ typeof(x) }).`)
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
    return isEof(c) or isWhitespace(c) or c == ";" or c == "=" or c == "(" or c == ")" or c == "," or c == "^" or c == '"' or operators[c]
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
        token = c
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
            error(`Invalid reference '{ token }'.`)
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
    elseif operators[token :: Operator] then -- TODO luau cast (also not accurate)
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

local parseExpression

local function parseBinaryOperation(state: ParseState, report: (string, Position) -> (), left: Expression, opTk: OperatorToken): Expression
    return ex({
        Type = "BinaryOperation",
        Operator = opTk.Operator,
        Left = left,
        Right = parseExpression(state, report),
    }, left)
end

function parseExpression(state: ParseState, report: (string, Position) -> (), shallow: boolean?): Expression
    local token = nextToken(state)

    if token.Type == "InvalidSyntax" then
        report(`Invalid syntax '{ token.Token }'.`, token.position)

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
                report(`Expected an identifier, got a { nameTk.Type }.`, nameTk.position)

                return ex({
                    Type = "InvalidSyntax",
                    Token = nameTk.Type,
                }, nameTk)
            end

            local equalsTk = nextToken(state)

            if equalsTk.Type ~= "Equals" then
                report(`Expected an '=' to follow declaration identifier, got a { equalsTk.Type }`, equalsTk.position)

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
                    Value = expr,
                },
            }, token)
        elseif token.Keyword == "nil" then
            return ex({
                Type = "Constant",
                Constant = { Type = "nil" },
            }, token)
        else
            unreachable(token.Keyword)
            error(`Unreachable`)
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
                table.insert(args, parseExpression(state, report))
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
        if token.Operator == "-" then
            local inner: Expression

            local parenParseState = table.clone(state)
            local nextTk = nextToken(parenParseState)

            if nextTk.Type == "OpenParentheses" then
                state.position = parenParseState.position
                state.line = parenParseState.line

                inner = parseExpression(state, report)
                state.position += 1 -- skip closing parens
            else
                inner = parseExpression(state, report, true)
            end

            local negation = ex({
                Type = "UnaryOperation",
                Operator = "-",
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
            report(`Unexpected '{ token.Operator }'.`, token.position)

            return ex({
                Type = "InvalidSyntax",
                Token = token.Operator,
            }, token)
        end
    else
        report(`Unexpected { token.Type }.`, token.position)

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
        local expr = parseExpression(state, function(s, l)
            success = false
            print(`{ l.line }:{ l.char }: { s }`)
        end)
        table.insert(exprs, expr)
        tk = nextToken(table.clone(state))
    end

    -- print(prettyPrint(exprs))
    -- print("\n-->\n")

    return { Success = success, Expressions = exprs }
end

local vars: { [string]: Value? } = {}
local lib = {
    print = function(args: { read Value }): Value
        local str = ""

        for _, a in args do
            str ..= tostring(a.Value) .. " "
        end

        str = string.sub(str, 1, #str - 1)
        print(str)

        return { Type = "nil" }
    end,

    typeof = function(args: { read Value }): Value
        assert(#args == 1, `'typeof' requires 1 argument.`)

        return { Type = "string", Value = typeof(args[1].Value) }
    end,
}

local function eval(expr: Expression): Value
    assert(expr.Type ~= "InvalidSyntax")

    local function fail(msg): never
        error(`{ expr.position.line }:{ expr.position.char }: { msg }`)
    end

    if expr.Type == "Declaration" then
        local v = eval(expr.Declaration.Value)
        vars[expr.Declaration.Name] = v

        return v
    elseif expr.Type == "Call" then
        if lib[expr.Call.Identifier] then
            local fn = lib[expr.Call.Identifier]
            local arguments = table.create<<Value>>(#expr.Call.Arguments)

            for i, a in expr.Call.Arguments do
                arguments[i] = eval(a)
            end

            return fn(arguments)
        else
            fail(`Unknown function '{ expr.Call.Identifier }'`)
            error(`Unreachable.`)
        end
    elseif expr.Type == "Constant" then
        return expr.Constant
    elseif expr.Type == "Reference" then
        local v = vars[expr.Reference]

        if v then
            return v
        else
            fail(`Invalid reference '{ expr.Reference }'`)
            error(`Unreachable.`)
        end

    elseif expr.Type == "BinaryOperation" then
        local leftEval = eval(expr.Left)
        local rightEval = eval(expr.Right)

        if leftEval.Type ~= "number" then
            fail(`Expected LHS of '{ expr.Operator }' operation to be a number, got a { leftEval.Type } instead.`)
            error(`Unreachable.`)
        end

        if rightEval.Type ~= "number" then
            fail(`Expected RHS of '{ expr.Operator }' operation to be a number, got a { leftEval.Type } instead.`)
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
            unreachable(expr.Operation)
            error(`Unreachable.`)
        end
    elseif expr.Type == "UnaryOperation" then
        local operandEval = eval(expr.Operand)

        if operandEval.Type ~= "number" then
            fail(`Cannot negate a { operandEval.Type }.`)
            error(`Unreachable.`)
        end

        return { Type = "number", Value = -operandEval.Value }
    else
        unreachable(expr.Type)
        error(`Unreachable.`)
    end
end

local function run(exprs: { Expression })
    local success, err = pcall(function()
        for _, expr in exprs do
            eval(expr)
        end
    end)

    if not success then
        local msg = tostring(err)
        local _, p = string.find(msg, ".luau:%d+:")

        if p then
            msg = string.sub(msg, p + 2, #msg)
        end

        print(msg)
    end
end

local program = [[

print("hello" + "helo")
]]

local result = parse(program)

if result.Success then
    run(result.Expressions)
end
