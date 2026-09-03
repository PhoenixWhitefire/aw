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
type Token =
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

type Expression =
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

local function nextToken(state: { position: number, line: number, program: string }): Token
    local token = ""
    local c

    repeat
        c, state.position = nextCharacter(state.program, state.position)

        if c == "\n" then
            state.line += 1
        end
    until not isWhitespace(c)

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

        return {
            Type = "Literal",
            Literal = literal,
        }
    elseif token == '"' then
        local strlit = ""
        local nextC = ""
        nextC, state.position = nextCharacter(state.program, state.position)

        while nextC ~= '"' do
            if isEof(nextC) then
                return {
                    Type = "InvalidSyntax",
                    Token = '"' .. strlit,
                }
            end

            strlit ..= nextC
            nextC, state.position = nextCharacter(state.program, state.position)
        end

        return {
            Type = "Literal",
            Literal = strlit,
        }
    elseif operators[token :: Operator] then -- TODO luau cast (also not accurate)
        return {
            Type = "Operator",
            Operator = token :: Operator,
        }
    elseif correspond[token] then
        return {
            -- TODO luau
            Type = assert(correspond[token])
        }
    elseif string.match(nextCharacter(token, 1), "%a") or (string.byte(nextCharacter(token, 1)) and string.byte(nextCharacter(token, 1)) > 127) then
        if keywords[token] then
            return {
                Type = "Keyword",
                Keyword = token :: Keyword,
            }
        else
            return {
                Type = "Identifier",
                Identifier = token,
            }
        end
    end

    return {
        Type = "InvalidSyntax",
        Token = token,
    }
end

local parseExpression

local function parseBinaryOperation(state: { position: number, line: number, program: string }, report: (string) -> (), left: Expression, opTk: OperatorToken): Expression
    return {
        Type = "BinaryOperation",
        Operator = opTk.Operator,
        Left = left,
        Right = parseExpression(state, report),
    }
end

function parseExpression(state: { position: number, line: number, program: string }, report: (string) -> (), shallow: boolean?): Expression
    local token = nextToken(state)

    if token.Type == "InvalidSyntax" then
        report(`Invalid syntax '{ token.Token }'.`)

        return {
            Type = "InvalidSyntax",
            Token = token.Token,
        }
    elseif token.Type == "Keyword" then
        if token.Keyword == "let" then
            local nameTk = nextToken(state)
            local name = "let" -- placeholder name

            if nameTk.Type == "Identifier" then
                name = nameTk.Identifier
            else
                report(`Expected an identifier, got a { nameTk.Type }.`)
            end

            local equalsTk = nextToken(state)

            if equalsTk.Type ~= "Equals" then
                report(`Expected an '=' to follow declaration identifier, got a { equalsTk.Type }`)
            end

            local expr = parseExpression(state, report)

            return {
                Type = "Declaration",
                Declaration = {
                    Name = name,
                    Value = expr,
                },
            }
        elseif token.Keyword == "nil" then
            return {
                Type = "Constant",
                Constant = { Type = "nil" },
            }
        else
            unreachable(token.Keyword)
            error(`Unreachable`)
        end
    elseif token.Type == "Identifier" then
        local reference: Expression = {
            Type = "Reference",
            Reference = token.Identifier,
        }

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

            return {
                Type = "Call",
                Call = {
                    Identifier = token.Identifier,
                    Arguments = args,
                },
            }
        elseif not shallow and nextTk.Type == "Operator" then
            state.position = callParseState.position
            state.line = callParseState.line

            return parseBinaryOperation(state, report, reference, nextTk)
        else
            return reference
        end
    elseif token.Type == "Literal" then
        local constant: Expression = {
            Type = "Constant",
            Constant = {
                Type = typeof(token.Literal) :: any,
                Value = token.Literal :: any,
            },
        }

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

            local negation = {
                Type = "UnaryOperation",
                Operator = "-",
                Operand = inner,
            }

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
            report(`Unexpected '{ token.Operator }'`)

            return {
                Type = "InvalidSyntax",
                Token = token.Operator,
            }
        end
    else
        report(`Unexpected { token.Type }.`)

        return {
            Type = "InvalidSyntax",
            Token = token.Type,
        }
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

local function parse(program: string): { Expression }?
    local state = {
        position = 1,
        line = 1,
        program = program,
    }

    -- TODO luau extraneous annotation
    local exprs: { Expression } = {}
    local compFailed = false
    local tk = nextToken(table.clone(state))

    while tk.Type ~= "EndOfFile" do
        local expr = parseExpression(state, function(s) error(s) end)
        table.insert(exprs, expr)
        tk = nextToken(table.clone(state))
    end

    print(prettyPrint(exprs))
    print("\n-->\n")

    return exprs
end

local vars = {}
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

function eval(expr: Expression): Value
    assert(expr.Type ~= "InvalidSyntax")

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
            error(`Unknown function '{ expr.Call.Identifier }'`)
        end
    elseif expr.Type == "Constant" then
        return expr.Constant
    elseif expr.Type == "Reference" then
        return vars[expr.Reference]
    elseif expr.Type == "BinaryOperation" then
        local leftEval = eval(expr.Left)
        local rightEval = eval(expr.Right)

        if leftEval.Type ~= "number" then
            error(`Expected LHS of '{ expr.Operator }' operation to be a number, got a { leftEval.Type } instead.`)
        end

        if rightEval.Type ~= "number" then
            error(`Expected RHS of '{ expr.Operator }' operation to be a number, got a { leftEval.Type } instead.`)
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
            error(`Cannot negate a { operandEval.Type }.`)
        end

        return { Type = "number", Value = -operandEval.Value }
    else
        unreachable(expr.Type)
        error(`Unreachable.`)
    end
end

local function run(exprs: { Expression })
    for _, expr in exprs do
        eval(expr)
    end
end

local program = [[

let x = 5
print(x + 1, "hello")
print(-(4))
]]

local ast = parse(program)

if ast then
    run(ast)
end
