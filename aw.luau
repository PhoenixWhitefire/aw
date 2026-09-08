-- aw.luau - erin5/@PhoenixWhitefire, 03/09/2026
--!strict
type Value =
    | LiteralValue
    | {
        read Type: "List",
        read Value: { Value },
      }
    | {
        read Type: "Function",
        read Value:
            | {
                read IsLuau: true,
                read Function: ({ Value }) -> Value?,
              }
            | {
                read IsLuau: false,
                read Parameters: { string },
                read Variadic: boolean,
                read Body: { Expression },
              },
      }
    | {
        read Type: "Tuple",
        read Value: { read Value },
      }

type LiteralValue =
    | {
        read Type: "nil",
        read Value: nil,
      }
    | {
        read Type: "boolean",
        read Value: boolean,
      }
    | {
        read Type: "number",
        read Value: number,
      }
    | {
        read Type: "string",
        read Value: string,
      }

type BasicToken = {
    read Type:
        | "OpenParentheses"  | "CloseParentheses"
        | "OpenCurlyBraces"  | "CloseCurlyBraces"
        | "OpenSquareBraces" | "CloseSquareBraces"
        | "Comma"
        | "Ellipsis"
        | "EndOfFile",
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
    ["~="] = true,
    [">"] = true,
    [">="] = true,
    ["<"] = true,
    ["<="] = true,
    ["="] = true,
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
        read Literal: LiteralValue,
      }
    | {
        read Type: "InvalidSyntax",
        read Token: string,
      }
    | OperatorToken
    | BasicToken

type Position = { Line: number, Character: number, Offset: number }
type Token = TokenData & { Position: Position }

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
        read Constant: LiteralValue,
      }
    | {
        read Type: "Reference",
        read Reference: string,
      }
    | {
        read Type: "Call",
        read Call: {
            read Identifier: string,
            read Arguments: { read Expression },
        },
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
        read Type: "ReadIndex",
        read List: Expression,
        read Index: Expression,
      }
    | {
        read Type: "WriteIndex",
        read List: Expression,
        read Index: Expression,
        read Value: Expression,
      }
    | {
        read Type: "Function",
        read Function: {
            read Name: string?,
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
        read Type: "List",
        read List: { read Expression },
      }
    | {
        read Type: "VariadicArguments",
      }
    | {
        read Type: "VariadicSelection",
        read Selection: Expression,
      }
    | {
        read Type: "VariadicLength",
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

type Expression = ExpressionData & { Position: Position }

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
    ["("] = "OpenParentheses",
    [")"] = "CloseParentheses",
    ["{"] = "OpenCurlyBraces",
    ["}"] = "CloseCurlyBraces",
    ["["] = "OpenSquareBraces",
    ["]"] = "CloseSquareBraces",
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
    return isEof(c) or isWhitespace(c) or c == ":" or c == ";" or c == "(" or c == ")" or c == "{" or c == "}" or c == "," or c == "^" or c == '"' or c == "#"
        or c == "[" or c == "]"
        or operators[c]
end

local function nextToken(state: ParseState): Token
    local token = ""
    local c

    repeat
        c, state.Position.Offset = nextCharacter(state.Program, state.Position.Offset)
        state.Position.Character += 1

        if c == "\n" then
            state.Position.Line += 1
            state.Position.Character = 1
        end
    until not isWhitespace(c)

    state.Position.Character += 1
    local start = table.clone(state.Position)

    local function tk(t: TokenData): Token
        local tok: Token = t :: Token
        tok.Position = start

        return tok
    end

    if isSeparator(c) then
        local next, pos = nextCharacter(state.Program, state.Position.Offset)

        if next == "=" or next == "-" then
            token = c .. next
            state.Position.Offset = pos
        else
            token = c
        end

        if token == "--" then
            repeat
                next, pos = nextCharacter(state.Program, state.Position.Offset)
                state.Position.Offset = pos
            until next == "\n" or next == ""

            state.Position.Character = 1
            state.Position.Line += 1
            return nextToken(state)
        end
    else
        local before = state.Position.Offset

        repeat
            token ..= c
            before = state.Position.Offset
            c, state.Position.Offset = nextCharacter(state.Program, state.Position.Offset)
        until isSeparator(c) or (tonumber(token) and not tonumber(c) and c ~= ".")

        state.Position.Offset = before
    end

    if tonumber((nextCharacter(token, 1))) then
        local literal = tonumber(token)

        if not literal then
            error(`Invalid identifier '{ token }'.`)
        end

        return tk{
            Type = "Literal",
            Literal = {
                Type = "number" :: "number", -- TODO luau cast
                Value = literal,
            },
        }
    elseif token == '"' then
        local strlit = ""
        local nextC = ""
        nextC, state.Position.Offset = nextCharacter(state.Program, state.Position.Offset)

        while nextC ~= '"' do
            if isEof(nextC) then
                return tk{
                    Type = "InvalidSyntax",
                    Token = '"' .. strlit,
                }
            end

            strlit ..= nextC
            nextC, state.Position.Offset = nextCharacter(state.Program, state.Position.Offset)
        end

        return tk{
            Type = "Literal",
            Literal = {
                Type = "string" :: "string",
                Value = strlit,
            },
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

local function peekNextToken(state: ParseState): (Token, () -> ())
    local peekState = table.clone(state)
    peekState.Position = table.clone(peekState.Position)

    return nextToken(peekState), function()
        state.Position = peekState.Position
    end
end

type ParseState = {
    Program: string,
    Position: Position,
    FunctionDepth: number,
}

type Report = (string, { Position: Position }) -> ()

local function ex(expr: ExpressionData, t: { Position: Position }): Expression
    local x: Expression = expr :: Expression
    x.Position = table.clone(t.Position)

    return x
end

local function invalidExpr(state, msg, report): Expression
    local withPos = { Position = table.clone(state.Position) }
    report(msg, withPos)

    return ex({
        Type = "InvalidSyntax",
        Token = msg,
    }, withPos)
end

local parseExpression

local function parseBody(state: ParseState, report: Report, stopAtElse: boolean?): ({ Expression }, Token)
    local body: { Expression } = {}
    local endTk, consumeEnd = peekNextToken(state)

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
        endTk, consumeEnd = peekNextToken(state)
    end

    consumeEnd()
    return body, endTk
end

local function parseBinaryOperation(state: ParseState, report: Report, left: Expression, opTk: OperatorToken): Expression
    if opTk.Operator == "=" and left.Type ~= "Reference" then
        report(`Mutation operator expects LHS to be a reference, got a { left.Type } instead.`, left)
    end

    return ex({
        Type = "BinaryOperation",
        Operator = opTk.Operator,
        Left = left,
        Right = parseExpression(state, report) or invalidExpr(state, `Got end-of-file trying to parse '{ opTk.Operator }' expression.`, report),
    }, left)
end

local function maybeCombineWithBinaryOperation(state: ParseState, report: Report, left: Expression, shallow: boolean?)
    if shallow then
        return left
    else
        local maybeOperator, consumeOperator = peekNextToken(state)

        if maybeOperator.Type == "Operator" then
            consumeOperator()
            return parseBinaryOperation(state, report, left, maybeOperator)
        else
            return left
        end
    end
end

local function parseFunction(state: ParseState, report: Report, token: Token): Expression
    local name = nil
    local openParenTk = nextToken(state)

    if openParenTk.Type == "Identifier" then
        name = openParenTk.Identifier
        openParenTk = nextToken(state)
    end

    if openParenTk.Type ~= "OpenParentheses" then
        return invalidExpr(state, `Expected opening parentheses after function prefix, got { openParenTk.Type }.`, report)
    end

    local parameters = {}
    local variadic = false
    local paramTk = nextToken(state)

    while paramTk.Type ~= "CloseParentheses" do
        if paramTk.Type == "Identifier" then
            if table.find(parameters, paramTk.Identifier) then
                report(`Duplicate parameter '{ paramTk.Identifier }'.`, paramTk)
            end

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

    state.FunctionDepth += 1

    local body = parseBody(state, report)

    state.FunctionDepth -= 1

    return ex({
        Type = "Function",
        Function = {
            Name = name,
            Parameters = parameters,
            Variadic = variadic,
            Body = body,
        },
    }, token)
end

function parseExpression(state: ParseState, report: Report, shallow: boolean?): Expression?
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

            if equalsTk.Type ~= "Operator" or equalsTk.Operator ~= "=" then
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
            return parseFunction(state, report, token)
        elseif token.Keyword == "return" then
            if state.FunctionDepth == 0 then
                return invalidExpr(state, "Cannot 'return' outside of functions.", report)
            else
                -- TODO not consuming it?
                local nextTk, _ = peekNextToken(state)
                local expr = if nextTk.Type == "Keyword" and nextTk.Keyword == "end" then nil else parseExpression(state, report)

                return ex({
                    Type = "Return",
                    Return = expr,
                }, token)
            end
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
        local left: Expression
        local maybeParenthesesTk, consumeParentheses = peekNextToken(state)

        if maybeParenthesesTk.Type == "OpenParentheses" then
            consumeParentheses()

            local argC = nextCharacter(state.Program, state.Position.Offset)
            local args = {}

            while argC ~= ")" do
                local arg = parseExpression(state, report)
                if not arg then
                    return invalidExpr(state, `Got end-of-file trying to parse function argument.`, report)
                end

                table.insert(args, arg)
                argC = nextCharacter(state.Program, state.Position.Offset)
            end

            state.Position.Offset += 1 -- skip parens

            left = ex({
                Type = "Call",
                Call = {
                    Identifier = token.Identifier,
                    Arguments = args,
                },
            }, token)
        else
            left = ex({
                Type = "Reference",
                Reference = token.Identifier,
            }, token)
        end

        local maybeSquareBracketTk, consumeSquareBracket = peekNextToken(state)

        if maybeSquareBracketTk.Type == "OpenSquareBraces" then
            consumeSquareBracket()

            local index = parseExpression(state, report) or invalidExpr(state, `Reached end-of-file trying to parse index expression.`, report)
            local closeBrackets = nextToken(state)

            if closeBrackets.Type ~= "CloseSquareBraces" then
                return invalidExpr(state, `Expected closing square brackets, got end-of-file.`, report)
            end

            local maybeEqualsTk, consumeEquals = peekNextToken(state)

            if maybeEqualsTk.Type == "Operator" and maybeEqualsTk.Operator == "=" then
                consumeEquals()

                local value = parseExpression(state, report) or invalidExpr(state, `Reached end-of-file trying to parse index assignment.`, report)
                return ex({
                    Type = "WriteIndex",
                    List = left,
                    Index = index,
                    Value = value,
                }, token)
            else
                left = ex({
                    Type = "ReadIndex",
                    List = left,
                    Index = index,
                }, token)
            end
        end

        return maybeCombineWithBinaryOperation(state, report, left, shallow)
    elseif token.Type == "Literal" then
        -- TODO luau does not report error when `position` is not attached even though the annotation is explicit
        local constant: Expression = ex({
            Type = "Constant",
            Constant = token.Literal,
        }, token)

        return maybeCombineWithBinaryOperation(state, report, constant)
    elseif token.Type == "Comma" then
        -- TODO check
        state.Position.Offset += 1
        return parseExpression(state, report)
    elseif token.Type == "Operator" then
        if token.Operator == "-" or token.Operator == "#" then
            local inner: Expression

            local nextTk, consumeNext = peekNextToken(state)

            if nextTk.Type == "OpenParentheses" then
                consumeNext()

                inner = parseExpression(state, report) or invalidExpr(state, `Got end-of-file parsing '{ token.Operator }' expression after parentheses.`, report)
                state.Position.Offset += 1 -- skip closing parens
                state.Position.Character += 1
            elseif nextTk.Type == "Ellipsis" then
                if token.Operator == "#" then
                    consumeNext()

                    return ex({
                        Type = "VariadicLength",
                    }, token)
                end
            else
                inner = parseExpression(state, report, true) or invalidExpr(state, `Got end-of-file parsing '{ token.Operator }' expression.`, report)
            end

            local negation = ex({
                Type = "UnaryOperation",
                Operator = token.Operator,
                Operand = inner,
            }, token)

            return maybeCombineWithBinaryOperation(state, report, negation)
        else
            report(`Unexpected '{ token.Operator }' operator.`, token)

            return ex({
                Type = "InvalidSyntax",
                Token = token.Operator,
            }, token)
        end
    elseif token.Type == "OpenCurlyBraces" then
        local items: { Expression } = {}
        local itemTk, consumeClosingParentheses = peekNextToken(state)

        while itemTk.Type ~= "CloseCurlyBraces" do
            local item = parseExpression(state, report)

            if item then
                table.insert(items, item)
            else
                return invalidExpr(state, `Reached end-of-file parsing list items.`, report)
            end

            itemTk, consumeClosingParentheses = peekNextToken(state)

            if itemTk.Type ~= "CloseCurlyBraces" then
                if itemTk.Type ~= "Comma" then
                    return invalidExpr(state, `Expected comma or closing parentheses, got { itemTk.Type }.`, report)
                end
            end
        end

        consumeClosingParentheses()

        return ex({
            Type = "List",
            List = items,
        }, token)

    elseif token.Type == "Ellipsis" then
        local selectorTk, consumeSelector = peekNextToken(state)

        if selectorTk.Type == "OpenSquareBraces" then
            consumeSelector()

            local selector = parseExpression(state, report) or invalidExpr(state, `Reached end-of-file parsing variadic argument selector.`, report)
            local closingTk = nextToken(state)

            if closingTk.Type ~= "CloseSquareBraces" then
                report(`Expected closing square brackets for variadic argument selector, got { closingTk.Type } instead.`, closingTk)
            end

            return maybeCombineWithBinaryOperation(state, report, ex({
                Type = "VariadicSelection",
                Selection = selector,
            }, token))
        else
            return ex({
                Type = "VariadicArguments",
            }, token)
        end
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
        Program = program,
        Position = {
            Line = 1,
            Character = 1,
            Offset = 1,
        },
        FunctionDepth = 0,
    }

    -- TODO luau extraneous annotation
    local exprs: { Expression } = {}
    local success = true
    local tk = peekNextToken(state)

    while tk.Type ~= "EndOfFile" do
        local expr = parseExpression(state, function(s, e)
            success = false
            print(`{ e.Position.Line }:{ e.Position.Character }: { s }`)
        end)
        assert(expr)

        table.insert(exprs, expr)
        tk = peekNextToken(state)
    end

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

local function valueToString(v: Value)
    if v.Type == "Function" then
        return "function"
    elseif v.Type == "List" then
        local items = table.create<<string>>(#v.Value)

        for i, v in v.Value do
            items[i] = valueToString(v)
        end

        return `\{ { table.concat(items, ", ") } \}`
    elseif v.Type == "Tuple" then
        local items = table.create<<string>>(#v.Value)

        for i, v in v.Value do
            items[i] = valueToString(v)
        end

        return `\[ { table.concat(items, ", ") } \]`
    else
        return tostring(if v.Type == "Function" then "function" else v.Value)
    end
end

local function nilValue(): Value
    return { Type = "nil" }
end

local function booleanValue(b): Value
    return { Type = "boolean", Value = b }
end

local function numberValue(n): Value
    return { Type = "number", Value = n }
end

local function stringValue(s): Value
    return { Type = "string", Value = s }
end

local env: { [string]: Value? } = {
    print = makeLuauFuncValue(function(args)
        local str = ""

        for _, a in args do
            str ..= valueToString(a) .. " "
        end

        str = string.sub(str, 1, #str - 1)
        print(str)

        return nil
    end),

    typeof = makeLuauFuncValue(function(args)
        assert(#args == 1, `'typeof' requires 1 argument.`)

        return stringValue(args[1].Type)
    end),

    tostring = makeLuauFuncValue(function(args)
        assert(#args == 1, `'tostring' requires 1 argument.`)

        return stringValue(valueToString(args[1]))
    end),

    tonumber = makeLuauFuncValue(function(args)
        assert(#args == 1, `'tonumber' requires 1 argument.`)

        local v = args[1]
        assert(v.Type == "number" or v.Type == "string", `'tonumber' requires a string or number argument, got { v.Type } instead.`)

        local n = tonumber(v.Value)

        if n then
            return numberValue(n)
        else
            return nilValue()
        end
    end),

    List_clear = makeLuauFuncValue(function(args)
        assert(#args == 2, `'List_clear' requires 3 arguments.`)

        local l = args[1]
        assert(l.Type == "List", `'List_clear' expects a List as the first argument.`)

        local n = args[2]
        assert(n.Type == "number", `'List_clear' expects a number as the second argument.`)

        local init = args[3]

        table.clear(l.Value)

        for i = 1, n.Value do
            l.Value[i] = init
        end

        return nil
    end),
}

type CallFrame = {
    read VariadicArguments: { read Value },
}

local callFrames: { CallFrame } = {}
local runUnprotected

local function evaluate(expr: Expression): Value?
    assert(expr.Type ~= "InvalidSyntax")

    local function fail(msg): never
        error(`{ expr.Position.Line }:{ expr.Position.Character }: { msg }`)
    end

    if expr.Type == "Declaration" then
        local name = expr.Declaration.Name

        local v = evaluate(expr.Declaration.Value)
        env[name] = v or nilValue()

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
                if a.Type == "Tuple" then
                    local varargs = callFrames[#callFrames].VariadicArguments

                    for vi, va in varargs do
                        arguments[i + vi - 1] = va
                    end
                else
                    arguments[i] = evaluate(a)
                end
            end

            if fn.Value.IsLuau then
                table.insert(callFrames, { VariadicArguments = {} })

                local returned = fn.Value.Function(arguments)

                table.remove(callFrames)
                return returned
            else
                for i, p in fn.Value.Parameters do
                    env[p] = arguments[i]
                end

                local variadicArgs = {}

                if #fn.Value.Parameters < #expr.Call.Arguments and fn.Value.Variadic then
                    for i = #fn.Value.Parameters + 1, #expr.Call.Arguments do
                        table.insert(variadicArgs, evaluate(expr.Call.Arguments[i]))
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
        local leftEval = evaluate(expr.Left) or nilValue()
        local rightEval = evaluate(expr.Right) or nilValue()

        if expr.Operator == "==" or expr.Operator == "~=" or expr.Operator == ">" or expr.Operator == ">="
            or expr.Operator == "<" or expr.Operator == "<="
        then
            if expr.Operator ~= "==" and expr.Operator ~= "~=" then
                if leftEval.Type ~= "number" then
                    fail(`Expected LHS of '{ expr.Operator }' expression to be a number, got a { leftEval.Type } instead.`)
                    error(`Unreachable.`)
                end

                if rightEval.Type ~= "number" then
                    fail(`Expected RHS of '{ expr.Operator }' expression to be a number, got a { rightEval.Type } instead.`)
                    error(`Unreachable.`)
                end
            end

            local left = leftEval.Value
            local right = rightEval.Value

            if expr.Operator == "==" then
                return booleanValue(left == right)
            elseif expr.Operator == "~=" then
                return booleanValue(left ~= right)
            elseif expr.Operator == ">" then
                assert(typeof(left) == "number" and typeof(right) == "number")
                return booleanValue(left > right)
            elseif expr.Operator == ">=" then
                assert(typeof(left) == "number" and typeof(right) == "number")
                return booleanValue(left >= right)
            elseif expr.Operator == "<" then
                assert(typeof(left) == "number" and typeof(right) == "number")
                return booleanValue(left < right)
            elseif expr.Operator == "<=" then
                assert(typeof(left) == "number" and typeof(right) == "number")
                return booleanValue(left <= right)
            else
                unreachable(expr.Operator)
                error(`Unreachable.`)
            end
        elseif expr.Operator == ".." then
            if leftEval.Type ~= "string" then
                fail(`Expected LHS of '{ expr.Operator }' operation to be a string, got a { leftEval.Type } instead.`)
                error(`Unreachable.`)
            end

            if rightEval.Type ~= "string" then
                fail(`Expected RHS of '{ expr.Operator }' operation to be a string, got a { rightEval.Type } instead.`)
                error(`Unreachable.`)
            end

            return stringValue(leftEval.Value .. rightEval.Value)
        elseif expr.Operator == "=" then
            assert(expr.Left.Type == "Reference", "This should've been caught by the parser.")

            env[expr.Left.Reference] = rightEval
            return rightEval
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
                return numberValue(left + right)
            elseif expr.Operator == "-" then
                return numberValue(left - right)
            elseif expr.Operator == "*" then
                return numberValue(left * right)
            elseif expr.Operator == "/" then
                return numberValue(left / right)
            elseif expr.Operator == "//" then
                return numberValue(left // right)
            else
                unreachable(expr.Operator)
                error(`Unreachable.`)
            end
        end
    elseif expr.Type == "UnaryOperation" then
        local operandEval = evaluate(expr.Operand) or nilValue()

        if expr.Operator == "-" then
            if operandEval.Type ~= "number" then
                fail(`Cannot negate a { operandEval.Type }.`)
                error(`Unreachable.`)
            end

            return numberValue(-operandEval.Value)
        elseif expr.Operator == "#" then
            if operandEval.Type == "string" or operandEval.Type == "List" then
                return numberValue(#operandEval.Value)
            else
                fail(`Cannot get the length of a { operandEval.Type }.`)
                error(`Unreachable.`)
            end
        else
            unreachable(expr.Operator)
            error(`Unreachable.`)
        end
    elseif expr.Type == "Function" then
        -- TODO luau extraneous annotation
        local f: Value = {
            Type = "Function",
            Value = {
                IsLuau = false,
                Parameters = expr.Function.Parameters,
                Variadic = expr.Function.Variadic,
                Body = expr.Function.Body,
            },
        }

        if expr.Function.Name then
            env[expr.Function.Name] = f
        end

        return f
    elseif expr.Type == "Return" then
        assert(#callFrames > 1, "'return' expression appeared outside of a function call frame, this should've failed to parse.")

        error({
            NormalReturn = true,
            NormalReturnValue = if expr.Return then evaluate(expr.Return) else nil,
        })
    elseif expr.Type == "List" then
        local values = table.create<<Value>>(#expr.List)

        for i, v in expr.List do
            values[i] = evaluate(v)
        end

        return {
            Type = "List",
            Value = values,
        }
    elseif expr.Type == "VariadicArguments" then
        return callFrames[#callFrames].VariadicArguments[1] or nilValue()

    elseif expr.Type == "VariadicSelection" then
        local varargs = callFrames[#callFrames].VariadicArguments
        local index = evaluate(expr.Selection) or nilValue()
        assert(index.Type == "number", `Variadic selection was expected to be a number, got { index.Type }.`)

        return varargs[index.Value] or nilValue()

    elseif expr.Type == "VariadicLength" then
        local varargs = callFrames[#callFrames].VariadicArguments
        return numberValue(#varargs)

    elseif expr.Type == "IfCondition" then
        local conditionResult = evaluate(expr.Condition) or nilValue()

        if conditionResult.Type ~= "boolean" then
            fail(`Expected boolean for condition, got { conditionResult.Type }.`)
            error(`Unreachable.`)
        end

        if conditionResult.Value == true then
            conditionResult = runUnprotected(expr.IfBody)
        elseif expr.ElseBody then
            conditionResult = runUnprotected(expr.ElseBody)
        end

        return conditionResult
    elseif expr.Type == "ReadIndex" then
        local list = evaluate(expr.List) or nilValue()
        assert(list.Type == "List", `Expected a list as the indexed value, got { list.Type }.`)

        local index = evaluate(expr.Index) or nilValue()
        assert(index.Type == "number", `Expected a number as the index value, got { index.Type }.`)

        return list.Value[index.Value]
    elseif expr.Type == "WriteIndex" then
        local list = evaluate(expr.List) or nilValue()
        assert(list.Type == "List", `Expected a list as the indexed value, got { list.Type }.`)

        local index = evaluate(expr.Index) or nilValue()
        assert(index.Type == "number", `Expected a number as the index value, got { index.Type }.`)

        local value = evaluate(expr.Value) or nilValue()
        list.Value[index.Value] = value

        return value
    else
        unreachable(expr.Type)
        error(`Unreachable.`)
    end
end

function runUnprotected(exprs: { read Expression }): Value?
    local last = nil

    for _, expr in exprs do
        local result = evaluate(expr)

        if expr.Type == "Return" then
            return result
        end

        last = result
    end

    return last
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
            msg = string.sub(msg, p + 2, #msg)
        end

        print(`Protected error: { msg }`)
        return nil
    end
end

local program = [[

let a_toasting = function(toToast)
    let a_toaster = toToast + 1

    return tostring(toToast)
end

let a_toasted = a_toasting(42)

print(tostring(a_toasting), tostring(a_toasted))

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
    return function(h)
        return g(f(h))
    end
end

let f_add = function(x)
    return x + 1
end

let f_mul = function(x)
    return x * 2
end

let f_addmul = f_compose(f_add, f_mul)
let f_result = f_addmul(2)
print(f_result)

let q = 5
q = "hello"
print(q)

]]

local logInference = false
local printAst = false

if select("#", ...) > 0 then
    for i = 1, select("#", ...) do
        local arg = select(i, ...)

        if arg == "--program" then
            program = select(i + 1, ...)
        elseif arg == "--log-inference" then
            logInference = true
        elseif arg == "--ast" then
            printAst = true
        elseif arg == "--help" then
            print([[

aw.luau - erin5/@PhoenixWhitefire.

Arguments:
    --program <A>    - Provide a string to parse, evaluate, and infer.
    -- log-inference - Enable logging for the inference algorithm.
    --ast            - Dump the generated AST of the program.
    --help           - Show this help message and exit.
            ]])

            return
        end
    end
end

local result = parse(program)

if printAst then
    print("AST:\n")
    print(prettyPrint(result.Expressions))
    print("")
end

if result.Success then
    print("Output:")
    local returned = run(result.Expressions)

    if returned then
        print(`= { valueToString(returned) }`)
    end
else
    print("Parse failed.")
end

-- https://github.com/7sharp9/write-you-an-inference-in-fsharp/blob/master/HMBasic/HMBasic.fs

type TypeVariable = {
    read Kind: "Variable",
    Name: string?,
    Instance: Type?,
}

type TypeOperator = {
    read Kind: "Operator",
    read Name: "*error*" | "void" | index<Value, "Type">,
    read Types: { read Type },
}

type Type =
    | TypeVariable
    | TypeOperator

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

local function exprToStringShallow(expr: Expression)
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

local function errorReport(msg, expr: Expression)
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

local function deepCopy(tab)
    local t = table.clone(tab)

    for k, v in t do
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
    read Source: Expression,
    Succeeded: boolean,
    read Children: { UnificationStep },
}

type InferenceStep = {
    read Source: Expression,
    read Unifications: { UnificationStep },
    read Children: { InferenceStep },
}

local totalUnificationSteps = 0
local inferenceStep: InferenceStep? = nil
local unificationStep: UnificationStep? = nil

local function unify(a: Type, b: Type, expr: Expression, depth: number?): boolean
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
                    errorReport(`Mismatching types.`, expr)
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

print("\n--")

local infer

local function infer_(expr: Expression, state: InferenceState): Type
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

for _, v in result.Expressions do
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
