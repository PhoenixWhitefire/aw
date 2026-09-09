-- caw.luau, detached fron aw.luau on 08/09/2026
-- Parser and interpreter.
local caw = {}

export type Value =
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

export type LiteralValue =
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
            read Name: string,
            read Value: Expression,
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

export type Expression = ExpressionData & { Position: Position }

export type ParseResult = { read Success: boolean, read Expressions: { read Expression } }

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

        if next == "=" or (next == "-" and c == "-") then
            token = c .. next
            state.Position.Offset = pos
        else
            token = c
        end

        if token == "--" then
            repeat
                next, pos = nextCharacter(state.Program, state.Position.Offset)
                state.Position.Offset = pos
            until next == "\n" or isEof(next)

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

local function getOperatorPrecedence(operatorTk: OperatorToken)
    local precedences: { [string]: number? } = {
        ["-"] = 1,
        ["*"] = 2,
        ["/"] = 2,
        ["//"] = 2,
    }

    return precedences[operatorTk.Operator] or 0
end

local function parseBinaryOperation(state: ParseState, report: Report, left: Expression, opTk: OperatorToken, prevPrecedence: number?): Expression
    if opTk.Operator == "=" and left.Type ~= "Reference" then
        report(`Mutation operator expects LHS to be a reference, got a { left.Type } instead.`, left)
    end

    local precedence = getOperatorPrecedence(opTk)

    local op = ex({
        Type = "BinaryOperation",
        Operator = opTk.Operator,
        Left = left,
        Right = parseExpression(state, report, prevPrecedence) or invalidExpr(state, `Got end-of-file trying to parse '{ opTk.Operator }' expression.`, report),
    }, left)

    local maybeOperator, consumeOperator = peekNextToken(state)

    if maybeOperator.Type == "Operator" then
        consumeOperator()

        if getOperatorPrecedence(maybeOperator) < (prevPrecedence or 0) then
            return parseBinaryOperation(state, report, op, maybeOperator)
        end
    end

    return op
end

local function maybeCombineWithBinaryOperation(state: ParseState, report: Report, left: Expression, precedence: number?)
    if precedence == math.huge then
        return left
    end

    local maybeOperator, consumeOperator = peekNextToken(state)

    if maybeOperator.Type == "Operator" then
        consumeOperator()
        return parseBinaryOperation(state, report, left, maybeOperator, precedence)
    else
        return left
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

function parseExpression(state: ParseState, report: Report, precedence: number?): Expression?
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

        return maybeCombineWithBinaryOperation(state, report, left, precedence)
    elseif token.Type == "Literal" then
        -- TODO luau does not report error when `position` is not attached even though the annotation is explicit
        local constant: Expression = ex({
            Type = "Constant",
            Constant = token.Literal,
        }, token)

        return maybeCombineWithBinaryOperation(state, report, constant, precedence)
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
                inner = parseExpression(state, report, math.huge) or invalidExpr(state, `Got end-of-file parsing '{ token.Operator }' expression.`, report)
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
            }, token), precedence)
        else
            return ex({
                Type = "VariadicArguments",
            }, token)
        end
    elseif token.Type == "OpenParentheses" then
        local inner = parseExpression(state, report)
        local parentheses = nextToken(state)
        assert(parentheses.Type == "CloseParentheses", `Expected closing parentheses, got { parentheses.Type }`)

        return inner

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

function caw.parse(program: string): ParseResult
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

function caw.valueToString(v: Value)
    if v.Type == "Function" then
        return "function"
    elseif v.Type == "List" then
        local items = table.create<<string>>(#v.Value)

        for i, v in v.Value do
            items[i] = caw.valueToString(v)
        end

        return `\{ { table.concat(items, ", ") } \}`
    elseif v.Type == "Tuple" then
        local items = table.create<<string>>(#v.Value)

        for i, v in v.Value do
            items[i] = caw.valueToString(v)
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
            str ..= caw.valueToString(a) .. " "
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

        return stringValue(caw.valueToString(args[1]))
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
        assert(#args == 3, `'List_clear' requires 3 arguments.`)

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

        if env[name] then
            fail(`A binding with the name '{ name }' already exists.`)
        end

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

function caw.run(exprs: { read Expression }): Value?
    callFrames = { { VariadicArguments = {} } }

    local success, data = pcall(runUnprotected, exprs)

    table.remove(callFrames)

    if success then
        return data
    else
        local msg = tostring(data)
        local _, p = string.find(msg, ".luau:%d+:")

        while p do
            msg = string.sub(msg, p + 2, #msg)
            _, p = string.find(msg, ".luau:%d+:")
        end

        print(`Protected error: { msg }`)
        return nil
    end
end

return table.freeze(caw)
