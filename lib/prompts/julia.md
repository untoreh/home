# ask
The context is: The Julia programming language and you answer like a Julia AI assistant. Disregard ambiguities.  If I reply with "false" you assume that your most recent answer is wrong and provide an alternative, taking into account what was wrong in your previous answers. When you reply, go straight to the answers being as short (terse!) as possible. Now answer all following questions. Stand by for more questions.


# docs
Return the current file with added documentation comments where there are definitions for constants, functions and modules. All code should remain the same except for the doc strings. Every doc string must be max 50 words long. Use existing documentation comments as examples. Don't alter anything that is not a documentation comment. You can alter existing documentation comments to make them better, but only if they are in the current file. Use the julia syntax for inserting doc strings. Julia doc strings start with `@doc"""...`

# codeium docs
## function
Write a julia documentation comment for the definition requested. Use at most 50 words. Provide an example only if necessary. The format to use is:

```

@doc """ [A short (header) description]

$(TYPEDSIGNATURES)

[A more detailed description]

"""

```

Only replace the square bracket place holders. Only include the detailed description if it is useful and doesn't repeat the short description. Rules inside the documentation block:
- leave the text `$(TYPEDSIGNATURES)` unchanged unless the definition is a type, in which case replace `$(TYPEDSIGNATURES)` with `$(FIELDS)`. If it is an abstract type, use only the short description (in one line).
- DO NOT remove and white space. 
- DO NOT repeat yourself. 
- DO NOT include the detailed description if it says the same thing as the header description. 
- Don't comment about logging unless the function is itself a logging function. 
- If the definition is a const use a one line documentation with format `@doc "[A short description]"`.
- The header description should be concise and on point.
- The detailed description (if present) has to provide reference gotchas, use cases, and examples if deemed important. It has to explain (shortly) NOT what the function does but HOW it does it.
- The detailed description CAN'T mention (under any circumstances) internal functions.
- Keep entries like "NOTE:","FIX:","HACK:","TODO:" or similar
- Don't start the description with "This function" or similar, the subject is obvious.
- In the detailed description every sentence (ending with a dot) is separated by a new line.
- Any symbol (function name, variable, literal, keyword, etc) inside the documentation should be wrapped in backticks, like "`true`"

The output should only contain the documentation block and nothing else and wrapped in markdown quotes.
Wait for requests.

## types
Comment the julia type requested with a short description, use the format
```
@doc """ [A short description]

$(FIELDS)

[A detailed description]
"""
<type>
```
Only use the detailed description for larg structs. Use max 50 words in the documentation comment. If using the detailed description summarize what the function does, do not explain every step. DO NOT remove `$(FIELDS)` and white space. DO NOT repeat yourself. Return the result markdown quoted. The requested type is:

## consts
Answer with a one line documentation comment for the following julia `const` definitions. Only return the documentation comment and nothing else. Don't wrap the result in markdown quotes. A julia doc comment starts with @doc. A one line comment is of the form 
```
@doc "[...]" 
<code>
```

## edit
Return the content of the current file edited with the rules given below. All code should remain the same in the output except when it would be modified by the editing rules given. The current code is julia, and it should conform to the syntax of the julia language. If a rule is unclear, and it is unclear if some code should be removed or kept, prefer ignoring the rule and keeping the code unmodified. All the rules operate within the context of the current file. All the rules assume that the returned output will contain the file content modified by all the rules. RULES:

# Bing
## Docs
### Function
Comment the following function, output only the documentation comment and nothing else. DO NOT wrap the output in markdown quotes. Use the format:
```
@doc """ [A short description].

$(TYPEDSIGNATURES)

[A detailed description (optional)]
"""
```
Only use the detailed description for long functions. Use max 50 words in the documentation comment. If using the detailed description summarize what the function does, do not explain every step, don't comment about logging unless the function is itself a logging function. Don't return the result markdown quoted.

------------
### Consts
Insert a one line documentation comment for the following julia definition. Only return the edited code and nothing else. Don't wrap the result in markdown quotes. A julia doc comment starts with @doc. A one line comment is of the form 
```
@doc "[...]" 
<code>
```

--------------
### Types
Comment the following julia type with a short description, use the format
```
@doc """ [A short description]

$(FIELDS)

[A detailed description]
"""
<type>
```
Only use the detailed description for larg structs. Use max 50 words in the documentation comment. If using the detailed description summarize what the function does, do not explain every step. Don't return the result markdown quoted.

# Phind
# file
The answer must only contain the code with the updated documentation for all definitions except import statements (import statements should be kept as is). When using function or variable names inside the documentation strings ensure to enclose them in escaped '`' ticks. Use the proper enclosing characthers to insert documentation according to the syntax rules of the language. In the documentation, don't spend too many words on small functions, only provide examples for long and complex functions. Use at most 50 words. Provide an example only if necessary. The format to use is:
```
@doc """ [A short (header) description]

$(TYPEDSIGNATURES)

[A more detailed description]

"""
```
Leave `$(TYPEDSIGNATURES)` unchanged unless the definition is a type, in which case replace it with `$(FIELDS)`. DON'T leave out source code. If a function already has a documentation comment, improve it. If a function doesn't have a documentation comment, add it. Apart from documentation modifications, do not modify the source code. Don't remove comments starting with '#'.
If the definition is for a const, the format is: `@doc "[A one line description]"` of max 100 chars length.
