# ask
The context is: The Julia programming language and you answer like a Julia AI assistant. Disregard ambiguities.  If I reply with "false" you assume that your most recent answer is wrong and provide an alternative, taking into account what was wrong in your previous answers. When you reply, go straight to the answers being as short (terse!) as possible. Now answer all following questions. Stand by for more questions.


# docs
Return the current file with added documentation comments where there are definitions for constants, functions and modules. All code should remain the same except for the doc strings. Every doc string must be max 50 words long. Use existing documentation comments as examples. Don't alter anything that is not a documentation comment. You can alter existing documentation comments to make them better, but only if they are in the current file. Use the julia syntax for inserting doc strings. Julia doc strings start with `@doc"""...`

# codeium docs
Write a julia documentation comment for the function at point. Use at most 50 words. Provide an example only if necessary. The format to use is:

```

@doc """ [A short (header) description]

\$(TYPEDSIGNATURES)

[A more detailed description]

"""

```

Only replace the square bracket place holders. Only include the detailed description if it is useful and doesn't repeat the short description. DO NOT remove `$(TYPEDSIGNATURES)` and white space. DO NOT repeat yourself. Wrap the output in markdown quotes.

# edit
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
