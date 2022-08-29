# import unittest
import metacall

    # Initialize MetaCall
echo metacall_initialize() == 0

# Load a script from a buffer
let tag = "node"
let script = "module.exports = { add: (a, b) => a + b };"
echo metacall_load_from_memory(tag.cstring, script, len(script).csize_t, nil) == 0
# let tag = "py"
# let script = "import os; wow = lambda: print('ok')"
# echo metacall_load_from_memory(tag.cstring, script, len(script).csize_t, nil) == 0
# check(load_from_memory(tag.cstring, script, len(script).csize_t, nil) == 0)

# Initialize call arguments
let args: array[2,pointer] = [
    metacall_value_create_double(3.0),
    metacall_value_create_double(6.0)
]
let name = "add"

# Invoke the function add of NodeJS
# let name = "wow"
# var args: array[0, pointer]
var result = metacall_v_s(name.cstring, args.unsafeAddr, 2)

# Clear arguments
metacall_value_destroy(args[0])
metacall_value_destroy(args[1])

# Check if the result value is correct
let r = metacall_value_to_double(result)
echo r
echo r == 9.0

# Clear result value
metacall_value_destroy(result)

# Destroy MetaCall
echo metacall_destroy() == 0
