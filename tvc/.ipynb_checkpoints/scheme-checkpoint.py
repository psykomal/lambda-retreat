"""Interpreter for a small subset of Scheme.
"""

def add(exp):
    return sum(exp)

def sub(exp):
    return exp[0] - exp[1]

def multiply(exp):
    return exp[0] * exp[1]

def divide(exp):
    return exp[0] / exp[1]

def mod(exp):
    return exp[0] % exp[1]

def gt(exp):
    return exp[0] > exp[1]

def lt(exp):
    return exp[0] < exp[1]

def eq(exp):
    return exp[0] == exp[1]


def tokenize(code):
    return code.replace("(", "( ").replace(")", " )").split()

def parse(code):
    def parse_node():
        nonlocal tokens
        current = tokens[0]
        if current == "(":
            tokens = tokens[1:]
            result = []
            while tokens[0] != ")":
                result.append(parse_node())
            tokens = tokens[1:]
            return result
        elif current.isdigit():
            tokens = tokens[1:]
            return int(current)
        else:
            tokens = tokens[1:]
            return current

    tokens = tokenize(code)
    return parse_node()

def setup_env():
    env = {}
    # TODO: add any builtins to include
    return env

global_env = setup_env()

def init_eval():
    
    global_env.update({
        "+": add,
        "-": sub,
        "*": multiply,
        "/": divide,
        "%": mod,
        ">": gt,
        "<": lt,
        "eq?": eq,
    })


def is_literal(exp):
    return type(exp)==int

def is_variable(exp):
    return type(exp)==str
    
def is_conditional(exp):
    return len(exp)>1 and exp[0]=="if"

def is_define(exp):
    return len(exp)>1 and exp[0]=="define"

def is_procedure(exp, env):
    return len(exp)>=1 and exp[0] in env
    

def eval_literal(exp):
    return exp

def lookup(exp, env):
    if exp in env:
        return lookup(env[exp], env)
    return exp

def eval_cond(exp, env):
    return eval(exp[2], env) if eval(exp[1], env) else eval(exp[3], env)

def define(exp, env):
    env[exp[1]] = eval(exp[2], env)
    return None

def call_procedure(exp, env):
    
    exp[0] = lookup(exp[0], env)
    
    if str(type(exp[0]))=="<class 'function'>":
        return exp[0](list(map(eval, exp[1:])))
    elif len(exp)==1:
        return exp[0]
    else:
        print("Illegal function")
        return None
    
def eval(exp, env=global_env):
    # print("eval", exp)
    # TODO: fix me
    
    if is_literal(exp):
        return eval_literal(exp)
    elif is_variable(exp):
        return lookup(exp, env)
    elif is_conditional(exp):
        return eval_cond(exp, env)
    elif is_define(exp):
        return define(exp, env)
    elif is_procedure(exp, env):
        return call_procedure(exp, env)
    else:
        print("Looks like a syntax issue")
        return None
    
    return exp


def run(code):
    return eval(parse(code))

def schemestr(val):
    return val

def repl(prompt='> '):
    while True:
        val = eval(parse(input(prompt)))
        if val is not None:
            print(schemestr(val))

def main():
    import sys
    init_eval()
    if len(sys.argv) > 1:
        filename = sys.argv[1]
        code = open(filename).read()
        run(code)
    else:
        repl()

if __name__ == "__main__":
    main()

