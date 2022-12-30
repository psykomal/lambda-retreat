import sys

def parse(expression):
    print(expression)
    exp_list = expression.replace("(", "( ").replace(")", " )").split()
    
    stack = []
    
    for element in exp_list:
        
        if element == "(":
            continue
        elif element == ")":
            y = stack.pop() 
            x = stack.pop()
            A = stack.pop()
            stack.append([A, x, y])
        else:
            element = element if not element.isnumeric() else int(element)
            stack.append(element)
            
    return stack[0]

if __name__ == "__main__":
    exp = str(sys.argv[1])
    print(parse(exp))