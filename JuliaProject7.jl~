module RudInt

push!(LOAD_PATH, ".")

using Error
using Lexer
export parse, calc, Num

#
# =================================================
#

abstract OWL

type Num <: OWL
	n::Real
end

type Binop <: OWL
	op::Function
	lhs::OWL
	rhs::OWL
end

type Unop <: OWL
	op::Function
	operand::OWL
end

#
# =================================================
#

function collatz(n::Real)
  return collatz_helper(n, 0)
end

function collatz_helper(n::Real, num_iters::Int)
  if n == 1
    return num_iters
  end
  if mod(n,2)==0
    return collatz_helper(n/2, num_iters+1)
  else
    return collatz_helper(3*n+1, num_iters+1)  
  end
end

#
# =================================================
#

# All operators will be stored in this dicitonary
op_table = Dict([	(:+,+),
			(:-,-),
			(:*,*),
			(:/,/),
			(:mod,mod),
			(:collatz,collatz)
			])

#
# =================================================
#

# This is the 'base case' of the parse tree.
function parse(expr::Real)
	return Num(expr)
end
# When an array is encountered use this
function parse(expr::Array{Any})
	# Check to make sure array is not empty
	if length(expr) < 1
	   	throw(LispError("No parameters"));
	end
	# The operator should be the first argument
	op_symbol = expr[1]
	if haskey(op_table, op_symbol) # Check to make sure this is a valid op.
		if length(expr) == 3 # All Operators have 3 or 2 length => 2 or 1 arguments respectively.
			if op_symbol != :collatz # Only the collatz operator needs 1 argument.
				return Binop(op_table[op_symbol], parse(expr[2]), parse(expr[3]))
			else
				throw(LispError("Too many operands")) # Throws if 'collatz' is called with more than one argument.
			end
		elseif length(expr) == 2
			if op_symbol == :- || op_symbol == :collatz # These are the only two allowed 1-arg symbols
				return Unop(op_table[op_symbol], parse(expr[2]))
			else
				throw(LispError("Not enough operands")) # Throw this if not uniary operator
			end
		else
			throw(LispError("Wrong amount of params!")) # Throw this if not correct arg length
		end
	else
		throw(LispError("Unknown Operation!"))
	end
end
# Catch all other parse types.
function parse(expr::Any)
	throw(LispError("Invalid type $expr"))
end

#
# =================================================
#

# Call number and return
function calc( e::Num )
	return e.n
end
# Apply binomial operator
function calc(e::Binop)
	left = calc(e.lhs)
	right = calc(e.rhs)
	if (e.op == /) && (right == 0)
		throw(LispError("Cannot divide by zero.")) # Make sure no errors
	else
		return e.op(left, right)
	end
end
# Apply uniary operator
function calc(e::Unop)
	unary = calc(e.operand)
	if (e.op == collatz && unary <= 0) # Check to make sure collatz is a valid input.
		throw(LispError("Cannot perform collatz on number $unary"))
	else
		return e.op(calc(e.operand))
	end
end
# Catch all other calc errors
function calc(e::Any)
	throw(LispError("Cannot calculate."))
end

#
# =================================================
#

end # module
