module ExtInt

push!(LOAD_PATH, ".")

using Error
using Lexer
export parse, calc, NumVal, ClosureVal

#
# =================================================
#

abstract OWL
abstract Environment
abstract RetVal

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

type If0 <: OWL
  condition::OWL
  zero_branch::OWL
  nonzero_branch::OWL
end

type Binder <: OWL
  name::Symbol
  binding_expr::OWL
end

type With <: OWL
  binders::Array{Binder}
  body::OWL
end

type Id <: OWL
  name::Symbol
end

type FunDef <: OWL
  formal_parameters::Array{Symbol}
  fun_body::OWL
end

type FunApp <: OWL
  fun_expr::OWL
  arg_exprs::Array{OWL}
end

# add to type hierarchy to better support return values

type NumVal <: RetVal
  n::Real
end

type ClosureVal <: RetVal
  params::Array{Symbol}
  body::OWL
  env::Environment  # this is the environment at definition time
end

# Definitions for environment data structures

type mtEnv <: Environment
end

type SymVal <: OWL
  name::Symbol
  value::RetVal
end

type CEnvironment <: Environment
  symvals::Array{SymVal}
  parent::Environment
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
# Parse things with type Id
function parse( expr::Symbol )
  return Id( expr ) # TODO: Check reservered words
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
			throw(Lisprror("Wrong amount of params!")) # Throw this if not correct arg length
		end
	# handle new operations and ast elements seperately
	elseif op_symbol==:if0
		if length(expr)==4
			test_case=parse(expr[2])
			then_branch=parse(expr[3])
			else_branch=parse(expr[4])
			return If0(test_case, then_branch, else_branch)
		else
		throw(LispError("Invalid \'If0\' syntax"))
	elseif op_symbol == :with
	       	if length(expr) == 3
		   	binding_exprs=expr[2]
			if no_dups(binding_exprs)
				binders=Binder[]
				for i in 1:size(exprs,1)
				      push!(binders, Binder(exprs[i][1], parse(binding_exprs[i][2])))
				end
				body = parse(expr[3])
				return With(binders, body)
			else
				throw(LispError("Duplicate symbols in with statement."))
			end
		else
		   	throw(LispError("Invalid \'with\' syntax"))
		end
	elseif op_symbol == :lambda
	       	if length(expr) == 3
		   	if no_dups(expr[2])
				return FunDef(expr[2],parse[expr[3]))
			else
				throw(LispError("Duplicate symbols in lambda expression"))
			end
	       	else
			throw(LispError("Invalid \'lambda\' syntax"))
		end
	else
		fun_expr=parse(op_symbol)
		arg_exprs=OWL[]
		for i in 2:size(expr,1)
		      	 push!(arg_exprs, parse(expr[i]))
		end
		return FunApp(fun_expr, arg_exprs)
	end
end
# Catch all other parse types.
function parse(expr::Any)
	throw(LispError("Invalid type $expr"))
end
# check for duplicate symbols in with statement
function no_dups( exprs::Array{Any} )
	if length(exprs) == 0
    	   	return true
  	end
  	syms = Any[]
  	if typeof(exprs[1]) == Array{Any,1}
    	   	for i in 1:size(exprs,1)
      		      	 if in( exprs[i][1], syms )
        		    	return false
			 else
				push!( syms, exprs[i][1] )
			 end
		end
		return true
	elseif typeof(exprs[1]) == Symbol
    	       	for i in 1:size(exprs,1)
      		      	 if typeof(exprs[i]) != Symbol
        		    	throw( LispError( "Must use symbols." ) )
      			 end
			 elseif in( exprs[i], syms )
			    	return false
			 else
				push!( syms, exprs[i] )
			 end
		end
		return true
	else
		throw( LispError( "Improper format. Missing parenthesis." ) )
	end
end
# default
function no_dups( exprs::Any )
	throw( LispError( "Improper format. Expected array." ) )
end

#
# =================================================
#

# check for symbol name in environment array
function hasSymVal(symvals::Array{SymVal}, name::Symbol)
	for i in 1:size(symvals,1)
	      	 if symvals[i].name == name
	      	    	 return true
		 end
	end
	return false
end
# get value associated with symbol name
function getValue(symvals::Array{SymVal}, name::Symbol)
	 for i in 1:size(symvals,1)
	       	 if symvals[i].name == name
		    	 return symvals[i].value
		 end
	 end
	 throw(LispError("Should not have gotten here (getValue)."))
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