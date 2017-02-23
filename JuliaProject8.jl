module transInt

push!(LOAD_PATH, ".")

using Error
using Lexer
export parse, calc, NumVal, ClosureVal, analyze

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
type NumVal <: RetVal
     	n::Real
end
type ClosureVal <: RetVal
     	params::Array{Symbol}
	body::OWL
	env::Environment  # this is the environment at definition time
end
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
type Plus <: OWL
	operands::Array{OWL}
end
type And <: OWL
	operands::Array{OWL}
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
reserved_words=[:+,:-,:*,:/,:mod,:collatz,:if0,:with,:lambda]

#
# =================================================
#

# This is the 'base case' of the parse tree.
function parse(expr::Real)
	return Num(expr)
end
# Parse things with type Id
function parse(expr::Symbol)
	if !(expr in reserved_words) # Make sure the word is not reserved
		return Id(expr)
	else
	   	throw(LispError("Symbol \'$expr\' is a reserved word"))
	end
end
# When an array is encountered use this
function parse(expr::Array{Any})
	# Check to make sure array is not empty
	if length(expr) >= 1
	   	# The operator should be the first argument
		op_symbol = expr[1]
		if haskey(op_table, op_symbol) # Check to make sure this is a valid op.
		   	if length(expr) == 3 # All Operators have 3 or 2 length => 2 or 1 arguments respectively.
			   	if op_symbol != :collatz # Only the collatz operator needs 1 argument.
				   	return Binop(op_table[op_symbol], parse(expr[2]), parse(expr[3]))
				else
					throw(LispError("Too many parameters given for \'$op_symbol\'")) # Throws if 'collatz' is called with more than one argument.
				end
			elseif length(expr) == 2
			       	if op_symbol == :- || op_symbol == :collatz # These are the only two allowed 1-arg symbols
				   	return Unop(op_table[op_symbol], parse(expr[2]))
				else
					throw(LispError("Not enough parameters given for \'$op_symbol\'")) # Throw this if not uniary operator
				end



# Added
			elseif op_symbol == :+ && length(expr) != 1
			       ops = OWL[]
			       for i in 2:size(expr,1)
			       	     	push!( ops, parse( expr[i] ) )
			       end
			       return Plus( ops )



			else
				throw(LispError("Wrong amount of parameters given for \'$op_symbol\'")) # Throw this if not correct arg length
			end



# Added
	  	elseif op_symbol == :and
			if length(expr) < 3
				throw( LispError( "Cannot perform and without operands." ) )
			end
			ops = OWL[]
			for i in 2:size(expr,1)
			      	push!( ops, parse( expr[i] ) )
			end
			return And( ops )


			# Handle non-binary/uniary operators and AST elements seperately
		elseif op_symbol==:if0
		        if length(expr)==4 # Verify correct input length
				return If0(parse(expr[2]), parse(expr[3]), parse(expr[4])) # Good Input gives good return
				# In case it isn't clear, 'If0' is a node in the AST and has a type defined for it.
			else
				throw(LispError("Wrong amount of parameters given for \'If0\'"))
			end
		elseif op_symbol == :with
	       	        if length(expr) == 3 # Verify correct argument length
		   	   	binding_exprs=expr[2] # This should be an array of 'pairs'
				if no_dups(binding_exprs) # Check to make sure there's no duplicates in the binders
					binders=Binder[]
					for i in 1:size(binding_exprs,1) # Go through list and bind values
				      	       push!(binders, Binder(binding_exprs[i][1], parse(binding_exprs[i][2]))) # Evaluate/parse all bindings
					end
					return With(binders, parse(expr[3])) # The third element is the actual body
					# We'll handle the 'With' node in the AST in the calc statement
				else
					throw(LispError("Duplicate symbols in \'with\' statement"))
				end
			else
				throw(LispError("Wrong amount of parameters given for \'with\'"))
			end
		elseif op_symbol == :lambda
	       	        if length(expr) == 3 # Verify correct argument length
		   	   	if no_dups(expr[2]) # Make sure the arguments aren't duplicated
					return FunDef(expr[2],parse(expr[3])) # elements: 2- argument list. 3-function body
				else
					throw(LispError("Duplicate symbols in \'lambda\' expression"))
				end
	       		else
				throw(LispError("Wrong amount of parameters given for \'lambda\'"))
			end
		else # Everything else must evaluate to a function
			arg_exprs=OWL[]
			for i in 2:size(expr,1)
		      	      	push!(arg_exprs, parse(expr[i]))
			end
			return FunApp(parse(op_symbol), arg_exprs) # The op symbol isn't a symbol. It's a function expression
		end
	else
		throw(LispError("No parameters given: Empty array."));
	end
end
# Catch all other parse types.
function parse(expr::Any)
	throw(LispError("Invalid type \'$expr\'"))
end
# This is the helper function that checks for duplicate symbols in with and lambda statements
function no_dups(exprs::Array{Any})
	if length(exprs) > 0
  	   	syms = Any[]
		if typeof(exprs[1]) == Array{Any,1} # With nodes should have this


# Added
		   	 if length(exprs[1]) == 0
			    	return true
			 end


    	   		 for i in 1:size(exprs,1)
			       	if typeof(exprs[i][1]) != Symbol
				   	throw(LispError("Must use symbols for binding expressions"))
				elseif in(exprs[i][1], reserved_words) # Check for reserved words
				        throw(LispError("Reserved word \'$exprs[i][1]\' cannot be used in binding expression."))
      		      	 	elseif in(exprs[i][1], syms) # Check for duplicates of partial forward-list
        		    	      	return false # Duplicate encountered
				else
					push!(syms, exprs[i][1]) # Push good symbols
			 	end
			end
			return true
		elseif typeof(exprs[1]) == Symbol # Used in lambda nodes
    	       	        for i in 1:size(exprs,1)
      		      	      	if typeof(exprs[i]) != Symbol # Make sure we're using symbols
        		    	   	throw(LispError("Must use symbols for single-dim argument lists."))
				elseif in(exprs[i], reserved_words) # Check for reserved words
				        throw(LispError("Reserved word \'$exprs[i]\' cannot be used in argument lists."))
				elseif in(exprs[i], syms) # Check for duplicates
			    	        return false # Duplicate encountered
			 	else
					push!(syms, exprs[i]) # All is good in the hood.
			 	end
			end
			return true
		else
			throw(LispError("Improper format. Probably missing parentheses."))
		end
	else
    	   	return true # No need to throw error or even check. Empty lists are okay.
  	end
end
# Default behavior to catch incorrect format. Throw error if it's not a list
function no_dups(exprs::Any)
	throw(LispError("Improper format. Array expected."))
end

#
# =================================================
#

# Calculate the basic OWL/root abstract syntax tree
function calc(ast::OWL)
	 return calc(ast, mtEnv())
end
# Base case is a number or closure which we will return
function calc(owl::Num, env::Environment)
	 return NumVal(owl.n)
end
# Handle binary operations as in the last lab
function calc(owl::Binop, env::Environment) # The environment will just be used on calculating either side.
	 left = calc(owl.lhs, env) 
	 right = calc(owl.rhs, env)
	 if typeof(left) == NumVal && typeof(right) == NumVal # Make sure things can evaluated in the calcs
	    	 if (owl.op == /) && right.n == 0 # Make sure that we can't devide by zero
		    	  throw(LispError("Cannot divide by zero in binary op"))
		 else
			  return NumVal(owl.op(left.n, right.n)) # Return correct calculation
		 end
	 else
		 throw(LispError("Invalid binary op types did not evaluate correctly"))
	 end
end
# Handle bnary operations in a similar fashion to binary operations
function calc(owl::Unop, env::Environment) # As in Binop, the environment is just used to calculate the expression recursively
	 unary = calc(owl.operand, env) # Calculate the expression
	 if typeof(unary) == NumVal # Make sure it evaluated to a value
		 if !(owl.op == collatz && unary.n <= 0) # Check to make sure the collatz function is called with a correct input.
			  return NumVal(owl.op(unary.n))
		 else
			  throw(LispError("Cannot perform collatz on \'$unary\'"))
		 end
	 else
		 throw(LispError("Invalid uninary op types did not evaluate correctly"))
	 end
end
# Handle If0 nodes
function calc(owl::If0, env::Environment) # Pass in the environment as always
	 cond = calc(owl.condition, env)
	 if typeof(cond) == NumVal # Make sure that the condition calculated correctly
	    	 if cond.n == 0
		    	  return calc(owl.zero_branch, env) # This is the 'then' branch
		 else
			  return calc(owl.nonzero_branch, env) # This is the 'else' branch
		 end
	 else
		 throw(LispError("Invalid \'If0\' type with value \'$cond\'."))
	 end
end
# Handle Id/Symbols in grammer
function calc(owl::Id, env::Environment)
	 if env == mtEnv() # Check to see if it's an mpty environment.
	    	 throw(LispError("Could not find symbol \'$owl\'"))
	 elseif hasSymVal(env.symvals, owl.name) # See helper function
	 	 return getValue(env.symvals, owl.name) # Return symbol value.
	 else
		 return calc(owl, env.parent) # I believe I need this for recursion
	 end
end
# Handle With nodes
function calc(owl::With, env::Environment)
	 symvals = SymVal[] # The With node has a bunch of sumbols we need to evaluate.
	 for i in 1:size(owl.binders,1)
	       	 push!(symvals, SymVal(owl.binders[i].name, calc(owl.binders[i].binding_expr, env)))
	 end
	 return calc(owl.body, CEnvironment(symvals, env)) # Extend the environment and pass it on.
end
# Define the FunDef Node and pass of the closure
function calc(owl::FunDef, env::Environment)
	 return ClosureVal(owl.formal_parameters, owl.fun_body, env) # super simple I know.
end
# This is the FunApp node
function calc( owl::FunApp, env::Environment )
	 # The function expression should result in a ClosureVal
	 the_closure = calc(owl.fun_expr, env)
	 if typeof(the_closure) == ClosureVal # Check for correct evaluation
	    	 if length(owl.arg_exprs) == length(the_closure.params) # These two lists should be equal.
	 	    	  # Extend the current environment by binding the actual parameters to the formal parameters
	 	 	  symvals = SymVal[] # Build array
	 	 	  for i in 1:size(owl.arg_exprs,1) # Iterate through array and push calcualted symbols
	       	       	      	   push!(symvals, SymVal(the_closure.params[i], calc(owl.arg_exprs[i], env)))
	 	 	  end
	      	 	  return calc(the_closure.body, CEnvironment(symvals, the_closure.env)) # Calculate new environment and return
		 else
		    	  throw(LispError("Number of parameters and arguments does not match in function call/declaration."))
	 	 end
	 else
		 throw(LispError("Invalid type did not return closure"))
	 end
end
# Default calc case
function calc(owl::Any)
	 throw(LispError("Cannot calculate unknown type"))
end
# This could also happen with environments.
function calc(owl::Any, env::Environment)
	 throw(LispError("Cannot calculate unknown type"))
end
# Helper function to check for symbol name in environment array
function hasSymVal(symvals::Array{SymVal}, name::Symbol)
	for i in 1:size(symvals,1) # Iterate through and find
	       	 if symvals[i].name == name
	       	    	 return true # Found
		 end
	end
	return false
	# return (name in symvals) # this won't work with different types fyi
end
# Helper function to get value associated with symbol name
function getValue(symvals::Array{SymVal}, name::Symbol)
	 for i in 1:size(symvals,1)
	       	 if symvals[i].name == name
		    	 return symvals[i].value
		 end
	 end
	 throw(LispError("Symbol with \'$name\' not found - Good job on getting here though"))
end

#
# =================================================
#

# This is for debugging
#function calc(expr::AbstractString)
#	 return calc(parse(Lexer.lex(expr)))
#end
#
## TODO: check recursive function
#println(calc("(with ( (recur (lambda (x) (
#		    if0 x 0 (recur (- x))
#)) ) ) (recur 5))"))
#
## TODO: Ask ScottyMack:
## - Should argument list in with node check for symbol?
## - Formal vs Actual

#
# =================================================
#

end # module