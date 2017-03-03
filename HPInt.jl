module HPInt

push!(LOAD_PATH, ".")
using Error
using Lexer
using Images
using Cairo
export parse, calc, NumVal, ClosureVal, analyze, MatrixVal

#
# =================================================
#

abstract OWL
abstract Environment
abstract RetVal
type Num <: OWL
	n::Real
end
type NumVal <: RetVal
     	n::Real
end
type Binop <: OWL
	op::Function
	lhs::OWL
	rhs::OWL
end
type Plus <: OWL
	operands::Array{OWL}
end
type Unop <: OWL
	op::Function
	operand::OWL
end
type Id <: OWL
     	name::Symbol
end
type If0 <: OWL
     	condition::OWL
	zero_branch::OWL
	nonzero_branch::OWL
end
type Binder <: OWL
     	name::Id
	binding_expr::OWL
end
type With <: OWL
     	binders::Array{Binder}
	body::OWL
end
type FunDef <: OWL
     	formal_parameters::Array{Id}
	fun_body::OWL
end
type FunApp <: OWL
     	fun_expr::OWL
	arg_exprs::Array{OWL}
end
type ClosureVal <: RetVal
     	params::Array{Id}
	body::OWL
	env::Environment
end
type And <: OWL
	operands::Array{OWL}
end
type mtEnv <: Environment
end
type SymVal <: OWL
     	name::Id
	value::Any
end
type CEnvironment <: Environment
     	symvals::Array{SymVal}
	parent::Environment
end
type SimpleLoad <: OWL
     	location::AbstractString
end
type SimpleSave <: OWL
	matrix::OWL
	location::AbstractString
end
type RenderText <: OWL
     	words::AbstractString
	xpos::OWL
	ypos::OWL
end
type Emboss <: OWL
     	matrix::OWL
end
type DropShadow <: OWL
     	matrix::OWL
end
type InnerShadow <: OWL
     	matrix::OWL
end
type MatrixVal <: RetVal
     	n::Array{Float32,2}
end

#
# =================================================
#

# The collatz primitive
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
# The simple_load primitive
function simple_load(imp_path::AbstractString)
	im= Images.load(img_path);
	tmp=Images.separate(im);
	d=Images.data(tmp);
	d=d[:,:,1];
	d=convert(Array{Float32,2},f);
	return d
end
# The simple_save primitive
function simple_save(output::Array, img_path::AbstractString)
	output[output .> 1.0]=1.0
	output[output .< 0.0]=0.0
	tmpc=convert(Array{UInt32,2},floor(output*255.0))
	tmp_output=tmpc+tmpc*256+tmpc*65536+ 0xff000000
    	c2=CairoImageSurface(transpose(tmp_output), Cairo.FORMAT_ARGB32)
    	write_to_png(c2, img_path)
    	return 42
end
# The render_text primitive
function render_text(text_str::AbstractString, xpos, ypos)
	data = Matrix{UInt32}(256, 256);
	c = CairoImageSurface( data, Cairo.FORMAT_ARGB32);
	cr = CairoContext(c);
	set_source_rgb(cr, 1., 1., 1.);
	rectangle(cr, 0., 0., 256., 256.);
	fill(cr);
	set_source_rgb( cr, 0, 0, 0 );
	select_font_face(cr, "Sans", Cairo.FONT_SLANT_NORMAL, Cairo.FONT_WEIGHT_BOLD);
	set_font_size(cr, 90.0);
	move_to(cr, xpos, ypos);
	show_text(cr, text_str);
	# tmp is an Array{UInt32,2}
	tmp = cr.surface.data;
	# grab just the blue channel, and convert the array to an array of floats
	tmp2 = convert(Array{Float32,2}, tmp & 0x000000ff) / 255.0;
	tmp2 = convert(Array{Float32,2}, tmp2);
	return tmp2
end
# The emboss primitive
function emboss( img::Array )
	f = [ -2. -1. 0. -1. 1. 1. 0. 1. 1. ];
	f = convert(Array{Float32,2}, f);
	es = conv2(f, img);
	es = es[1:256,1:256];
	return es
end
# The drop_shadow primitive
function drop_shadow(img::Array)
	foo = convert( Array{Float32,2}, gaussian2d(5.0,[25,25]) );
	foo = foo / maximum(foo);
	ds = conv2(foo, img);
	ds = ds[13:256+12,13:256+12];
	ds = ds / sum(foo);
	return ds
end
# The inner_shadow primitive
function inner_shadow(img::Array) # Assumes imgage is black-on-white
	 foo = convert(Array{Float32,2}, gaussian2d(5.0,[25,25]));
	 foo = foo / maximum(foo);
	 is = conv2(foo, 1.0-img);
	 is = is[8:251+12,8:251+12];
	 is = is / sum(foo);
	 is = max(is, img);
	 return is
end

#
# =================================================
#

# All operators will be stored in this dicitonary
op_table = Dict([
			(:+,+),
			(:-,-),
			(:*,*),
			(:/,/),
			(:mod,mod),
			(:collatz,collatz),
      			(:min, min),
			(:max, max)
		])
reserved_words=[:+,:-,:*,:/,:mod,:collatz,:min,:max,:if0,:with,:lambda, :and, :simple_load, :simple_save, :render_text, :emboss, :drop_shadow, :inner_shadow] # added new reserved word

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
			elseif length(expr) >= 3 && op_symbol == :+ # Handle new grammer with multiple sums
			        ops = OWL[]
			        for i in 2:size(expr,1)
			       	     	push!(ops, parse(expr[i]))
			        end
			        return Plus(ops)
			else
				throw(LispError("Wrong amount of parameters given for \'$op_symbol\'")) # Throw this if not correct arg length
			end
	  	elseif op_symbol == :and # new grammer rule for 'and'
			if length(expr) >= 3 # Must have more than two parameters
			   	ops = OWL[]
				for i in 2:size(expr,1)
			      	      	push!(ops, parse(expr[i]))
				end
				return And(ops)
			else
				throw(LispError("Cannot perform 'and' operation without 2 or more operands."))
			end
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
					if length(binding_exprs) == 0
					   	# Do nothing
					elseif typeof(binding_exprs[1]) == Array{Any,1}
					       	for i in 1:size(binding_exprs,1) # Go through list and bind values
						      	if length(binding_exprs[i]) == 2
				      	       	       	  	push!(binders, Binder(parse(binding_exprs[i][1]), parse(binding_exprs[i][2]))) # Evaluate/parse all bindings
							else
								throw(LispError("Must have one symbol per one expression"))
							end
						end
					else
						throw(LispError("Incorrect syntax on with statement"))
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
					return FunDef(map(parse, expr[2]),parse(expr[3])) # elements: 2- argument list. 3- function body
				else
					throw(LispError("Duplicate symbols in \'lambda\' expression"))
				end
	       		else
				throw(LispError("Wrong amount of parameters given for \'lambda\'"))
			end
		elseif op_symbol==:simple_load
			if length(expr) == 2 && typeof(expr[2]) == String # Verify correct arguments
			   	return SimpleLoad(expr[2]) # Easy parse I know
			else
				throw(LispError("Invalid syntax for simple_load."))
			end
		elseif op_symbol == :simple_save
		        if length(expr) == 3 && typeof(expr[3]) == String # Verify correct arguments
				return SimpleSave(parse(expr[2]),expr[3]) # Easy parse I know
			else
			   	throw(LispError("Invalid syntax for simple_save"))
			end
		elseif op_symbol == :render_text
		       	if length(expr) == 4 && typeof(expr[2]) != String # Verify correct arguments
			   	return RenderText(expr[2], parse(expr[3]), parse(expr[4])) # Easy parse I know
			else
				throw(LispError("Invalid syntax for render_text."))
			end
		elseif op_symbol == :emboss
		       	if length(expr) == 2 # Verify correct argument length
			   	return Emboss(parse(expr[2])) # Easy parse I know
			else
				throw(LispError("Invalid syntax for emboss."))
			end
		elseif op_symbol == :drop_shadow
		       	if length(expr) == 2 # Verify correct argument length
			   	return DropShadow(parse(expr[2])) # Another easy parse I know
			else
				throw(LispError("Invalid syntax for drop_shadow."))
			end
		elseif op_symbol == :inner_shadow
		       	if length(expr) == 2 # Verify correct argument length
			   	return InnerShadow(parse(expr[2])) # Easy parse I know
			else
				throw(LispError("Invalid syntax for inner_shadow."))
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
		if typeof(exprs[1]) == Array{Any,1} # With nodes should all have this
		   	if length(exprs[1]) != 0 # Empty lists are okay. Handle seperately
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

# Analyze number structure
function analyze(owl::Num)
	return owl
end
# Analyze binomial operator tree
function analyze(owl::Binop)
	return Binop(owl.op, analyze(owl.lhs), analyze(owl.rhs)) # Pass on the recursion
end
# Handle plus nodes
function analyze(owl::Plus)
	if length(owl.operands) == 2 # Is this binomial?
		return Binop(+, analyze(owl.operands[1]), analyze(owl.operands[2]))
	else
		return Binop(+, analyze(owl.operands[1]), analyze(Plus(owl.operands[2:end])))
	end
end
# Handle Uni-operator trees
function analyze(owl::Unop) # Just pass on the recursion
	return Unop(owl.op, analyze(owl.operand))
end
# Handle id nodes
function analyze(owl::Id) # Just pass on the recursion
	return owl
end
# Handle if0 nodes
function analyze(owl::If0) # Just pass on the recursion
	return If0(analyze(owl.condition), analyze(owl.zero_branch), analyze(owl.nonzero_branch))
end
# Handle with nodes
function analyze(owl::With)
	syms = Id[]
	exprs = OWL[]
	for i in 1:size(owl.binders,1)
	      	push!(syms, owl.binders[i].name)
		push!(exprs, owl.binders[i].binding_expr)
	end
	return FunApp(FunDef(syms, analyze(owl.body)), map(analyze, exprs))
end
# Handle function definition nodes
function analyze(owl::FunDef) # Just pass on the recursion
	return FunDef(owl.formal_parameters, analyze(owl.fun_body))
end
# Handle function application nodes
function analyze(owl::FunApp) # Just pass on the recursion
	return FunApp(analyze(owl.fun_expr), map(analyze, owl.arg_exprs))
end
# Handle and nodes
function analyze(owl::And)
	if length(owl.operands) == 1 # It shouldn't ever return one on a root call. It will on recursion though.
		return If0(analyze(owl.operands[1]), Num(0), Num(1))
	else
		return If0(analyze(owl.operands[1]), Num(0), analyze(And(owl.operands[2:end])))
	end
end
# Handle simple load nodes
function analyze(owl::SimpleLoad)
	return owl # Leaf node
end
# Handle simple save nodes
function analyze(owl::SimpleSave)
	return SimpleSave(analyze(owl.matrix), owl.location) # Just pass on the recursion
end
# Handle render text nodes
function analyze(owl::RenderText)
	return RenderText(owl.words, analyze(owl.xpos), analyze(owl.ypos)) # Just pass on the recursion
end
# Handle emboss nodes
function analyze(owl::Emboss)
	return Emboss(analyze(owl.matrix)) # Just pass on the recursion
end
# Handle drop shadow nodes
function analyze(owl::DropShadow)
	return DropShadow(analyze(owl.matrix)) # Just pass on the recursion
end
# Handle inner shadow nodes
function analyze(owl::InnerShadow)
	return InnerShadow(analyze(owl.matrix)) # Just pass on the recursion
end
# Handle default function call
function analyze(owl::OWL) # Should never get here. Throw error.
	throw(LispError("Unkown node!")) # Just pass on the recursion
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
	 left = fetch(@spawn calc(owl.lhs, env)) # This is for threading both sides
	 right = fetch(@spawn calc(owl.rhs, env))
	 if (owl.op != ./) || right.n != 0
	    	 if typeof(left) == NumVal && typeof(right) == NumVal # Make sure things can evaluated in the calcs
		    	return NumVal(owl.op(left.n, right.n)) # Return correct calculation
		 # Handle new data types
		 elseif (typeof(left) == NumVal && typeof(right) == MatrixVal) || (typeof(left) == MatrixVal && typeof(right) == NumVal)
		 	return MatrixVal(owl.op(left.n, right.n))
		 # Handle new data types
		 elseif typeof(left) == MatrixVal && typeof(right) == MatrixVal
	       	 	if length(left.n) == length(right.n) && length(left.n[1]) == length(right.n[1]) # Check correct sizes
			   	return MatrixVal(owl.op(left.n, right.n))
			else
			   	throw(LispError("Cannot perform binary operations on matrices of different sizes."))
			end
		 else
			throw(LispError("Invalid binary op types did not evaluate correctly"))
		 end
	else
		throw(LispError("Cannot divide by zero."))
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
	 elseif hasSymVal(env.symvals, owl) != -1 # See helper function
	 	 return getValue(env.symvals, hasSymVal(env.symvals, owl)) # Return symbol value.
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
	 return ClosureVal(owl.formal_parameters, owl.fun_body, env) # Super simple I know.
end
# This is the FunApp node
function calc( owl::FunApp, env::Environment )
	 # The function expression should result in a ClosureVal
	 ret_closure = calc(owl.fun_expr, env)
	 if typeof(ret_closure) == ClosureVal # Check for correct evaluation
	    	 if length(owl.arg_exprs) == length(ret_closure.params) # These two lists should be equal.
	 	    	  # Extend the current environment by binding the actual parameters to the formal parameters
	 	 	  symvals = SymVal[] # Build array
			  for param in zip(ret_closure.params, owl.arg_exprs)
			      	  push!(symvals, SymVal(param[1], @spawn calc(param[2], env)))
	 	 	  end
	      	 	  return calc(ret_closure.body, CEnvironment(symvals, ret_closure.env)) # Calculate new environment and return
		 else
		    	  throw(LispError("Number of parameters and arguments does not match in function call/declaration."))
	 	 end
	 else
		 throw(LispError("Invalid type did not return closure"))
	 end
end
# Calculate SimpleLoad nodes
function calc(owl::SimpleLoad, env::Environment)
	return MatrixVal(simple_load(owl.location)) # Construct primitive off of loader function
end
# Calculate SimpleSave nodes
function calc(owl::SimpleSave, env::Environment)
	matrix = calc(owl.matrix, env)
	if typeof(matrix) == MatrixVal # Check correct input
	   	return simple_save(matrix.n, owl.location)
	else
		throw(LispError("Expected MatrixVal in simple_save of $matrix."))
	end
end
# Calculate RenderText nodes
function calc(owl::RenderText, env::Environment)
	xpos = fetch(@spawn calc(owl.xpos, env)) # Thread this too.
	ypos = fetch(@spawn calc(owl.ypos, env))
	if typeof(xpos) == NumVal && typeof(ypos) == NumVal # Check correct input
	   	return MatrixVal(render_text(owl.words, xpos.n, ypos.n))
	else
		throw(LispError("Expected NumVal in render_text."))
	end
end
# Calculate Emboss nodes
function calc(owl::Emboss, env::Environment)
	matrix = calc(owl.matrix, env)
	if typeof(matrix) == MatrixVal # Check correct input
	   	return MatrixVal(emboss(matrix.n))
	else
		throw(LispError("Expected MatrixVal in emboss."))
	end
end
# Calculate DropShadow nodes
function calc(owl::DropShadow, env::Environment)
	matrix = calc(owl.matrix, env)
	if typeof(matrix) == MatrixVal # Check correct input
	   	return MatrixVal(drop_shadow(matrix.n))
	else
		throw(LispError("Expected MatrixVal in drop_shadow."))
	end
end
# Calculate InnerShadow nodes
function calc(owl::InnerShadow, env::Environment)
	matrix = calc(owl.matrix, env)
	if typeof(matrix) == MatrixVal # Check correct input
	   	return MatrixVal(inner_shadow(matrix.n))
	else
		throw(LispError("Expected MatrixVal in inner_shadow."))
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
function hasSymVal(symvals::Array{SymVal}, name::Id)
	for i in 1:size(symvals,1) # Iterate through and find
	       	 if symvals[i].name.name == name.name # I added some parameters to the AST nodes
	       	    	 return i # Found
		 end
	end
	return -1 # Not found
end
# Helper function to get value associated with symbol name
function getValue(symvals::Array{SymVal}, index::Int)
	 return fetch(symvals[index].value)
	 throw(LispError("Symbol with \'$name\' not found - Good job on getting here though"))
end

#
# =================================================
#

# This is for debugging
function calc(expr::AbstractString)
	 return calc(analyze(parse(Lexer.lex(expr))))
end

#
# =================================================
#

end # module