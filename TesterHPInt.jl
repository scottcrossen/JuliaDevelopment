push!(LOAD_PATH, ".")
macro tnum()
	return :(testnum += 1)
end
using Lexer
using Error
using HPInt
using Cairo
using Images
testnum = 0
function testNum(num)
	return string(num) * ". "
end
function parseT(str)
	HPInt.parse(Lexer.lex(str))
end
function analyze(str)
	HPInt.analyze(parseT(str))
end
function interpret(str)
	HPInt.calc(analyze(str))
end


test1="(with ((x 1)) x)"
test2="((lambda (x) x) 1)"
println(testNum(@tnum()) * "analyze " * test1)
try
	HPInt.analyze(HPInt.parse(Lexer.lex(test1)))
catch
	println("Failed test " * testNum(testnum))
	println(HPInt.analyze(HPInt.parse(Lexer.lex(test1))))
end
println(testNum(@tnum()) * "analyze " * test2)
try
	HPInt.analyze(HPInt.parse(Lexer.lex(test2)))
catch
	println("Failed test " * testNum(testnum))
	println(HPInt.analyze(HPInt.parse(Lexer.lex(test2))))
end
try
	if(string(HPInt.analyze(HPInt.parse(Lexer.lex(test1))))==string(analyze(HPInt.parse(Lexer.lex(test2)))))
		println("Passed tests " * testNum(testnum-1) * "and " * testNum(testnum))
	else
		println("Failed test " * testNum(testnum-1) * "and " * testNum(testnum) * "These should be the same:")
		println(HPInt.analyze(HPInt.parse(Lexer.lex(test1))))
		println(HPInt.analyze(HPInt.parse(Lexer.lex(test2))))
	end
catch 
	println("Failed test " * testNum(testnum-1) * "and " * testNum(testnum) * "These should be the same:")
	println(HPInt.analyze(HPInt.parse(Lexer.lex(test1))))
	println(HPInt.analyze(HPInt.parse(Lexer.lex(test2))))
end
println(testNum(@tnum()) * "calc " * test1)
try
	HPInt.calc(test1)
catch
	println("Failed test " * testNum(testnum))
	println(HPInt.calc(test1))
end
println(testNum(@tnum()) * "calc " * test2)
try
	HPInt.calc(test2)
catch
	println("Failed test " * testNum(testnum))
	println(HPInt.calc(test2))
end
try
	if(string(HPInt.calc(test1))==string(HPInt.calc(test2)))
		println("Passed tests " * testNum(testnum-1) * "and " * testNum(testnum))
	else
		println("Failed test " * testNum(testnum-1) * "and " * testNum(testnum) * "These should be the same:")
		println(HPInt.calc(test1))
		println(HPInt.calc(test2))
	end
catch
	println("Failed test " * testNum(testnum-1) * "and " * testNum(testnum) * "These should be the same:")
	println(HPInt.calc(test1))
	println(HPInt.calc(test2))
end


test1="(with ((x 1) (y 2)) y)"
test2="(with ((x 1) (y 2)) y)"
println(testNum(@tnum()) * "analyze " * test1)
try
	HPInt.analyze(HPInt.parse(Lexer.lex(test1)))
catch
	println("Failed test " * testNum(testnum))
	println(HPInt.analyze(HPInt.parse(Lexer.lex(test1))))
end
println(testNum(@tnum()) * "analyze " * test2)
try
	HPInt.analyze(HPInt.parse(Lexer.lex(test2)))
catch
	println("Failed test " * testNum(testnum))
	println(HPInt.analyze(HPInt.parse(Lexer.lex(test2))))
end
try
	if(string(HPInt.analyze(HPInt.parse(Lexer.lex(test1))))==string(HPInt.analyze(HPInt.parse(Lexer.lex(test2)))))
		println("Passed tests " * testNum(testnum-1) * "and " * testNum(testnum))
	else
		println("Failed test " * testNum(testnum-1) * "and " * testNum(testnum) * "These should be the same:")
		println(HPInt.analyze(HPInt.parse(Lexer.lex(test1))))
		println(HPInt.analyze(HPInt.parse(Lexer.lex(test2))))
	end
catch
	println("Failed test " * testNum(testnum-1) * "and " * testNum(testnum) * "These should be the same:")
	println(HPInt.analyze(HPInt.parse(Lexer.lex(test1))))
	println(HPInt.analyze(HPInt.parse(Lexer.lex(test2))))
end
println(testNum(@tnum()) * "calc " * test1)
try
	HPInt.calc(test1)
catch
	println("Failed test " * testNum(testnum))
	println(HPInt.calc(test1))
end
println(testNum(@tnum()) * "calc " * test2)
try
	HPInt.calc(test2)
catch
	println("Failed test " * testNum(testnum))
	println(HPInt.calc(test2))
end
try
	if(string(HPInt.calc(test1))==string(HPInt.calc(test2)))
		println("Passed tests " * testNum(testnum-1) * "and " * testNum(testnum))
	else
		println("Failed test " * testNum(testnum-1) * "and " * testNum(testnum) * "These should be the same:")
		println(HPInt.calc(test1))
		println(HPInt.calc(test2))
	end
catch
	println("Failed test " * testNum(testnum-1) * "and " * testNum(testnum) * "These should be the same:")
	println(HPInt.calc(test1))
	println(HPInt.calc(test2))
end


println(testNum(@tnum()) * "Load and Save")
test="(with ((cat (simple_load \"cat_256.png\"))) (simple_save cat \"output"*string(testnum)*".png\"))"
try
	interpret(test)
	println("Passed test " * testNum(testnum))
catch
	println("Failed to create image from sample code.")
    	interpret(test)
end
println(testNum(@tnum()) * "Emboss")
test="(with ((cat (simple_load \"cat_256.png\"))) (simple_save (emboss cat) \"output"*string(testnum)*".png\"))"
try
	interpret(test)
	println("Passed test " * testNum(testnum))
catch
	println("Failed to create image from sample code.")
    	interpret(test)
end
println(testNum(@tnum()) * "Inner Shadow")
test="(with ((cat (simple_load \"cat_256.png\"))) (simple_save (inner_shadow cat) \"output"*string(testnum)*".png\"))"
try
	interpret(test)
	println("Passed test " * testNum(testnum))
catch
	println("Failed to create image from sample code.")
    	interpret(test)
end
println(testNum(@tnum()) * "Drop Shadow")
test="(with ((cat (simple_load \"cat_256.png\"))) (simple_save (drop_shadow cat) \"output"*string(testnum)*".png\"))"
try
	interpret(test)
	println("Passed test " * testNum(testnum))
catch
	println("Failed to create image from sample code.")
    	interpret(test)
end
println(testNum(@tnum()) * "Max")
test="(with ((cat (simple_load \"cat_256.png\"))) (simple_save (max 0.5 cat) \"output"*string(testnum)*".png\"))"
try
	interpret(test)
	println("Passed test " * testNum(testnum))
catch
	println("Failed to create image from sample code.")
    	interpret(test)
end
println(testNum(@tnum()) * "Min")
test="(with ((cat (simple_load \"cat_256.png\"))) (simple_save (min 0.5 cat) \"output"*string(testnum)*".png\"))"
try
	interpret(test)
	println("Passed test " * testNum(testnum))
catch
	println("Failed to create image from sample code.")
    	interpret( test)
end
println(testNum(@tnum()) * "Decrement")
test="(with ((cat (simple_load \"cat_256.png\"))) (simple_save (- 1 cat) \"output"*string(testnum)*".png\"))"
try
	interpret(test)
	println("Passed test " * testNum(testnum))
catch
	println("Failed to create image from sample code.")
    	interpret(test)
end
println(testNum(@tnum()) * "Addition")
test="(with ((cat (simple_load \"cat_256.png\"))) (simple_save (+ cat cat) \"output"*string(testnum)*".png\"))"
try
	interpret(test)
	println("Passed test " * testNum(testnum))
catch
	println("Failed to create image from sample code.")
    	interpret(test)
end
println(testNum(@tnum()) * "Subtraction")
test="(with ((cat (simple_load \"cat_256.png\"))) (simple_save (- cat cat) \"output"*string(testnum)*".png\"))"
try
	interpret(test)
	println("Passed test " * testNum(testnum))
catch
	println("Failed to create image from sample code.")
    	interpret( test)
end
println(testNum(@tnum()) * "Render Text")
test="(with ((cat (simple_load \"cat_256.png\"))) (simple_save (min (render_text \"CAT\" 40 230) cat) \"output"*string(testnum)*".png\"))"
try
	interpret(test)
	println("Passed test " * testNum(testnum))
catch
	println("Failed to create image from sample code.")
    	interpret(test)
end
println(testNum(@tnum()) * "Complex")
test="(with ((cat (simple_load \"cat_256.png\"))) (simple_save (* (- 1 cat) cat) \"output"*string(testnum)*".png\"))"
try
	interpret(test)
	println("Passed test " * testNum(testnum))
catch
	println("Failed to create image from sample code.")
    	interpret(test)
end


println(testNum(@tnum()) * "Swirl Load")
test="(with ((cat (simple_load \"swirl_256.png\"))) (simple_save (* (- 1 cat) cat) \"output"*string(testnum)*".png\"))"
try
	interpret(test)
	println("Passed test " * testNum(testnum))
catch
	println("Failed to create image from sample code.")
    	interpret(test)
end
println(testNum(@tnum()) * "Sample Hello World")
test="(with ( (base_img (render_text \"Hello\" 25 100)) (swirl (simple_load \"swirl_256.png\")) ) (with ( (ds (drop_shadow base_img)) ) (with ( (tmp4 (+ (* (+ (min ds base_img) (- 1 base_img)) base_img) (* (- 1 base_img) swirl) )) ) (with ( (tmp5 (- 1 (emboss tmp4))) (base_img2 (render_text \"world!\" 5 200)) ) (with ( (is (inner_shadow base_img2)) ) (with ( (tmp6 (max base_img2 (* (- 1 base_img2) is))) ) (with ( (output (min tmp5 tmp6)) ) (simple_save output \"output"*string(testnum)*".png\") ) ) ) ) ) ) )"
try
	interpret(test)
	println("Passed test " * testNum(testnum))
catch
	println("Failed to create image from sample code.")
    	interpret(test)
end


println(testNum(@tnum()) * "Cat picture")
test="(with ((cat (simple_load \"cat_256.png\"))) (with ((inv (- 1 cat))) (with ( (output (drop_shadow inv)) ) (simple_save output \"output"*string(testnum)*".png\"))))"
try
	interpret(test)
	println("Passed test " * testNum(testnum))
catch
	println("Failed to create cat image.")
	interpret(test)
end