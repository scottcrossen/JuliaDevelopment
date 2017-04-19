# example: run this with "python grader_filename.py your_code_filename"
# this is an auto-generated file for general student testing

import sys
import subprocess
import os
from difflib import Differ
if __name__ == "__main__":
    fn = sys.argv[1]
    tmp_fn = "tmp.jl"
    feedback_fn = "feedback.txt"
    run_cmd = "julia"
    tests = """


function lexParse(str)
  CITypes.parse(Lexer.lex(str))
end

function parseInter(str)
  CITypes.type_of_expr(lexParse(str))
end

function removeNL(str)
  replace(string(str), "\n", "")
end

function testerr(f, param)
  try
    return removeNL(f(param))
  catch Y
    if (typeof(Y) != Error.LispError)
      return Y
    else
      return "Error"
    end
  end
end


println(testerr(parseInter, "true"))
println(testerr(parseInter, "4"))
println(testerr(parseInter, "(+ 1 2)"))

println(testerr(parseInter, "(iszero nempty)"))
println(testerr(parseInter, "(ifb false (ncons 1 nempty) nempty)"))

println(testerr(parseInter, "(with x 3 ( + 1 x))"))
println(testerr(parseInter, "(lambda x : number false)"))
println(testerr(parseInter, "((lambda x : number false) false)"))

println(testerr(parseInter, "nempty"))
println(testerr(parseInter, "(nisempty false)"))
println(testerr(parseInter, "(nfirst (ncons 1 nempty))"))
println(testerr(parseInter, "(nrest nempty)"))
"""
    tests_info = """
1. 1 point. true
2. 1 point. 4
3. 1 point. (+ 1 2)
4. 1 point. (iszero nempty)
5. 1 point. (ifb false (ncons 1 nempty) nempty)
6. 1 point. (with x 3 ( + 1 x))
7. 1 point. (lambda x : number false)
8. 1 point. ((lambda x : number false) false)
9. 1 point. nempty
10. 1 point. (nisempty false)
11. 1 point. (nfirst (ncons 1 nempty))
12. 1 point. (nrest nempty)
"""
    correctoutput = """
CITypes.BoolType()
CITypes.NumType()
CITypes.NumType()
Error
CITypes.NListType()
CITypes.NumType()
CITypes.FunType(CITypes.NumType(),CITypes.BoolType())
Error
CITypes.NListType()
Error
CITypes.NumType()
CITypes.NListType()
"""
    grade = 0
    total_possible = 0
    with open(fn, "r") as f:
        with open(tmp_fn, "w") as w:
            w.write(f.read())
            w.write(tests)
    cmd = [run_cmd, tmp_fn]
    process = subprocess.Popen(cmd, stdout=subprocess.PIPE)
    studentoutput, err = process.communicate()
    comparison = "".join(Differ().compare(correctoutput.splitlines(1), studentoutput.splitlines(1)))
    error_line_nos = []
    extra_line_nos = []
    q_line_nos = []
    for count, i in enumerate(comparison.splitlines()):
        if "-" == i[0]:
            error_line_nos.append(count)
        elif "+" == i[0]:
            extra_line_nos.append(count)
        elif "?" == i[0]:
            q_line_nos.append(count)
    failed_tests_line_nos = []
    for x in error_line_nos:
        numextralines = len([y for y in extra_line_nos if y < x])
        numqlines = len([z for z in q_line_nos if z < x])
        failed_tests_line_nos.append(x - numextralines - numqlines)
    with open(feedback_fn, "w") as feedback_file:
        feedback_file.write("        Correct output:\n")
        feedback_file.write(str(correctoutput))
        feedback_file.write("\n        Your output:\n")
        feedback_file.write(str(studentoutput))
        #feedback_file.write("\n        Failed tests:\n")
        #for count, l in enumerate(tests_info.splitlines(1)):
        #    points = int(l.split()[1])
        #    if count in failed_tests_line_nos:
        #        total_possible += points
        #        feedback_file.write(l)
        #    else:
        #        total_possible += points
        #        grade += points
        #feedback_file.write("\n        Grade:\n" + str(grade) + " out of " + str(total_possible))
    os.remove(tmp_fn)
    print("See feedback file: " + feedback_fn)
