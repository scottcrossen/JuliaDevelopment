# example: run this with "python grader_filename.py your_code_filename"
# this is an auto-generated file for general student testing

import sys
import subprocess
import os
from difflib import Differ
if __name__ == "__main__":
    fn = sys.argv[1]
    tmp_fn = "tmp.rkt"
    feedback_fn = "feedback.txt"
    run_cmd = "julia"
    tests = """

origin = Position(0,0)
out = Position(50, 50)

circle1 = Circ(origin, 1/sqrt(pi))
rectangle1 = Rect(origin, 3, 3)
square1 = Square(origin, 4)
println(round(area(circle1), 3)) #1.0
println(in_shape(rectangle1, origin)) # true

ptest1 = Pixel(8, 10, 12)
arrayPix1 = fill(ptest1, (3,3))
println(join(greyscale(arrayPix1), ","))
ptest2 = Pixel(255, 255, 255)
arrayPix2 = fill(ptest2, (3,3))
println(join(invert(arrayPix2), ","))

dadTest1 = Person("Ben", 2012, :fusia, Unknown(), Unknown())
momTest1 = Person("Kaitlyn ", 2012, :brown, Unknown(), Unknown())
me = Person("Test", 2012, :ink, dadTest1, momTest1)

println(count_persons(me)) # 3
println(average_age(me)) # 4

lambdaTester1 = x -> Person("Test", x.birthyear, x.eyecolor, x.father, x.mother)

println(tree_map(lambdaTester1, me))
println(add_last_name("Name", me))

println(eye_colors(me))
"""
    tests_info = """1. 1 point. area
2. 1 point. in-shape
3. 1 point. greyscale
4. 1 point. invert
5. 1 point. count_persons
6. 1 point. average_age
7. 1 point. tree_map
8. 1 point. last_name
9. 1 point. eye-colors"""
    correctoutput = """1.0
true
Pixel(10.0,10.0,10.0),Pixel(10.0,10.0,10.0),Pixel(10.0,10.0,10.0),Pixel(10.0,10.0,10.0),Pixel(10.0,10.0,10.0),Pixel(10.0,10.0,10.0),Pixel(10.0,10.0,10.0),Pixel(10.0,10.0,10.0),Pixel(10.0,10.0,10.0)
Pixel(0,0,0),Pixel(0,0,0),Pixel(0,0,0),Pixel(0,0,0),Pixel(0,0,0),Pixel(0,0,0),Pixel(0,0,0),Pixel(0,0,0),Pixel(0,0,0)
3
4.0
Person("Test",2012,:ink,Person("Test",2012,:fusia,Unknown(),Unknown()),Person("Test",2012,:brown,Unknown(),Unknown()))
Person("TestName",2012,:ink,Person("BenName",2012,:fusia,Unknown(),Unknown()),Person("Kaitlyn Name",2012,:brown,Unknown(),Unknown()))
Any[:ink,:fusia,:brown]
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
        feedback_file.write("\n        Failed tests:\n")
        for count, l in enumerate(tests_info.splitlines(1)):
            points = int(l.split()[1])
            if count in failed_tests_line_nos:
                total_possible += points
                feedback_file.write(l)
            elif count in extra_line_nos:
                pass
            else:
                total_possible += points
                grade += points
        feedback_file.write("\n        Grade:\n" + str(grade) + " out of " + str(total_possible))
    os.remove(tmp_fn)
    print("See feedback file: " + feedback_fn)
