CURRENT_YEAR=2017

# Define shapes
abstract Shape
type Position
     x::Real
     y::Real
end
type Circ <: Shape
     center::Position
     radius::Real
end
type Square <: Shape
     upper_left::Position
     length::Real
end
type Rect <: Shape
     upper_left::Position
     width::Real
     height::Real
end


# Define area function
function area(shape::Rect)
	 shape.width * shape.height
end
function area(shape::Circ)
	 return (pi * (shape.radius * shape.radius))
end
function area(shape::Square)
	 return (shape.length * shape.length)
end


# Define in_shape function
function in_shape(shape::Square, position::Position)
	 !(position.x > shape.upper_left.x ||
	 	      position.x < shape.upper_left.x + shape.length ||
		      position.y > shape.upper_left.y ||
		      position.y < shape.upper_left.y - shape.length)
end
function in_shape(shape::Circ, position::Position)
	 ((position.x - shape.center.x)^2 + (position.y - shape.center.y)^2) <= shape.radius^2
end
function in_shape(shape::Rect, position::Position)
	 !(position.x < shape.upper_left.x ||
	 	      position.x > shape.upper_left.x + shape.width ||
		      position.y > shape.upper_left.y ||
		      position.y < shape.upper_left.y - shape.height)
end


# Define 'Pixel'
type Pixel
     r::Real
     b::Real
     g::Real
end


# Define greyscale function
function greyscale(picture::Array{Pixel,2})
	 map(x -> Pixel((x.r+x.b+x.g)/3, (x.r+x.b+x.g)/3, (x.r+x.b+x.g)/3), picture)
end


# Define invert function
function invert(picture::Array{Pixel,2})
	 map(x -> Pixel(255 - x.r, 255 - x.b, 255 - x.g), picture)
end


# Define data trees
abstract TreeItem
type Person <: TreeItem
     name::AbstractString
     birthyear::Integer
     eyecolor::Symbol
     father::TreeItem
     mother::TreeItem
end
type Unknown <: TreeItem
end


# Define count_persons function
function count_persons(tree)
	 isa(tree, Person) ? 1 + count_persons(tree.mother) + count_persons(tree.father) : 0
end


# Define average_age function
function total_age(tree)
	 isa(tree, Person) ? (CURRENT_YEAR - tree.birthyear) + total_age(tree.mother) + total_age(tree.father) : 0
end
function average_age(tree)
	 isa(tree, Person) ? total_age(tree) / count_persons(tree) : 0
end


# Define tree_map function
function tree_map(f, tree)
	 if isa(tree, Person)
	    treeitem=Person(tree.name, tree.birthyear, tree.eyecolor, tree.father, tree.mother)
	    treeitem.mother = tree_map(f,treeitem.mother)
	    treeitem.father = tree_map(f,treeitem.father)
	    afterfunc=f(treeitem)
	    afterfunc == nothing ? treeitem : afterfunc
	 else
	    Unknown()
	 end
end


# Define add_last_name function
function add_last_name(name::AbstractString, tree)
	 tree_map(x -> Person(x.name * name, x.birthyear, x.eyecolor, x.father, x.mother), tree)
end


# Define eye_colors function
function eye_colors(tree)
	 isa(tree, Person) ? [tree.eyecolor; eye_colors(tree.father); eye_colors(tree.mother)] : []
end





#=
# Begin test cases

# Test shape constructors
rect = Rect(Position(1, 1), 3, 4)
circ = Circ(Position(1, 1), 2)
square = Square(Position(1, 1), 2)

# Test area function
println(area(rect))
println(area(circ))
println(area(square))
println(in_shape(square, Position(0,2)))

# Test pixel constructors
px1=Pixel(128,128,128)
px2=Pixel(0,255,0)
px3=Pixel(255,0,255)
px4=Pixel(0,128,0)
picture=[px1 px2 ; px3 px4]
println(picture)
# Test greyscale function
println(greyscale(picture))
# Test invert function
println(invert(picture))
# Test tree constructrs
person1=Person("name1",1992,:C1,Unknown(),Unknown())
person2=Person("name2",1993,:C2,Unknown(),Unknown())
person3=Person("name3",1994,:C3,person1,person2)
person4=Person("name4",1995,:C4,person3,Unknown())
# Test count_persons function
println(count_persons(Unknown()))
println(count_persons(person1))
println(count_persons(person2))
println(count_persons(person3))
println(count_persons(person4))
# Test average_age function
println(average_age(Unknown()))
println(average_age(person1))
println(average_age(person2))
println(average_age(person3))
println(average_age(person4))
# Test tree_map function
println("unkown:")
tree_map(println, Unknown())
println("person1:")
tree_map(println, person1)
println("person2:")
tree_map(println, person2)
println("person3:")
tree_map(println, person3)
println("person4:")
tree_map(println, person4)
# Test add_last_name function
println(add_last_name(" last",Unknown()))
println(add_last_name(" last",person1))
println(add_last_name(" last",person2))
println(add_last_name(" last",person3))
println(add_last_name(" last",person4))
# Test eye_colors function
println(eye_colors(Unknown()))
println(eye_colors(person1))
println(eye_colors(person2))
println(eye_colors(person3))
println(eye_colors(person4))
# Finish test cases
println("Finished test cases\n\n\n\n\n")
=#