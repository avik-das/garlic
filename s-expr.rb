#!/usr/bin/env ruby

class EvalException < Exception; end

module Lang
  def Lang.is_truthy(val)
    val != false && val != []
  end
end

class EnvFrame
  def initialize(parent = nil)
    self.vars = {}
    self.parent = parent
  end

  def add(name, value)
    self.vars[name] = value
  end

  def find(name)
    if self.vars.has_key?(name)
      self.vars[name]
    elsif self.parent
      self.parent.find(name)
    else
      raise EvalException.new("undefined symbol '#{name}'")
    end
  end

  attr_accessor :vars, :parent
end

class Env
  def initialize
    self.root_frame = EnvFrame.new
    self.root_frame.add(:+, BuiltinFunction.new { |*args| args.reduce(0, :+) })
    self.root_frame.add(:*, BuiltinFunction.new { |*args| args.reduce(1, :*) })
    self.root_frame.add(:-, BuiltinFunction.new { |x, y| x - y })
    self.root_frame.add(:'=', BuiltinFunction.new { |*args|
      if args.all? { |arg| arg.is_a?(Numeric) }
        res = true
        0.upto(args.size - 2) do |i|
          if args[i] != args[i + 1]
            res = false
            break
          end
        end

        res
      else
        false
      end
    })
  end

  attr_accessor :root_frame
end

class Callable; end

class BuiltinFunction < Callable
  def initialize(&block)
    self.block = block
  end

  def call(*args)
    self.block.call(*args)
  end

  attr_accessor :block
end

class Lambda < Callable
  def initialize(params, body, parent_frame)
    self.params = params
    self.body = body
    self.parent_frame = parent_frame
  end

  def call(*args)
    unless args.size == self.params.size
      raise EvalException.new(
        "Expecting #{self.params.size} arguments, got #{args.size}")
    end

    frame = EnvFrame.new(self.parent_frame)

    self.params.zip(args).each do |param, arg|
      frame.add(param, arg)
    end

    self.body.eval(frame)
  end

  attr_accessor :params, :body, :parent_frame
end

class Program
  def initialize
    self.expressions = []
  end

  def add_child(child)
    self.expressions << child
    child.parent = self
  end

  def statements
    self.expressions.map(&:to_value)
  end

  def eval(env = nil)
    env ||= Env.new
    frame = env.root_frame
    self.expressions[0, self.expressions.size - 1].reduce(frame) { |frame, expr|
      expr.eval(frame)
      frame
    }

    self.expressions[-1].eval(frame)
  end

  attr_accessor :expressions
end

class Node
  attr_accessor :parent
end

class Var < Node
  def initialize(name)
    self.name = name
  end

  def to_value
    self.name
  end

  def eval(frame)
    frame.find(self.name)
  end

  attr_accessor :name
end

class Int < Node
  def initialize(value)
    self.value = value
  end

  def to_value
    self.value
  end

  def eval(frame)
    self.value
  end

  attr_accessor :value
end

class BoolValue < Node; end

class True < BoolValue
  def to_value
    true
  end

  def eval(frame)
    true
  end
end

class False < BoolValue
  def to_value
    false
  end

  def eval(frame)
    false
  end
end

class NestedNode < Node
  def initialize
    self.children = []
  end

  def add_child(child)
    self.children << child
    child.parent = self
  end

  def to_value
    self.children.map(&:to_value)
  end

  def eval(frame)
    first = self.children.first

    if first.is_a?(Var) and first.name == :define
      unless self.children.size == 3
        raise EvalException.new(
          "Definition must contain three elements! #{self.to_value}")
      end

      unless self.children[1].is_a?(Var)
        raise EvalException.new(
          "Name of definition is not a symbol! #{self.children[1].to_value}")
      end

      name = self.children[1].name
      value = self.children[2].eval(frame)

      frame.add(name, value)
    elsif first.is_a?(Var) and first.name == :let
      unless self.children.size == 3
        raise EvalException.new(
          "Let must contain three elements! #{self.to_value}")
      end

      unless self.children[1].is_a?(NestedNode)
        raise EvalException.new(
          "Binding list is not a list! #{self.children[1].to_value}")
      end

      bindings = self.children[1].children

      bindings.each do |binding|
        unless binding.is_a?(NestedNode)
          raise EvalException.new(
            "Binding is not a list! #{binding.to_value}")
        end

        unless binding.children.size == 2
          raise EvalException.new(
            "Binding must contain two elements! #{binding.to_value}")
        end

        unless binding.children.first.is_a?(Var)
          raise EvalException.new(
            "Binding name is not a symbol! #{binding.children.first.to_value}")
        end
      end

      new_frame = EnvFrame.new(frame)

      bindings.each do |binding|
        name = binding.children[0].name
        value = binding.children[1].eval(frame)
        new_frame.add(name, value)
      end

      res = self.children[2].eval(new_frame)

      res
    elsif first.is_a?(Var) and first.name == :lambda
      unless self.children.size == 3
        raise EvalException.new(
          "Lambda must contain three elements! #{self.to_value}")
      end

      unless self.children[1].is_a?(NestedNode)
        raise EvalException.new(
          "Parameter list is not a list! #{self.children[1].to_value}")
      end

      params = self.children[1].children

      params.each do |param|
        unless param.is_a?(Var)
          raise EvalException.new(
            "Parameter is not a symbol! #{param.to_value}")
        end
      end

      body = self.children[2]
      Lambda.new(params.map(&:name), body, frame)
    elsif first.is_a?(Var) and first.name == :if
      unless self.children.size == 4
        raise EvalException.new(
          "If must contain four elements! #{self.to_value}")
      end

      cond = Lang.is_truthy(self.children[1].eval(frame))

      if cond
        self.children[2].eval(frame)
      else
        self.children[3].eval(frame)
      end
    elsif first.is_a?(Var) and first.name == :quote
      unless self.children.size == 2
        raise EvalException.new(
          "Nothing to quote! #{self.to_value}")
      end

      self.children[1].to_value
    else
      unless self.children.size >= 1
        raise EvalException.new(
          "Function call must have at least one element! #{self.to_value}")
      end

      fn = first.eval(frame)

      unless fn.is_a?(Callable)
        raise EvalException.new("Cannot call #{fn.to_value}")
      end

      args = self.children[1, self.children.size - 1].map { |arg|
        arg.eval(frame)
      }

      fn.call(*args)
    end
  end

  attr_accessor :children
end

def parse(str)
  str.strip!
  str.gsub!("\n", " ")

  curr = root = Program.new

  while !str.empty?
    if str =~ /^(\s+)/
      str = str[$1.size, str.size - 1]
    elsif str[0] == "("
      child = NestedNode.new
      curr.add_child(child)
      curr = child

      str = str[1, str.size - 1]
    elsif str[0] == ")"
      curr = curr.parent
      str = str[1, str.size - 1]
    elsif str =~ /^#t/
      curr.add_child(True.new)
      str = str[2, str.size - 1]
    elsif str =~ /^#f/
      curr.add_child(False.new)
      str = str[2, str.size - 1]
    elsif str =~ /^([0-9]+)/
      value = $1
      int = Int.new(value.to_i)
      curr.add_child(int)

      str = str[value.size, str.size - 1]
    elsif str =~ /^([a-z\-\+\*=][a-z0-9\-\?\+\*=]*)/i
      name = $1
      var = Var.new(name.intern)
      curr.add_child(var)

      str = str[name.size, str.size - 1]
    else
      # XXX: for now
      puts "Warning: ignoring beginning of #{str}"
      str = str[1, str.size - 1]
    end
  end

  root
end

## MAIN (TESTING) ##############################################################

#prog = "(define add +) (define a 4) (add a (* 3 2))"

#prog = "
#(let ((a 2)
#      (b 3))
#     (let ((f *))
#          (f a b)))
#"

#prog = "
#(let ((a 2))
#     (+ a (let ((a 3)) a)))
#"

#prog = "
#((lambda (x y) (+ x y)) 1 2)
#"

#prog = "
#(define fn
#        (lambda (x y z) (+ x y (* y z))))
#
#(fn 1 2 3)
#"

#prog = "(if (quote (1 2 3)) 1 2)"

#prog = "
#(define fac
#        (lambda (n) (if (= n 0) 1
#                                (* n (fac (- n 1))) )) )
#
#(fac 6)
#"

env = Env.new
stdlib = File.read("stdlib.scm")
parse(stdlib).eval(env)

prog = "
(define my-list
        (cons 1 (cons 2 (cons 3 (cons 4 (quote ()))))) )

(map (lambda (x) (* x 2)) my-list)
"

parsed = parse(prog)
puts parsed.statements.inspect
puts parsed.eval(env)

consPrinter = BuiltinFunction.new { |x, y|
  if y == []
    puts x
  else
  print "#{x}, "
    y.call(consPrinter)
  end
}
parsed.eval(env).call(consPrinter)

exit

## MAIN ########################################################################

puts "s-expr 0.0.1 - by Avik Das"
puts "Press Ctrl-D to exit"
puts

env = Env.new
stdlib = File.read("stdlib.scm")
parse(stdlib).eval(env)

while true
  print "> "
  inp = gets

  break unless inp

  parsed = parse(inp)

  begin
    puts parsed.eval(env).inspect
  rescue EvalException => e
    puts "Err: #{e.to_s}"
  end
end

# vim: ts=2 sw=2 :
