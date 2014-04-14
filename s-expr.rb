#!/usr/bin/env ruby

require 'parslet'
require 'singleton'

## INITIAL PARSER ##############################################################

class Scheme < Parslet::Parser
  rule(:space)     { match('\s').repeat(1) }
  rule(:space?)    { space.maybe }

  rule(:int)       { match('[0-9]').repeat(1).as(:int) }
  rule(:bool)      { str('#t').as(:true) | str('#f').as(:false) }
  rule(:var)       {
    (match('[A-Za-z\-\+\*\/_]') >>
      match('[A-Za-z\-\+\*\/_\?0-9]').repeat(1).maybe).as(:var)
  }

  rule(:list)      {
    str('(') >>
      space? >>
      # Make sure to use .repeat(0, 1) instead of .maybe, since we always want
      # a list in the parse tree, regardless of whether it matched one sub-
      # expression or more.
      (expr >> (space >> expr).repeat(1).maybe).repeat(0, 1).as(:list) >>
      space? >>
      str(')')
  }

  rule(:atom)      { int | bool | var }
  rule(:expr)      { atom | list }
  rule(:expr_list) { ((space? >> expr).repeat(1).maybe >> space?).as(:exprs) }

  root(:expr_list)
end

## AST GENERATION ##############################################################

module AST
  class ParseException < Exception; end

  class Program
    def initialize(*statements)
      @statements = statements
    end

    def specialize
      @statements = @statements.map(&:specialize)
      self
    end

    def to_s
      @statements.join("\n\n")
    end

    attr :statements
  end

  class Node
    def has_children?
      false
    end

    def specialize
      self
    end
  end

  class Var < Node
    def initialize(name)
      @name = name
    end

    def to_s
      @name.to_s
    end

    attr :name
  end

  class IntVal < Node
    def initialize(value)
      @value = value
    end

    def to_s
      @value.to_s
    end

    attr :value
  end

  class TrueVal < Node
    include Singleton

    def to_s
      "#t"
    end
  end

  class FalseVal < Node
    include Singleton

    def to_s
      "#f"
    end
  end

  class NestedNode < Node
    def initialize(*children)
      @children = children
    end

    def has_children?
      true
    end

    def specialize
      if @children.empty?
        self # TODO
      else
        first = @children.first
        first_is_var = first.is_a?(Var)

        if first_is_var
          case first.name
            when :define
              return Definition.new(*children)
            when :lambda
              return Lambda.new(*children)
            when :if
              return If.new(*children)
          end
        end

        specialized_children = @children.map(&:specialize)
        FunctionCall.new(*specialized_children)
      end
    end

    def internal_color
      "1;32"
    end

    def to_s
      children_str = @children.map { |child|
        if child.has_children?
          child.to_s
        else
          "\033[#{self.internal_color}m#{child}\033[0m"
        end
      }.join(" ")

      "(#{children_str})"
    end

    attr :children
  end

  class SpecializedNode < Node
    def has_children?
      true
    end
  end

  class Definition < SpecializedNode
    def initialize(*children)
      unless children.size == 3
        raise ParseException.new(
          "Definition must have 3 children, got #{children.size}: " +
            "(#{children.join(" ")})")
      end

      unless children[1].is_a?(Var)
        raise ParseException.new(
          "Definition name must be a var, got: " +
            "#{children[1]} of type #{children[1].class}")
      end

      @name = children[1]
      @value = children[2].specialize
    end

    def to_s
      "(\033[#{self.internal_color}mdefine\033[0m " +
        "\033[1m#{@name}\033[0m #{@value})"
    end

    def internal_color
      "1;31"
    end

    attr :name, :value
  end

  class Lambda < SpecializedNode
    def initialize(*children)
      unless children.size == 3
        raise ParseException.new(
          "Lambda must have 3 children, got #{children.size}: " +
            "(#{children.join(" ")})")
      end

      unless children[1].has_children?
        raise ParseException.new(
          "Lambda parameter list must be a list, got: " +
            "#{children[1]} of type #{children[1].class}")
      end

      children[1].children.each do |param|
        unless param.is_a?(Var)
          raise ParseException.new(
            "Lambda parameter must be a var, got: " +
              "#{param} of type #{param.class}")
        end
      end

      @params = children[1].children
      @body = children[2].specialize
    end

    def to_s
      "(\033[#{self.internal_color}mlambda\033[0m " +
        "(\033[1m#{@params.join(" ")}\033[0m) " + body.to_s
    end

    def internal_color
      "1;33"
    end

    attr :params, :body
  end

  class FunctionCall < SpecializedNode
    def initialize(*children)
      @func = children.first
      @args = children[1, children.size - 1]
    end

    def to_s
      args_str = @args.map(&:to_s).join(" ")
      "(\033[#{self.internal_color}m#{@func}\033[0m #{args_str})"
    end

    def internal_color
      "1;34"
    end

    attr :func, :args
  end

  class If < SpecializedNode
    def initialize(*children)
      unless children.size == 4
        raise ParseException.new(
          "If must have 4 children, got #{children.size}: " +
            "(#{children.join(" ")})")
      end

      @cond = children[1].specialize
      @true_expr = children[2].specialize
      @false_expr = children[3].specialize
    end

    def to_s
      "(\033[#{self.internal_color}mif\033[0m " +
        "#{@cond} #{@true_expr} #{@false_expr}"
    end

    def internal_color
      "1;35"
    end

    attr :cond, :true_expr, :false_expr
  end

  class ASTTransform < Parslet::Transform
    rule(var: simple(:name))  { Var.new(name.to_s.intern) }
    rule(int: simple(:value)) { IntVal.new(value.to_i) }
    rule(true: simple(:x))    { TrueVal.instance }
    rule(false: simple(:x))   { FalseVal.instance }

    rule(list: sequence(:x))  { NestedNode.new(*x) }

    rule(exprs: sequence(:x)) { Program.new(*x) }
  end

  def AST.construct_from_parse_tree(tree)
    ast = ASTTransform.new.apply(tree)
    ast.specialize
  end
end

## MAIN (TESTING) ##############################################################

inp = File.read("stdlib.scm")
parsed = Scheme.new.parse(inp)
ast = AST.construct_from_parse_tree(parsed)
require 'pp'
pp ast
exit

## MAIN ########################################################################

env = Env.new
stdlib = File.read("stdlib.scm")
parse(stdlib).eval(env)

puts "s-expr 0.0.1 - by Avik Das"
puts "Press Ctrl-D to exit"
puts

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
