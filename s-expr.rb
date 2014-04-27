#!/usr/bin/env ruby

require 'parslet'
require 'singleton'
require 'digest/md5'
require 'securerandom'

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

  rule(:quoted)     {
    str('\'') >> (atom | list).as(:quoted)
  }

  rule(:comment)    {
    str(';') >> match('[^\n]').repeat(1).maybe.as(:comment) >> str("\n")
  }

  rule(:atom)      { int | bool | var }
  rule(:expr)      { atom | list | quoted | comment }
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

    def specialize_quoted
      self
    end
  end

  class Comment < Node
    def initialize(text)
      @text = text
    end

    attr :text
  end

  class Quoted < Node
    def initialize(value)
      @value = value
    end

    def specialize
      specialize_quoted
    end

    def specialize_quoted
      @value.specialize_quoted
    end

    attr :value
  end

  class QuotedAtom < Node
    def initialize(name)
      @name = name
    end

    attr :name
  end

  class QuotedList < Node
    def initialize(children)
      @children = children
    end

    def has_children?
      true
    end

    attr :children
  end

  class Nil < Node
    include Singleton

    def to_s
      "'()"
    end
  end

  class Var < Node
    def initialize(name)
      @name = name
    end

    def to_s
      @name.to_s
    end

    def specialize_quoted
      QuotedAtom.new(@name)
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

    def specialize_quoted
      self
    end

    attr :value
  end

  class TrueVal < Node
    include Singleton

    def to_s
      "#t"
    end

    def specialize_quoted
      self
    end
  end

  class FalseVal < Node
    include Singleton

    def to_s
      "#f"
    end

    def specialize_quoted
      self
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

    def specialize_quoted
      if @children.empty?
        Nil.instance
      else
        quoted_children = @children.map(&:specialize_quoted)
        QuotedList.new(quoted_children)
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

    rule(quoted: simple(:q))  { Quoted.new(q) }

    rule(comment: simple(:c)) { Comment.new(c) }
    rule(exprs: sequence(:x)) { Program.new(*x) }
  end

  def AST.construct_from_parse_tree(tree)
    ast = ASTTransform.new.apply(tree)
    ast.specialize
  end
end

## NATIVE CODE GENERATION ######################################################

module AST
  # The registers used for passing arguments to function calls, in the order of
  # priority. That is, the first argument of a function goes into %rdi, then
  # the second argument goes into %rsi, etc. The rest are passed on the stack,
  # from right to left.
  ARG_REGISTERS = [
    "%rdi",
    "%rsi",
    "%rdx",
    "%rcx",
    "%r8",
    "%r9"
  ]

  class Program
    def codegen(filename)
      vm = VM::VM.new(filename)

      @statements.each do |statement|
        statement.codegen(vm)
      end

      vm.commit
    end
  end

  class Comment < Node
    def codegen(vm)
      # do nothing!
    end
  end

  class QuotedAtom < Node
    def codegen(vm)
      name = vm.addquotedatom(@name)
      vm.asm "        mov     $#{name}_name, %rdi"
      vm.asm "        call    get_atom"
    end
  end

  class QuotedList < Node
    def codegen(vm)
      vm.asm "        mov     $0, %rsi"

      @children.reverse.each_with_index do |child, index|
        vm.push("%rsi")
        child.codegen(vm)
        vm.pop("%rsi")

        vm.asm "        mov     %rax, %rdi"
        vm.asm "        call    make_cons"

        unless index == @children.size - 1
          vm.asm "        mov     %rax, %rsi"
        end
      end
    end
  end

  class Nil < Node
    def codegen(vm)
      vm.asm "        mov     $0, %rax"
    end
  end

  class Var < Node
    def codegen(vm)
      varname = vm.addvarname(self)

      vm.argframe
      vm.asm "        mov     $#{varname}, %rsi"
      vm.asm "        call    find_in_frame"
    end
  end

  class IntVal < Node
    def codegen(vm)
      # TODO: large numbers
      val = (@value << 1) | 0x1
      vm.asm "        mov     $#{val}, %rax"
    end
  end

  class TrueVal < Node
    def codegen(vm)
      # TODO
    end
  end

  class FalseVal < Node
    def codegen(vm)
      # TODO
    end
  end

  class Definition < SpecializedNode
    def codegen(vm)
      varname = vm.addvarname(@name)
      @value.codegen(vm)
      # now the value is in %rax
      vm.argframe
      vm.asm "        mov     $#{varname}, %rsi"
      vm.asm "        mov     %rax, %rdx"
      vm.asm "        call    add_to_frame"
    end
  end

  class Lambda < SpecializedNode
    def codegen(vm)
      # Generating code for a lambda is a bit involved, so it's good to list
      # out the steps.
      #
      # 1. Immediately jmp ahead to after the generated code that will be
      #    called when the lambda is invoked. No need to call it now!
      #
      # 2. For each argument on the stack, generate code to add it to the
      #    current frame. Note that when the lambda is being invoked, the
      #    scm_fncall function has already created a new frame using the
      #    lambda's parent frame, so we'll be using that frame.
      #
      # 3. Generate code to run the lambda body.
      #
      # 4. Generate code to return from the function.
      #
      # 5. Now, we're at the point we jmp'ed to earlier. This code *will* be
      #    executed when the lambda is defined. Here, we'll generate code to
      #    create a wrapped lambda using the make_fn function. The result is
      #    stored in %rax, and we're done.
      #
      # One thing to watch out for: when scm_fncall invokes the lambda body, a
      # new frame has already been created and pushed onto the stack. Thus, we
      # have to reset the offset on the stack to the current frame in our VM.

      name = vm.genfnname
      endname = "#{name}_end"

      vm.asm "        jmp     #{endname}"
      vm.asm "#{name}:"

      # lambda body

      vm.fnstart

      params.each_with_index do |param, i|
        varname = vm.addvarname(param)
        offset = 24 + (i * 8)

        vm.argframe
        vm.asm "        mov     $#{varname}, %rsi"
        vm.asm "        mov     #{offset}(%rsp), %rdx"
        vm.asm "        call    add_to_frame"
      end

      body.codegen(vm)

      vm.fnend

      vm.asm "        ret"
      vm.asm "#{endname}:"

      # generate the actual lambda

      vm.argframe
      vm.asm "        mov     $#{name}, %rsi"
      vm.asm "        call    make_fn"
    end
  end

  class FunctionCall < SpecializedNode
    def codegen(vm)
      # First, generate code for each argument and push it onto the stack in
      # reverse order. This is because arguments that don't fit in the
      # registers are pushed onto the stack from right to left. Additionally,
      # after all the arguments have been pushed onto the stack, the ones that
      # fit in the registers can be popped off one by one before calling the
      # function.

      @args.reverse.each do |arg|
        arg.codegen(vm)
        vm.push("%rax")
      end

      # Evaluate the expression in order to get the wrapped function pointer.
      @func.codegen(vm)

      # TODO: align stack pointer

      vm.asm "        mov     %rax, %rdi"
      vm.asm "        call    scm_fncall"

      vm.popn(@args.size)
    end
  end

  class If < SpecializedNode
    def codegen(vm)
      # TODO
    end
  end
end

module VM
  class VM
    def initialize(filename)
      @filename = filename
      @statements = []
      @varnames = {}
      @quotedatoms = {}
      @frame_offsets = [0]
      @currfn = 0

      prologue
    end

    def prologue
      asm("# " + ("-" * 77))
      asm "# compiled.s"
      asm("# " + ("-" * 77))
      asm ""
      asm "        .global main"
      asm ""
      asm "        .text"
      asm "main:"
      asm "        call    create_atoms"
      asm "        call    new_root_frame"
      asm "        push    %rax"
    end

    def epilogue
      asm ""
      asm "        pop     %rax"
      asm "        mov     $0, %rax"
      asm "        ret"
      asm ""

      @varnames.each do |name, label|
        asm "#{label}:"
        asm "        .asciz  \"#{name}\""
      end

      asm ""
      asm "create_atoms:"
      asm "        call    init_atom_db"

      @quotedatoms.each do |name, label|
        asm "        mov     $#{label}_name, %rdi"
        asm "        call    create_atom"
      end

      asm "        ret"
      asm ""

      @quotedatoms.each do |name, label|
        asm "#{label}_name:"
        asm "        .asciz  \"#{name}\""
      end
    end

    def commit
      epilogue

      File.open(@filename, 'w') do |f|
        f.puts(@statements.join("\n"))
      end
    end

    def addvarname(name)
      name_str = name.name.to_s

      if @varnames.has_key?(name_str)
        @varnames[name_str]
      else
        label = "var_#{Digest::MD5.hexdigest(name_str)}"
        @varnames[name_str] = label
        label
      end
    end

    def addquotedatom(name)
      name_str = name.to_s

      if @quotedatoms.has_key?(name_str)
        @quotedatoms[name_str]
      else
        label = "atom_#{Digest::MD5.hexdigest(name_str)}"
        @quotedatoms[name_str] = label
        label
      end
    end

    def genfnname
      name = "fn_#{@currfn}"
      @currfn += 1
      name
    end

    def push(reg = nil)
      if reg.nil?
        asm "        sub     $8, %rsp"
      else
        asm "        push    #{reg}"
      end

      last = @frame_offsets.pop
      @frame_offsets.push(last + 8)
    end

    def pop(reg = nil)
      if reg.nil?
        asm "        add     $8, %rsp"
      else
        asm "        pop     #{reg}"
      end

      last = @frame_offsets.pop
      @frame_offsets.push(last - 8)
    end

    def popn(n)
      asm "        add     $#{n * 8}, %rsp"

      last = @frame_offsets.pop
      @frame_offsets.push(last - n * 8)
    end

    def argframe
      offset = @frame_offsets.last
      asm "        add     $#{offset}, %rsp" if offset > 0
      asm "        mov     (%rsp), %rdi"
      asm "        sub     $#{offset}, %rsp" if offset > 0
    end

    def fnstart
      @frame_offsets.push(8)
    end

    def fnend
      @frame_offsets.pop
    end

    def asm(statement)
      @statements << statement
    end

    #def newframe(parent = nil)
    #  # push parent to stack
    #  # call newframe function
    #  # push address of new frame onto stack
    #end

    #def lookupvar(frame, var)
    #  # push frame to stack
    #  # push var.name to stack
    #  # call lookup function
    #end

    #def addvar(var)
    #  # push frame to stack
    #  # push var.name to stack
    #  # push value to stack
    #  # call addvar function
    #end

    #def fncall(fn, *args)
    #  # check arity vs. args
    #  # create new frame with fn's parent frame as parent
    #  # add args to frame
    #  # push frame to stack
    #  # jmp to fn's address
    #end
  end
end

## MAIN ########################################################################

# TODO: check arguments (possibly read from stdin)
# TODO: allow outputting to a different intermediate .s file

inp = File.read(ARGV[0])
parsed = Scheme.new.parse(inp)
ast = AST.construct_from_parse_tree(parsed)
require 'pp'
#pp ast
ast.codegen("compiled.s")
exit

# vim: ts=2 sw=2 :
