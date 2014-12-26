#!/usr/bin/env ruby

require 'parslet'
require 'singleton'
require 'digest/md5'
require 'fileutils'

## INITIAL PARSER ##############################################################

class Scheme < Parslet::Parser
  rule(:space)     { match('\s').repeat(1) }
  rule(:space?)    { space.maybe }

  rule(:int)       { (match('[+-]').maybe >> match('[0-9]').repeat(1)).as(:int) }
  rule(:bool)      { str('#t').as(:true) | str('#f').as(:false) }

  rule(:simplevar) {
    match('[A-Za-z\-\+\*\/_=]') >>
      match('[A-Za-z\-\+\*\/_\?0-9=]').repeat(1).maybe
  }

  rule(:var)       {
    ((simplevar >> str(':') >> simplevar) | simplevar).as(:var)
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

  rule(:dotlist)   {
    str('(') >>
      space? >>
      # Make sure to use .repeat(0, 1) instead of .maybe, since we always want
      # a list in the parse tree, regardless of whether it matched one sub-
      # expression or more.
      (
        (expr >> (space >> expr).repeat(1).maybe).repeat(0, 1) >>
        space >> str('.') >> space >>
        expr
      ).as(:dotlist) >>
      space? >>
      str(')')
  }

  rule(:quoted)     {
    str('\'') >> (atom | dotlist | list).as(:quoted)
  }

  rule(:string)     {
    str('"') >>
      (
        (str('\\') >> any) |
        (str('"').absent? >> any)
      ).repeat(1).maybe.as(:string) >>
      str('"')
  }

  rule(:comment)    {
    str(';') >> match('[^\n]').repeat(1).maybe.as(:comment) >> str("\n")
  }

  rule(:atom)      { int | bool | var }
  rule(:expr)      { atom | list | dotlist | quoted | string | comment }
  rule(:expr_list) { ((space? >> expr).repeat(1).maybe >> space?).as(:exprs) }

  root(:expr_list)
end

## AST GENERATION ##############################################################

module AST
  class ParseException < Exception; end

  class SchemeModule
    MODULE_TYPE_SCM = :scheme

    def initialize(filename,
                   module_name,
                   is_main,
                   module_type,
                   ast,
                   child_modules)
      @filename = filename
      @module_name = module_name
      @is_main = is_main
      @module_type = module_type

      @ast = ast
      @child_modules = child_modules
    end

    attr :filename,
         :module_name,
         :is_main,
         :module_type

    attr :ast,
         :child_modules
  end

  module CommonTransformations
    def with_hoisted_definitions(statements)
      defines = statements.find_all { |s| s.is_a?(Definition) }
      others  = statements.reject   { |s| s.is_a?(Definition) }

      defines + others
    end
  end

  class Program
    include CommonTransformations

    def initialize(*statements)
      @statements = statements
    end

    def static_transformed
      @statements, module_requires = separated_module_requires(@statements)
      @statements, module_exports = separated_module_exports(@statements)
      @statements = @statements.map(&:static_transformed)
      @statements = with_hoisted_definitions(@statements)

      @module_requires = gathered_requires(module_requires)
      @module_exports = gathered_exports(module_exports)

      self
    end

    def recursive_require_modules
      @module_requires.map { |name|
        gather_asts(
          "#{name}.scm",
          name,
          false,
          SchemeModule::MODULE_TYPE_SCM
        )
      }.uniq { |m| m.filename }
    end

    def separated_module_requires(statements)
      separated_statements_of_type(:require)
    end

    def separated_module_exports(statements)
      separated_statements_of_type(:'module-export')
    end

    def separated_statements_of_type(type)
      statements.partition { |statement|
        statement.is_a?(NestedNode) &&
          !statement.children.empty? &&
          statement.children[0].is_a?(Var) &&
          statement.children[0].name == type
      }.reverse
    end

    def gathered_requires(module_exports)
      module_exports.map { |exp|
        name = exp.children[1]

        if name.is_a?(Var)
          basedir = compiler_dir
          absolute_path_for_module(name.name.to_s, basedir)
        elsif name.is_a?(String)
          basedir = File.dirname(self.filename)
          absolute_path_for_module(name.contents, basedir)
        else
          raise ParseException.new("require with invalid name: #{name}")
        end
      }.flatten
    end

    def gathered_exports(module_exports)
      module_exports.map { |exp|
        exp
          .children
          .drop(1)
          .map(&:name)
      }.flatten
    end

    def to_s
      @statements.join("\n")
    end

    attr :statements
    attr_accessor :filename
  end

  class Node
    def has_children?
      false
    end

    def static_transformed
      self
    end

    def specialized_quoted
      self
    end
  end

  class Comment < Node
    def initialize(text)
      @text = text
    end

    def to_s
      "\033[32m;#{text}\033[0m"
    end

    attr :text
  end

  class Quoted < Node
    def initialize(value)
      @value = value
    end

    def static_transformed
      specialized_quoted
    end

    def specialized_quoted
      @value.specialized_quoted
    end

    def to_s
      "'#{value}"
    end

    attr :value
  end

  class String < Node
    def initialize(contents)
      @contents = contents
    end

    def to_s
      "\033[1;32m\"#{@contents}\"\033[0m"
    end

    attr :contents
  end

  class QuotedAtom < Node
    def initialize(name)
      @name = name
    end

    def to_s
      "\033[1;32m'#{name}\033[0m"
    end

    attr :name
  end

  class QuotedList < Node
    def initialize(is_nil_terminated, children)
      @is_nil_terminated = is_nil_terminated
      @children = children
    end

    def has_children?
      true
    end

    def to_s
      children_list = @children.map(&:to_s)

      if is_nil_terminated
        last_child = children_list.pop
        children_list << "."
        children_list << last_child
      end

      children_str = children_list.join(" ")
      "'(#{children_str})"
    end

    attr :is_nil_terminated, :children
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

    def specialized_quoted
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

    def specialized_quoted
      self
    end

    attr :value
  end

  class TrueVal < Node
    include Singleton

    def to_s
      "#t"
    end

    def specialized_quoted
      self
    end
  end

  class FalseVal < Node
    include Singleton

    def to_s
      "#f"
    end

    def specialized_quoted
      self
    end
  end

  class SpecializedNode < Node
    def has_children?
      true
    end
  end

  class NestedNode < Node
    def initialize(*children)
      @children = children
    end

    def has_children?
      true
    end

    def static_transformed
      if @children.empty?
        self # TODO
      else
        first = @children.first
        first_is_var = first.is_a?(Var)

        filtered_children = @children.reject { |child| child.is_a?(Comment) }

        specialized =
          if first_is_var
            case first.name
            # If we find a require or a module-export, it means it was not
            # removed by the relevant pre-processing step, which only looks at
            # the top-level statements. Thus, this statement is nested, which
            # is illegal.
            when :require
              raise ParseException.new("require found in nested scope")
            when :'module-export'
              raise ParseException.new("module-export found in nested scope")
            when :define
              Definition.new(*filtered_children)
            when :lambda
              Lambda.new(*filtered_children)
            when :if
              If.new(*filtered_children)
            when :cond
              Cond.new(*filtered_children)
            when :let
              Let.new(:bare, *filtered_children)
            when :'let*'
              Let.new(:star, *filtered_children)
            else
              as_function_call
            end
          else
            as_function_call
          end

        specialized.static_transformed
      end
    end

    def as_function_call
      FunctionCall.new(*@children)
    end

    def specialized_quoted
      if @children.empty?
        Nil.instance
      else
        quoted_children = @children.map(&:specialized_quoted)
        QuotedList.new(false, quoted_children)
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

  class DottedList < NestedNode
    def initialize(*children)
      @children = children
    end

    def has_children?
      true
    end

    def static_transformed
      # A dotted list never appears on its own. There are two cases where a
      # dotted list are encountered:
      #
      #  - In a special form, such as in the argument list of a function
      #    definition. These situations are handled specially by the parent
      #    node's static transformation.
      #
      #  - In a quoted list, which is taken care of by "specialized_quoted".
      raise ParseException.new("invalid dotted list")
    end

    def specialized_quoted
      quoted_children = @children.map(&:specialized_quoted)
      QuotedList.new(true, quoted_children)
    end

    def internal_color
      "1;32"
    end

    def to_s
      children_list = @children.map { |child|
        if child.has_children?
          child.to_s
        else
          "\033[#{self.internal_color}m#{child}\033[0m"
        end
      }

      last_child = children_list.pop
      children_list << "."
      children_list << last_child

      children_str = children_list.join(" ")
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
      @children = children
    end

    def static_transformed
      if @children[1].is_a?(Var)
        unless @children.size == 3
          raise ParseException.new(
            "Variable definition must have 3 children, " +
              "got #{@children.size}: " +
              "(#{@children.join(" ")})")
        end

        @name = @children[1]
        @value = @children[2].static_transformed
      elsif @children[1].is_a?(NestedNode)
        unless @children.size >= 3
          raise ParseException.new(
            "Variable definition must have at least 3 children, " +
              "got #{@children.size}: " +
              "(#{@children.join(" ")})")
        end

        signature = @children[1].children

        if signature.empty?
          raise ParseException.new("Definition must have a name")
        end

        @name = signature[0]

        params_array = signature.drop(1)
        if @children[1].is_a?(DottedList)
          params = DottedList.new(*params_array)
        else
          params = NestedNode.new(*params_array)
        end

        @value = Lambda.new(
          :lambda,
          params,
          *@children.drop(2)
        ).static_transformed
      else
        raise ParseException.new(
          "Definition name must be a var or list, got: " +
            "#{@children[1]} of type #{@children[1].class}")
      end

      self
    end

    def to_s
      "(\033[#{self.internal_color}mdefine\033[0m " +
        "\033[1m#{@name}\033[0m #{@value})"
    end

    def internal_color
      "1;31"
    end

    attr :name, :value, :children
  end

  class Lambda < SpecializedNode
    include CommonTransformations

    def initialize(*children)
      @children = children
    end

    def static_transformed
      unless @children.size >= 3
        raise ParseException.new(
          "Lambda must have at least 3 children, got #{@children.size}: " +
            "(#{@children.join(" ")})")
      end

      unless @children[1].is_a?(Var) or @children[1].has_children?
        raise ParseException.new(
          "Lambda parameter list must be a var or list, got: " +
            "#{@children[1]} of type #{@children[1].class}")
      end

      if @children[1].has_children?
        @children[1].children.each do |param|
          unless param.is_a?(Var)
            raise ParseException.new(
              "Lambda parameter must be a var, got: " +
                "#{param} of type #{param.class}")
          end
        end
      end

      if @children[1].is_a?(DottedList)
        @params = @children[1].children.take(@children[1].children.size - 1)
        @rest_param = @children[1].children.last
        @is_vararg = true
      elsif @children[1].is_a?(Var)
        @params = []
        @rest_param = @children[1]
        @is_vararg = true
      else
        @params = @children[1].children
        @rest_param = nil
        @is_vararg = false
      end

      @body_statements = @children[2, children.length - 1]
        .map(&:static_transformed)
      @body_statements = with_hoisted_definitions(@body_statements)

      self
    end

    def to_s
      "(\033[#{self.internal_color}mlambda\033[0m " +
        "(\033[1m#{@params.join(" ")}\033[0m)\n" +
        @body_statements.map(&:to_s).map { |s| "\t#{s}" }.join("\n") + ")"
    end

    def internal_color
      "1;33"
    end

    attr :params, :rest_param, :is_vararg, :body_statements, :children
  end

  class FunctionCall < SpecializedNode
    def initialize(*children)
      @func = children.first
      @args = children[1, children.size - 1]
    end

    def static_transformed
      @func = @func.static_transformed
      @args = @args.map(&:static_transformed)

      self
    end

    def to_s
      args_str = @args.map(&:to_s).join(" ")

      if not args_str.empty?
        args_str = " #{args_str}"
      end

      "(\033[#{self.internal_color}m#{@func}\033[0m#{args_str})"
    end

    def internal_color
      "1;34"
    end

    attr :func, :args
  end

  class If < SpecializedNode
    def initialize(*children)
      @children = children
    end

    def static_transformed
      unless @children.size == 4
        raise ParseException.new(
          "If must have 4 children, got #{@children.size}: " +
            "(#{@children.join(" ")})")
      end

      @cond = @children[1].static_transformed
      @true_expr = @children[2].static_transformed
      @false_expr = @children[3].static_transformed

      self
    end

    def to_s
      "(\033[#{self.internal_color}mif\033[0m " +
        "#{@cond} #{@true_expr} #{@false_expr})"
    end

    def internal_color
      "1;35"
    end

    attr :cond, :true_expr, :false_expr, :children
  end

  class Cond < SpecializedNode
    def initialize(*children)
      @children = children
    end

    def static_transformed
      unless @children.size >= 2
        raise ParseException.new(
          "Cond must have at least two children, got #{@children.size}: " +
            "(#{@children.join(" ")})")
      end

      conditions = @children.drop(1)
      @conditions = conditions.each_with_index.map { |cond, index|
        unless cond.is_a?(NestedNode)
          raise ParseException.new(
            "Condition in Cond expression must be a list, got: #{cond}")
        end

        cond_children = cond.children.reject { |child| child.is_a?(Comment) }

        unless cond_children.size >= 2
          raise ParseException.new(
            "Condition in Cond expression must be have a test " +
              "and at least one expression, got: #{cond_children}")
        end

        test = cond_children[0]
        expressions = cond_children
          .drop(1)
          .map(&:static_transformed)
        if test.is_a?(Var) && test.name == :else
          unless index == conditions.size - 1
            raise ParseException.new(
              "Else condition must be be last condition")
          end

          CondElse.new(expressions)
        else
          CondCondition.new(
            test.static_transformed,
            expressions
          )
        end
      }

      self
    end

    attr :conditions
  end

  class CondCondition < SpecializedNode
    def initialize(test, expressions)
      @test = test
      @expressions = expressions
    end

    attr :test, :expressions
  end

  class CondElse < SpecializedNode
    def initialize(expressions)
      @expressions = expressions
    end

    attr :expressions
  end

  class Let < SpecializedNode
    include CommonTransformations

    def initialize(let_type, *children)
      @let_type = let_type
      @children = children
    end

    def static_transformed
      unless @children.size >= 3
        raise ParseException.new(
          "Let must have at least three children, got #{@children.size}: " +
            "(#{@children.join(" ")})")
      end

      unless @children[1].is_a?(NestedNode)
        raise ParseException.new(
          "Let bindings must be a list, got: (#{@children[1]})")
      end

      @bindings = @children[1].children.map { |binding|
        unless binding.is_a?(NestedNode)
          raise ParseException.new(
            "Let binding must be a list, got: (#{binding})")
        end

        unless binding.children.size == 2
          raise ParseException.new(
            "Let binding must have two children, got " +
              "#{binding.children.size}: (#{binding.children.join(" ")})")
        end

        unless binding.children[0].is_a?(Var)
          raise ParseException.new(
            "Let binding must bind to a variable, got: " +
              "(#{binding.children[0]})")
        end

        name = binding.children[0]
        value = binding.children[1].static_transformed
        LetBinding.new(name, value)
      }

      @expressions = @children.drop(2).map(&:static_transformed)
      @expressions = with_hoisted_definitions(@expressions)

      self
    end

    attr :let_type, :bindings, :expressions
  end

  class LetBinding < SpecializedNode
    def initialize(name, value)
      @name = name
      @value = value
    end

    attr :name, :value
  end

  class ASTTransform < Parslet::Transform
    rule(var: simple(:name))  { Var.new(name.to_s.intern) }
    rule(int: simple(:value)) { IntVal.new(value.to_i) }
    rule(true: simple(:x))    { TrueVal.instance }
    rule(false: simple(:x))   { FalseVal.instance }

    rule(list: sequence(:x))  { NestedNode.new(*x) }
    rule(dotlist: sequence(:x))  { DottedList.new(*x) }

    rule(quoted: simple(:q))  { Quoted.new(q) }
    rule(string: simple(:s))  { String.new(s) }

    rule(comment: simple(:c)) { Comment.new(c) }
    rule(exprs: sequence(:x)) { Program.new(*x) }
  end

  def AST.construct_from_parse_tree(tree, filename)
    ast = ASTTransform.new.apply(tree)
    ast.filename = filename
    ast = ast.static_transformed
  end
end

## NATIVE CODE GENERATION ######################################################

module AST
  class SchemeModule
    class CompileException < Exception; end

    def codegen
      case module_type
      when MODULE_TYPE_SCM
        ast.codegen(filename, module_name, is_main)
      else
        raise CompileException.new("unknown module type: #{module_type}")
      end
    end
  end

  class Program
    def codegen(filename, symbol_prefix, is_main)
      vm = VM::VM.new(
        filename,
        symbol_prefix,
        is_main,
        @module_requires,
        @module_exports
      )

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
      vm.movlabelreg "#{name}_name", "%rdi"
      vm.with_aligned_stack do
        vm.call "get_atom"
      end
    end
  end

  class QuotedList < Node
    def codegen(vm)
      vm.asm "        mov     $0, %rsi" if !is_nil_terminated

      @children.reverse.each_with_index do |child, index|
        if !is_nil_terminated or index > 0
          vm.push("%rsi")
          child.codegen(vm)
          vm.pop("%rsi")

          vm.asm "        mov     %rax, %rdi"
          vm.with_aligned_stack do
            vm.call "make_cons"
          end
        else
          child.codegen(vm)
        end

        unless index == @children.size - 1
          vm.asm "        mov     %rax, %rsi"
        end
      end
    end
  end

  class String < Node
    def codegen(vm)
      name = vm.addstring(@contents)
      vm.movlabelreg "#{name}_contents", "%rdi"

      vm.with_aligned_stack do
        vm.call "make_string_with_contents"
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
      if name.to_s.include?(":")
        module_prefix, internal_name = name.to_s.split(":")
        varname = vm.addvarname(Var.new(internal_name))
        vm.movlabelreg varname, "%rdi"

        vm.with_aligned_stack do
          getter_name = VM::VM.getter_name_for_module(module_prefix)
          vm.call getter_name
        end
      else
        varname = vm.addvarname(self)

        vm.argframe
        vm.movlabelreg varname, "%rsi"
        vm.with_aligned_stack do
          vm.call "find_in_frame"
        end
      end
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
      vm.asm "        mov     $2, %rax"
    end
  end

  class FalseVal < Node
    def codegen(vm)
      vm.asm "        mov     $4, %rax"
    end
  end

  class Definition < SpecializedNode
    def codegen(vm)
      varname = vm.addvarname(@name)
      @value.codegen(vm)
      # now the value is in %rax
      vm.argframe
      vm.movlabelreg varname, "%rsi"
      vm.asm "        mov     %rax, %rdx"
      vm.with_aligned_stack do
        vm.call "add_to_frame"
      end
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

      if is_vararg
        # Start by the gathering up the last argument. The steps are as
        # follows:
        #
        # 1. Take the number of arguments passed to this lambda and subtract
        #    the number of non-vararg arguments.
        #
        # 2. Use gather_varargs to put all the varargs (which are at the "top"
        #    of the stack after accounting for return pointers, etc.) into
        #    %rax.
        #
        # 3. Add the resulting list to the frame.
        #
        # 4. Restore the number of arguments in %rsi by adding back in the
        #    number of non-vararg arguments (this has to be done a little later
        #    actually, because we still need the number gathered arguments).
        rest_varname = vm.addvarname(rest_param)

        vm.asm "        sub     $#{params.size}, %rsi"

        vm.with_aligned_stack do
          vm.call "gather_varargs"
        end

        vm.push("%rsi")

        vm.argframe
        vm.movlabelreg rest_varname, "%rsi"
        vm.asm "        mov     %rax, %rdx"
        vm.with_aligned_stack do
          vm.call "add_to_frame"
        end

        vm.pop("%rsi")
      end

      # Now set up the pointer to the first non-vararg argument:
      vm.asm "        mov     %rsp, %r8"
      vm.asm "        add     $32, %r8"

      if is_vararg
        vm.asm "        mov     %rsi, %rax"
        vm.asm "        shlq    $3, %rax"   # multiply by 8
        vm.asm "        add     %rax, %r8"

        # Now we can actually restore the number of total arguments passed to
        # the lambda, should be we need it again.
        vm.asm "        add     $#{params.size}, %rsi"
      end

      params.reverse.each do |param|
        varname = vm.addvarname(param)

        vm.argframe
        vm.movlabelreg varname, "%rsi"
        vm.asm "        mov     (%r8), %rdx"
        vm.push("%r8")
        vm.with_aligned_stack do
          vm.call "add_to_frame"
        end
        vm.pop("%r8")
        vm.asm "        add     $8, %r8"
      end

      body_statements.each do |statement|
        statement.codegen(vm)
      end

      vm.fnend

      vm.asm "        ret"
      vm.asm "#{endname}:"

      # generate the actual lambda

      vm.argframe
      vm.movlabelreg name, "%rsi"
      vm.with_aligned_stack do
        vm.call "make_fn"
      end
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
      #
      # Certain arguments should be ignored completely, namely comments, since
      # they do not produce any values in %rax that need to be pushed onto the
      # stack. They should not contribute to the number of arguments either.

      filtered_args = @args.reject { |arg| arg.is_a?(Comment) }

      vm.with_aligned_stack(filtered_args.size) do
        filtered_args.each do |arg|
          arg.codegen(vm)
          vm.push("%rax")
        end

        # Evaluate the expression in order to get the wrapped function pointer.
        @func.codegen(vm)

        vm.asm "        mov     %rax, %rdi"
        vm.asm "        mov     $#{filtered_args.size}, %rsi"
        vm.call "scm_fncall"

        vm.popn(filtered_args.size)
      end
    end
  end

  class If < SpecializedNode
    def codegen(vm)
      label = vm.gencondname

      @cond.codegen(vm)

      vm.asm "        cmp     $4, %rax"
      vm.asm "        je      #{label}_false"

      @true_expr.codegen(vm)

      vm.asm "        jmp     #{label}_end"
      vm.asm "#{label}_false:"

      @false_expr.codegen(vm)

      vm.asm "#{label}_end:"
    end
  end

  class Cond < SpecializedNode
    def codegen(vm)
      label = vm.gencondname

      condition_labels = @conditions.each_with_index.map { |cond, index|
        if cond.is_a?(CondCondition)
          if index.zero?
            "#{label}_begin"
          else
            "#{label}_#{index}"
          end
        elsif cond.is_a?(CondElse)
          "#{label}_else"
        end
      }

      condition_labels.push("#{label}_end")

      @conditions.each_with_index do |cond, index|
        vm.asm "#{condition_labels[index]}:"

        if cond.is_a?(CondCondition)
          cond.test.codegen(vm)

          vm.asm "        cmp     $4, %rax"
          vm.asm "        je      #{condition_labels[index + 1]}"

          cond.expressions.each do |exp|
            exp.codegen(vm)
          end

          if index != @conditions.size - 1
            vm.asm "        jmp     #{condition_labels.last}"
          end
        elsif cond.is_a?(CondElse)
          cond.expressions.each do |exp|
            exp.codegen(vm)
          end
        end
      end

      vm.asm "#{condition_labels.last}:"
    end
  end

  class Let < SpecializedNode
    def codegen(vm)
      vm.argframe
      vm.with_aligned_stack do
        vm.call "new_frame_with_parent"
      end

      # Note that we *won't* be pushing a return address. with_aligned_stack
      # assumes that we will, so after that fictitious return address is
      # pushed, we want to end up back at the same point we are at now.
      vm.with_aligned_stack(-1) do
        vm.newframe
        vm.push("%rax")

        case let_type
        when :bare then add_bindings_bare(vm)
        when :star then add_bindings_star(vm)
        end

        expressions.each do |expression|
          expression.codegen(vm)
        end

        vm.pop
        vm.remframe
      end
    end

    def add_bindings_bare(vm)
      bindings.each do |binding|
        binding.value.codegen(vm)
        vm.push("%rax")
      end

      bindings.reverse.each do |binding|
        varname = vm.addvarname(binding.name)

        vm.argframe
        vm.movlabelreg varname, "%rsi"
        vm.pop("%rdx")
        vm.with_aligned_stack do
          vm.call "add_to_frame"
        end
      end
    end

    def add_bindings_star(vm)
      bindings.each do |binding|
        varname = vm.addvarname(binding.name)

        binding.value.codegen(vm)

        vm.argframe
        vm.movlabelreg varname, "%rsi"
        vm.asm "        mov     %rax, %rdx"
        vm.with_aligned_stack do
          vm.call "add_to_frame"
        end
      end
    end
  end
end

module VM
  class VM
    def initialize(filename,
                   module_name,
                   is_main,
                   module_requires,
                   module_exports)
      # VM properties
      @filename = filename
      @module_name = module_name
      @symbol_prefix = VM.symbol_prefix_from_module_name(module_name)
      @is_main = is_main
      @module_requires = module_requires
      @module_exports = module_exports

      @statements = []

      # names
      @varnames = {}
      @quotedatoms = {}
      @stringnames = {}

      @frame_offsets = [0]

      # counters
      @currfn = 0
      @currcond = 0

      prologue
    end

    def self.getter_name_for_module(module_name)
      symbol_prefix = symbol_prefix_from_module_name(module_name)
      "module_get_#{symbol_prefix}"
    end

    def prologue
      main_name =
        if @is_main
          "main"
        else
          "init_#{@symbol_prefix}"
        end

      asm("# " + ("-" * 77))
      asm "# compiled.S"
      asm("# " + ("-" * 77))
      asm ""

      asm "# MACROS: needed for cross-platform compatibility"

      asm "#if defined(__WIN32__) || defined(__APPLE__)"
      asm "# define cdecl(s) _##s"
      asm "#else"
      asm "# define cdecl(s) s"
      asm "#endif"

      asm ""

      asm "#if defined(__APPLE__)"
      asm "# define movlabelreg(src, dst) movq    src##\@GOTPCREL(%rip), dst"
      asm "#else"
      asm "# define movlabelreg(src, dst) mov     $##src##, dst"
      asm "#endif"

      asm ""

      asm "#if defined(__APPLE__)"
      asm "# define movlabelvalreg(src, dst) \\"
      asm "    movq    src##\@GOTPCREL(%rip), %r8 ;\\"
      asm "    movq    (%r8), dst"
      asm "#else"
      asm "# define movlabelvalreg(src, dst) movq    src##, dst"
      asm "#endif"

      asm ""

      asm "#if defined(__APPLE__)"
      asm "# define movreglabel(src, dst) \\"
      asm "    movq    dst##\@GOTPCREL(%rip), %r8 ;\\"
      asm "    movq    src, (%r8)"
      asm "#else"
      asm "# define movreglabel(src, dst) movq    src, dst"
      asm "#endif"

      asm ""
      asm "        .global cdecl(#{main_name})"
      asm ""
      asm "        .text"
      asm "cdecl(#{main_name}):"
      asm "        movlabelvalreg(#{@symbol_prefix}_is_initialized, %rax)"
      asm "        cmpq    $0, %rax"
      asm "        je      #{main_name}_do_init"
      asm "        ret"
      asm "#{main_name}_do_init:"
      asm "        movreglabel($1, #{@symbol_prefix}_is_initialized)"
      push
      asm "        call    #{@symbol_prefix}_create_atoms"
      call "new_root_frame"
      asm "        movreglabel(%rax, #{@symbol_prefix}_root_frame)"
      pop
      push("%rax")

      asm ""
      @module_requires.each do |name|
        call "init_#{VM.symbol_prefix_from_module_name(name)}"
      end
    end

    def epilogue
      asm ""
      asm "        add     $8, %rsp"
      asm "        mov     $0, %rax"
      asm "        ret"
      asm ""

      asm "#{@symbol_prefix}_create_atoms:"
      asm "        sub     $8, %rsp"

      call "init_atom_db"

      @quotedatoms.each do |name, label|
        movlabelreg "#{label}_name", "%rdi"
        call "create_atom"
      end
      asm ""

      asm "        add     $8, %rsp"
      asm "        ret"
      asm ""

      module_name = File.basename(@module_name)
      getter_name = VM.getter_name_for_module(module_name)

      asm "        .global cdecl(#{getter_name})"
      asm "cdecl(#{getter_name}):"
      asm "        sub     $8, %rsp"

      asm "        mov     %rdi, %rsi"
      asm "        movlabelreg(#{@symbol_prefix}_root_frame, %rdi)"
      asm "        mov     (%rdi), %rdi"

      call "find_in_frame"
      asm "        add     $8, %rsp"
      asm "        ret"
      asm ""

      asm "        .data"
      asm ""

      @varnames.each do |name, label|
        asm "#{label}:"
        asm "        .asciz  \"#{name}\""
      end

      asm "        ret"
      asm ""

      @quotedatoms.each do |name, label|
        asm "#{label}_name:"
        asm "        .asciz  \"#{name}\""
      end
      asm ""

      @stringnames.each do |contents, label|
        asm "#{label}_contents:"
        asm "        .asciz  \"#{contents}\""
      end

      asm ""
      asm "#{@symbol_prefix}_root_frame:"
      asm "        .quad 0"
      asm "#{@symbol_prefix}_is_initialized:"
      asm "        .quad 0"
    end

    def commit
      epilogue

      output_filename = VM.output_filename(@module_name)
      File.open(output_filename, 'w') do |f|
        f.puts(@statements.join("\n"))
      end
    end

    def addvarname(name)
      name_str = name.name.to_s

      if @varnames.has_key?(name_str)
        @varnames[name_str]
      else
        label = "#{@symbol_prefix}_var_#{Digest::MD5.hexdigest(name_str)}"
        @varnames[name_str] = label
        label
      end
    end

    def addquotedatom(name)
      name_str = name.to_s

      if @quotedatoms.has_key?(name_str)
        @quotedatoms[name_str]
      else
        label = "#{@symbol_prefix}_atom_#{Digest::MD5.hexdigest(name_str)}"
        @quotedatoms[name_str] = label
        label
      end
    end

    def addstring(contents)
      contents_str = contents.to_s

      if @stringnames.has_key?(contents_str)
        @stringnames[contents_str]
      else
        label =
          "#{@symbol_prefix}_string_#{Digest::MD5.hexdigest(contents_str)}"
        @stringnames[contents_str] = label
        label
      end
    end

    def genfnname
      name = "#{@symbol_prefix}_fn_#{@currfn}"
      @currfn += 1
      name
    end

    def gencondname
      name = "#{@symbol_prefix}_cond_#{@currcond}"
      @currcond += 1
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
      offset = lstoffset - 8
      if offset > 0
        asm "        mov     #{offset}(%rsp), %rdi"
      else
        asm "        mov     (%rsp), %rdi"
      end
    end

    def newframe
      @frame_offsets.push(0)
    end

    def remframe
      @frame_offsets.pop
    end

    def fnstart
      # The current frame is there on the stack, right before the return
      # address, but it's useful to have it available right at the top of the
      # current frame offset so that vm.argframe can continue to work.

      # This is definitely not the most efficient way to handle this, since
      # this duplicates the frame pointer right before and after the return
      # address. One option might be to separate the stack frame offset from
      # the environment frame offset so vm.argframe can continue to work
      # independently of the stack alignment.
      newframe

      asm "        mov     8(%rsp), %rax"
      push("%rax")
    end

    def fnend
      # Remove the duplicated current frame pointer.
      pop
      remframe
    end

    def lstoffset
      @frame_offsets.last
    end

    def call(fnname)
      asm "        call    cdecl(#{fnname})"
    end

    def movlabelreg(src, dst)
      asm "        movlabelreg(#{src}, #{dst})"
    end

    def with_aligned_stack(num_args_to_push = 0)
      # When a function call is made, the stack needs to be aligned to a 16-
      # byte boundary. There's a catch, in that the call needs to be made with
      # the stack alignment offset by 8 bytes, because the return address will
      # be pushed to the top of the stack. The stack has to be aligned *after*
      # the return address is pushed!

      # Another issue, when calling a "user-defined" function, is that we want
      # the arguments to be as close to the top of the stack as possible, so
      # that the scm_fncall function (and other functions it calls into) know
      # where its arguments are, regardless of how the stack had to be aligned.
      # So, we'll align the stack *before* pushing the arguments. Whether or
      # not we need to align depends on the current stack alignment and the
      # number arguments we'll be pushing.
      is_stack_aligned = (lstoffset / 8) % 2 == 1
      has_even_num_args = num_args_to_push % 2 == 0

      should_not_align_stack =
        ( is_stack_aligned &&  has_even_num_args) ||
        (!is_stack_aligned && !has_even_num_args)
      should_align_stack = !should_not_align_stack

      if should_align_stack
        asm("# aligning stack")
        push
      end

      yield

      if should_align_stack
        pop
        asm("# unaligning stack")
      end
    end

    def asm(statement)
      @statements << statement
    end

    private

    def self.normalized_name_for_name(name)
      name = name.to_s
      normalized_name = name.gsub(/[^a-z_]+/i, '_')
      normalized_name = 'empty' if normalized_name.empty?
      normalized_name
    end

    def self.symbol_prefix_from_module_name(module_name)
      "m_#{Digest::MD5.hexdigest(module_name.to_s)}"
    end

    def self.output_filename(module_name)
      "build/#{self.symbol_prefix_from_module_name(module_name)}.S"
    end

  end
end

## FILE PROCESSING #############################################################

BUILD_DIR = 'build'
OUT_EXE = 'main'

def create_fresh_build_env(out_exe_name, build_dir_name)
  if File.file?(out_exe_name)
    FileUtils.rm(out_exe_name)
  end

  if File.directory?(build_dir_name)
    FileUtils.rm_r(build_dir_name)
  end

  FileUtils.mkdir_p(build_dir_name)
end

def compile_file(filename)
  module_tree = gather_asts(filename, 'main', true)

  # Linearize the dependency DAG
  to_codegen = []
  tree_queue = [module_tree]
  until tree_queue.empty?
    tree = tree_queue.shift
    tree_queue += tree.child_modules
    to_codegen << tree
  end

  to_codegen.each(&:codegen)
end

def gather_asts(filename,
                module_name,
                is_main,
                module_type = AST::SchemeModule::MODULE_TYPE_SCM)
  file = File.read(filename)
  parsed = Scheme.new.parse(file)
  ast = AST.construct_from_parse_tree(parsed, filename)

  child_modules = ast.recursive_require_modules

  AST::SchemeModule.new(
    filename,
    module_name,
    is_main,
    module_type,
    ast,
    child_modules
  )
end

def run_gcc(out_filename)
  system "gcc -g build/*.S stdlib.c stdlib.S hashmap.c -o #{out_filename}"
end

def compiler_dir
  File.dirname(__FILE__)
end

def absolute_path_for_module(relative_path, basedir = nil)
  File.absolute_path(relative_path, basedir)
end

## MAIN ########################################################################

# TODO: check arguments (possibly read from stdin)

create_fresh_build_env(OUT_EXE, BUILD_DIR)
compile_file(absolute_path_for_module(ARGV[0]))
run_gcc(OUT_EXE)

# vim: ts=2 sw=2 :
