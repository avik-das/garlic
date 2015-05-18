require 'parslet'

module CParser
  class CExport < Struct.new(:name, :arity, :is_vararg)
    def to_s
      "{\"#{name}\", #{arity}, #{is_vararg ? 1 : 0}}"
    end
  end

  class CExports < Parslet::Parser
    rule(:space)   { match('\s').repeat(1) }
    rule(:space?)  { space.maybe }

    rule(:filler)  { match('.').repeat(1).maybe }

    rule(:id)      {
      match('[A-Za-z_]') >>
        match('[A-Za-z_0-9]').repeat(1).maybe
    }

    rule(:number)  {
      match('[0-9]').repeat(1)
    }

    rule(:string)  {
      str('"') >>
        (
          (str('\\') >> any) |
          (str('"').absent? >> any)
        ).repeat(1).maybe.as(:string) >>
        str('"')
    }

    rule(:exports) {
      space? >>
        str('garlic_native_export_t') >>
        space >>
        id >>
        str('[]') >>
        space? >>
        str('=') >>
        space? >>
        str('{') >>
        space? >>

        (
          str('{') >>
            space? >>

            string.as(:fnname) >>
            space? >>
            str(',') >>
            space? >>
            id >>
            space? >>
            str(',') >>
            space? >>
            number.as(:arity) >>
            space? >>

            (
              str(',') >>
              space? >>
              number.as(:is_vararg) >>
              space?
            ).maybe >>

            str('}') >>
            space? >>
            str(',') >>
            space?
        ).repeat(1).maybe.as(:exports) >>

        str('0') >>
        space? >>

        str('}') >>
        space? >>
        str(';') >>
        filler
    }

    root(:exports)
  end

  class CExportsTransform < Parslet::Transform
    rule(string: simple(:string)) { string.to_s }

    rule(fnname: simple(:fnname),
         arity: simple(:arity)) {
      CExport.new(fnname, arity.to_i, false)
    }

    rule(fnname: simple(:fnname),
         arity: simple(:arity),
         is_vararg: simple(:is_vararg)) {
      CExport.new(fnname, arity.to_i, is_vararg == "1")
    }
  end

  def self.parse_c_exports_from_string(module_name, src)
    src = src.sub(/.*garlic_native_export_t/m, 'garlic_native_export_t')
    tree = CExports.new.parse(src)
    transformed = CExportsTransform.new.apply(tree)
    transformed[:exports]
  end
end

# vim: ts=2 sw=2 :
