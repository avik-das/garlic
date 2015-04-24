require 'rspec'

require_relative '../../c_parse'

describe CParser do
  describe '#parse_c_exports_from_string' do
    it 'parses a basic exports structure' do
      input = <<-CODE
scm_native_export_t mod_exports[] = {
    {"init", init, 1},
    {"cleanup", cleanup, 2},
    0
};
      CODE

      exports = [
        CParser::CExport.new('init', 1),
        CParser::CExport.new('cleanup', 2)
      ]

      parsed = CParser.parse_c_exports_from_string('mod', input)
      expect(parsed).to eq(exports)
    end

    it 'allows for various amounts of space' do
      input = <<-CODE
scm_native_export_t mod_exports[] = {
    {"a", a, 0},
    {"b",b,0},
    { "c", c, 0 },
    {"d",d,0},{"e",f,0},

    { "f", f, 0 },
    0
};
      CODE

      exports = [
        CParser::CExport.new('a', 0),
        CParser::CExport.new('b', 0),
        CParser::CExport.new('c', 0),
        CParser::CExport.new('d', 0),
        CParser::CExport.new('e', 0),
        CParser::CExport.new('f', 0)
      ]

      parsed = CParser.parse_c_exports_from_string('mod', input)
      expect(parsed).to eq(exports)
    end

    it 'allows for tight spacing' do
      input = <<-CODE
scm_native_export_t mod_exports[]={{"a",a,0},0};
      CODE

      exports = [
        CParser::CExport.new('a', 0)
      ]

      parsed = CParser.parse_c_exports_from_string('mod', input)
      expect(parsed).to eq(exports)
    end

    it 'ignores surrounding code' do
      input = <<-CODE
#include <scm.h>
#include <other_lib.h>

scm_value_t init(scm_value_t a) {
    return NULL;
}

scm_value_t cleanup() {
    return NULL;
}

scm_native_export_t mod_exports[] = {
    {"init", init, 1},
    {"cleanup", cleanup, 2},
    0
};

void private_function() {
    // do something
}
      CODE

      exports = [
        CParser::CExport.new('init', 1),
        CParser::CExport.new('cleanup', 2)
      ]

      parsed = CParser.parse_c_exports_from_string('mod', input)
      expect(parsed).to eq(exports)
    end
  end
end

# vim: ts=2 sw=2 :
