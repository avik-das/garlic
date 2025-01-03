#!/usr/bin/env ruby

def to_tc(val, nbytes)
  return val if val >= 0

  base = 0
  nbytes.times do base = (base << 8) | 0xff; end
  base - (-val - 1)
end

def to_le(val, nbytes)
  bytes = []
  nbytes.times do
    bytes << (val & 0xff)
    val = val >> 8
  end

  bytes
end

LabelDef = Struct.new(:name)
LabelRef = Struct.new(:generator, :refs) do
  def to_initial_attempt =
    LabelRefAttempt.new(generator, refs, generator.call(0))
end

Bytes = Struct.new(:bytes) do
  def size = bytes.size
end

LabelRefAttempt = Struct.new(:generator, :refs, :last_bytes) do
  def size = last_bytes.size
end

NOP = Bytes.new([0x90])
jmp = lambda { |delta|
  if delta >= -128 and delta <= 127
    [0xeb] + to_le(to_tc(delta, 1), 1)
  else
    [0xe9] + to_le(to_tc(delta, 4), 4)
  end
}

def resolve_local_labels(instrs)
  # Pass 1: generate attempts with label deltas == 0
  attempt = instrs.map { |inst|
    case inst
    when LabelDef then inst
    when Bytes then inst
    when LabelRef then inst.to_initial_attempt
    end
  }

  # Next passes:
  #   1. Calculate label addresses
  #   2. Go through ref attempts to see if anything changed
  # TODO: multiple attempts
  # TODO: calculate label addresses
  attempt = attempt.map { |inst|
    case inst
    when LabelDef then inst
    when Bytes then inst
    when LabelRefAttempt
      # TODO: create new attempt
    end
  }

  # Finally: output bytes
  resolved = []
  attempt.each do |inst|
    case inst
    when LabelDef then nil
    when Bytes then resolved << inst.bytes
    when LabelRefAttempt then resolved << inst.last_bytes
    end
  end

  resolved
end

resolved = resolve_local_labels([
  LabelDef.new(:label1),
  NOP,
  LabelRef.new(jmp, [:label2]),
  NOP,
  LabelRef.new(jmp, [:label1]),
  NOP,
  LabelDef.new(:label2),
])

addr = 0
resolved.each do |inst|
  astr = addr.to_s(16).rjust(4, " ")
  istr = inst.map { |b| b.to_s(16).rjust(2, "0") }.join(" ")
  puts " #{astr}:  #{istr}"

  addr += inst.size
end

## TEST BYTE SERIALIZATION ####################################################

# [
#   # 1 byte
#   [
#     1,
#     [
#         0,
#         1,
#         2,
#         4,
#         8,
#        16,
#        32,
#        64,
#       127,
#        -1,
#        -2,
#        -4,
#        -8,
#       -16,
#       -32,
#       -64,
#      -127,
#      -128,
#     ]
#   ],
# 
#   # 2 bytes
#   [
#     2,
#     [
#           0,
#           1,
#           2,
#           4,
#           8,
#          16,
#          32,
#          64,
#         128,
#         256,
#         512,
#        1024,
#       32767,
#          -1,
#          -2,
#          -4,
#          -8,
#         -16,
#         -32,
#         -64,
#        -128,
#        -256,
#        -512,
#       -1024,
#      -32767,
#      -32768,
#     ]
#   ],
# 
#   # 4 bytes
#   [
#     4,
#     [
#                0,
#                1,
#                2,
#                4,
#                8,
#               16,
#               32,
#               64,
#              128,
#              256,
#              512,
#             1024,
#            32767,
#       2147483647,
#               -1,
#               -2,
#               -4,
#               -8,
#              -16,
#              -32,
#              -64,
#             -128,
#             -256,
#             -512,
#            -1024,
#           -32767,
#           -32768,
#      -2147483647,
#      -2147483648,
#     ]
#   ],
# ].each do |n, vs|
#   puts "BYTES: #{n}"
#   puts "--------"
#   vs.each do |v|
#     t = to_tc(v, n)
#     l = to_le(t, n)
#     vstr = v.to_s(10).rjust(11       , " ")
#     tstr = t.to_s(16).rjust(2 * n + 1, " ")
#     lstr = l.map { |b| b.to_s(16).rjust(2, "0") }.join(", ")
#     puts "  #{vstr} -> #{tstr} -> [#{lstr}]"
#   end
#   puts
# end

## TEST INSTRUCTIONS ##########################################################

# [
#   [:jmp, jmp,    1], # JMP rel8
#   [:jmp, jmp,    2],
#   [:jmp, jmp,    4],
#   [:jmp, jmp,    8],
#   [:jmp, jmp,   16],
#   [:jmp, jmp,   32],
#   [:jmp, jmp,   64],
#   [:jmp, jmp,  127],
#   [:jmp, jmp,   -1],
#   [:jmp, jmp,   -2],
#   [:jmp, jmp,   -4],
#   [:jmp, jmp,   -8],
#   [:jmp, jmp,  -16],
#   [:jmp, jmp,  -32],
#   [:jmp, jmp,  -64],
#   [:jmp, jmp, -127],
#   [:jmp, jmp, -128],
#   [:jmp, jmp,  128], # JMP rel32
#   [:jmp, jmp,  256],
#   [:jmp, jmp, -129],
#   [:jmp, jmp, -256],
# ].each do |mnemonic, generator, *args|
#   bs = generator.call(*args)
#   bsstr = bs.map { |b| b.to_s(16).rjust(2, "0") }.join(", ")
#   puts "#{mnemonic} -> [#{bsstr}]"
# end
