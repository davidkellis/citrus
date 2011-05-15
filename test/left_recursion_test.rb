require File.expand_path('../helper', __FILE__)

class LeftRecursionTest < Test::Unit::TestCase
  def test_exec
    grammar = Grammar.new {
      rule(:a) { 'a' }
      rule(:b) { 'b' }
      rule(:c) { 'c' }
      rule(:z) { 'z' }
      rule(:s) { any([:s, :a, :b, :c], :z) }
    }
    
    match = grammar.parse("zabcabcabc", :consume => true, :left_recurse => true, :root => :s)
    assert(match)
    
    # input = LeftRecursiveMemoizedInput.new()
    # events = input.exec(s)

    # expected_events = [
    #   s,
    #     s,
    #       s,
    #         s,
    #           z, CLOSE, 1,
    #         CLOSE, 1,
    #         a, CLOSE, 1,
    #         b, CLOSE, 1,
    #         c, CLOSE, 1,
    #       CLOSE, 4,
    #       a, CLOSE, 1,
    #       b, CLOSE, 1,
    #       c, CLOSE, 1,
    #     CLOSE, 7,
    #     a, CLOSE, 1,
    #     b, CLOSE, 1,
    #     c, CLOSE, 1,
    #   CLOSE, 10
    # ]

    # assert_equal(expected_events, events)
  end
end
