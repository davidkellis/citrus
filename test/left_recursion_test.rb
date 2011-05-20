require File.expand_path('../helper', __FILE__)

class LeftRecursionTest < Test::Unit::TestCase
  def test_lr1
    grammar = Grammar.new {
      rule(:a) { 'a' }
      rule(:b) { 'b' }
      rule(:s) { any([:s, :b], :a) }
    }
    
    match = grammar.parse("abbbbbb", :consume => true, :left_recurse => true, :root => :s)
    assert(match)
  end
  
  def test_lr2
    grammar = Grammar.new {
      rule(:a) { 'a' }
      rule(:b) { 'b' }
      rule(:s) { any(:t, :a) }
      rule(:t) { all(:s, :b) }
    }
    
    match = grammar.parse("abbbbbbbbbbbbb", :consume => true, :left_recurse => true, :root => :s)
    assert(match)
  end

  def test_lr3
    grammar = Grammar.new do
      rule :expr do
        any(all(label(:expr, 'lhs'), '-', label(:expr, 'rhs')){lhs.value - rhs.value},
            :num)
      end
  
      rule :num do
        ext(/[0-9]+/){to_i}
      end
    end
  
    match = grammar.parse("3-4-5", {:consume => true, :left_recurse => true})
    assert(match)
    assert_equal("3-4-5", match)
    assert_equal(-6, match.value)
  end
end
