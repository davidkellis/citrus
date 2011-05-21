require File.expand_path('../helper', __FILE__)

class IndirectLeftRecursionTest < Test::Unit::TestCase
  def test_leftrecursived?
    assert MemoizedInput.new('').memoized?
  end

  grammar :ILR do
    rule :x do
      :expr
    end

    rule :expr do
      any(all(:x, '-', :num),:num)
    end

    rule :num do
      /[0-9]+/
    end
    root :x
  end
  
  def test_ilr
    match = ILR.parse("3-4-5", {:left_recurse=>true})
  end
  
  grammar :BigILR do
    rule :t do
      ext(:term)
    end
    
    rule :term do
      any(all(:t, '+', :f){t.value + f.value},
          all(:t, '-', :f){t.value - f.value},
          :f)
    end
    
    rule :f do
      ext(:fa)
    end
    rule :fa do
      ext(:fact)
    end

    rule :fact do
      any(all(:f, '*', :num){f.value * num.value},
          all(:f, '/', :num){f.value / num.value},
          :num)
    end

    rule :num do
      ext(/[0-9]+/){to_i}
    end
  end

  def test_big_ilr
    match = BigILR.parse("3-4-5", {:left_recurse=>true})
    assert(match)
    assert_equal("3-4-5", match)
    assert_equal(-6, match.value)
    
    match = BigILR.parse("5*4*2-5*9*2", {:left_recurse=>true})
    assert(match)
    assert_equal("5*4*2-5*9*2", match)
    assert_equal(-50, match.value)
    
    match = BigILR.parse("5*4-5-3-18*228", {:left_recurse=>true})
    assert(match)
    assert_equal("5*4-5-3-18*228", match)
    assert_equal(-4092, match.value)
  end
  
  grammar :BigILR2 do
    rule :t do
      ext(:term)
    end
    
    rule :term do
      any(all(label(:t, 'lhs'), '+', label(:t, 'rhs')){lhs.value + rhs.value},
          all(label(:t, 'lhs'), '-', label(:t, 'rhs')){lhs.value - rhs.value},
          :f)
    end
    
    rule :f do
      ext(:fa)
    end
    rule :fa do
      ext(:fact)
    end

    rule :fact do
      any(all(label(:f, 'lhs'), '*', label(:f, 'rhs')){lhs.value * rhs.value},
          all(label(:f, 'lhs'), '/', label(:f, 'rhs')){lhs.value / rhs.value},
          :num)
    end

    rule :num do
      ext(/[0-9]+/){to_i}
    end
  end

  def test_big_ilr2
    match = BigILR2.parse("3-4-5", {:left_recurse=>true})
    assert(match)
    assert_equal("3-4-5", match)
    assert_equal(-6, match.value)
    
    match = BigILR2.parse("5*4*2-5*9*2", {:left_recurse=>true})
    assert(match)
    assert_equal("5*4*2-5*9*2", match)
    assert_equal(-50, match.value)
    
    match = BigILR2.parse("5*4-5-3-18*228", {:left_recurse=>true})
    assert(match)
    assert_equal("5*4-5-3-18*228", match)
    assert_equal(-4092, match.value)
  end
end