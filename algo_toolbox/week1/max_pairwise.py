"""Implementations for maximum pairwise product of a non-negative integer sequence:

The functions work strictly on the defined domain, do not handle inputs. 
In business applications one would handle errors (...-> Result[int, Error]).

    * Naive solution - by brute force (useful in stress-tests correctness)
    * Standard solution - 2n comparisons
    * Using numpy - should scale very well (because of introselect and arrays)
    * Minimize number of comparisons - theoretically exciting, but overhead not worth it
"""

from typing import List, NewType, Tuple
from itertools import combinations
import numpy as np


# Key Idea: Make impossible states impossible
NonNegativeInt = NewType("NonNegativeInt", int)
NonNegativeSeq = NewType("NonNegativeSeq", List[NonNegativeInt])


def max_pairwise_naive(sequence: NonNegativeSeq) -> NonNegativeInt:
    """A brute-force way, using a `combinations()` generator for pairs (lazy evaluation)
    Then map() products and reduce using max().
        
    Time complexity:  
        O(n^2). For n = 2*10^5 results in 2*10^10. Practically, too slow after 10^4
    """
    
    products = map(
        lambda pair: pair[0] * pair[1],  # function to apply to each tuple
        combinations(sequence, 2),       # a generator
    )
    
    return max(products)


def max_pairwise(sequence: NonNegativeSeq) -> NonNegativeInt:
    """A straightforward algorithm taking advantage of python specificities"""

    # Can also init by using first two elements of list and compare them: 
    # Worst-case complexity: 2N-3 => O(N)
    first = second = float('-inf')
    
    for x in sequence:
        if x > second:
            if x >= first:
                first, second = x, first          
            else:  # meaning x > second and x < first
                second = x
                
    return first * second


def max_pairwise_numpy(sequence: NonNegativeSeq) -> NonNegativeInt:
    """A solution using numpy, a very compact algorithm, leveraging introselect algorithm
    Worst case complexity is O(nlog(n)).
    
        1. Finding the indices of 2 greatest values in the list with `introselect` (numpy)
        2. Compute product of those values
    """

    xs = np.array(sequence)
    idx = np.argpartition(xs, -2)[-2:]  # two largest values
    return np.product(xs[idx])


if __name__ == "__main__":
    x = range(1, 200000+1)
    print(
        max_pairwise(x), 
        max_pairwise_numpy(x), 
    )