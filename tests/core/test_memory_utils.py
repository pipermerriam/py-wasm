from hypothesis import (
    given,
    strategies as st,
)

from wasm import constants
from wasm._utils.memory import (
    round_up_to_page,
)


@given(value=st.integers(min_value=0, max_value=2**32 - 1))
def test_round_up_to_page(value):
    result = round_up_to_page(value)
    assert result % constants.PAGE_SIZE_64K == 0
