from wasm import constants


def round_up_to_page(value: int) -> int:
    remainder = value % constants.PAGE_SIZE_64K
    if remainder == 0:
        return value
    else:
        return value + constants.PAGE_SIZE_64K - (remainder)
