from typing import Callable, TypeVar, Any, Tuple, Type

F = TypeVar('F', bound=Callable[..., Any])

def retry(
    tries: int,
    errors: Tuple[Type[BaseException], ...],
) -> Callable[[F], F]:
    ...
