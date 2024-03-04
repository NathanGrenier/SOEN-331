% head/2 returns the first element of a list
head([Head|_], Head).

% tail/2 returns the list without its first element
tail([_|Tail], Tail).

% cons/3 adds an element to the beginning of a list
cons(Element, List, [Element|List]).