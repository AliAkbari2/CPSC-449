pop(canada, 20).
pop(usa, 200).
pop(india, 800).
pop(china, 1200).
pop(brazil, 120).

area(canada, 4).
area(usa, 3).
area(india, 3).
area(china, 4).
area(brazil, 3).

density(C, D) :-area(C, AC), pop(C, PC), D is PC/AC.