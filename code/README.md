# Ściana z info

## Instrukcje
Żeby był porządek to dam troche instrukcji do wstawiania kodu.

  1.   Wg mnie warto jest, aby to co jest już zrobione dobrze trzymać w oddzielnym pliku.
  2.   Następnie to nad czym pracujemy umieszczamy w nowym pliku z odpowiednim tytułem.
  3.   Chyba warto pisac rozne rzeczy w funkcjach, bo potem łatwo jest znowu ich użyć 
  4.   Żeby użyć poprzednich plików należy użyć funkcji `source()` (przykład: `source("wczytanie_json.R")`)

## Opisy plików
Dodatkowo jeśli będziemy mieć już kilka plików to warto wiedzieć, co jest w którym więc spróbujmy opisać je tu.

  * _wczytanie_json.R_ - wczytuje sie tu pliki .json z /messages/inbox. Są do tego dwie funkcje: load_one_conversation() i load_all_conversations(). Myślę, że nazwy mówią same za siebie. (Przykład użycia funkcji: `mess_df <- load_all_conversations()` - macie ramke ze wszystkimi konwersacjami i jest git.
  * _twd_fb_dane.ipynb_ - mazuryk ogarniał wczytywanie polskich znaków z plikow .json
  * _liczenie_slow.R_ - są tu funkcje głownie przydatne do szukania różnych patternów w tym co piszemy. `make_my_df()` - po prostu filtruje nasze wszystkie konwersacje, żeby zostały nasze wiadomości. `get_nr_of_messages()` zwraca nam ile napisaliśmy my wiadomości i wszyscy inni do nas na grupach i osobistych konwersacjach. `substring_count()` zwraca ile razey w naszych wiadomościach pojawił się jakis substring. `word_count()` robi to samo, ale dla całych słów. `words_timeline()` tworzy szybki wykres w stylu "Google Trends" dla podanego przez nas słowa. `xd_haha_comparison()` tworzy podonby wykres używaych przez nas różnych rodzajów "xd" i "haha".`messages_sent()` łączna ilość wiadomości w danym  w kolejnych miesiącach.
  * _day_streak.R_ - funkcja `day_streak()`  zwraca nieprzerwane serie dni, w których z kimś pisaliśmy w chacie osobistym, a do tego kiedy te serie się zaczęły i skończyły i z kim były, dla wszystkich naszych konwersacji. Funkcja  `day_streak_1p()` robi to samo tylko dla jednej konwersacji jaką wybierzemy.
  * _potezne_xd.R_ - funkcja `get_potezne_xd()` znajduje nasze najdłuższe "xd" (niezaleznie od wielkości liter) i zwraca nam kilka wiadomości przed i po tym jak to się wydarzyło 


Jak macie lepszy pomysł jak tym zarządzać to git. Jak się wam nie chce do tego stosować unlucky panowie.
