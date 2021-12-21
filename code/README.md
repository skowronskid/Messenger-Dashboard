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


Jak macie lepszy pomysł jak tym zarządzać to git. Jak się wam nie chce do tego stosować unlucky panowie.
