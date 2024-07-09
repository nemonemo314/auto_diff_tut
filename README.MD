# Intro
Olá, seja bem vindo, esse repo contem as coisas apresentadas nos videos, além de
codigo adicionado "fora da tela", como casos de teste e rescritas do conteudo do
notebook em forma de bilbliotecas.

# Estrutura
Ainda estou construindo o diretorio, e os videos, portanto as coisas podem
mudar. Por enquanto as coisas estão assim:
- Introducao: conteudo dos primeiros videos na forma de notebook
- lib: pasta com codigo fonte na forma de biblioteca, atualmente possui macros
  que implementam aproximação por diferenças finitas multivariada. Espero que
  tanto a simbolica, quanto a autodiff estejam aqui em breve.
- symbolic: codigo referente a diferenciação simbolica apresentado no video 4
 e 5. =bold= Em breve sera movido para a pasta lib. =bold=
- tests: contem utils.ss que tem funções auxiliares e uma framework de tests
  feita com cuspe e fita adesiva entre os videos.

# Como rodar?
Atualmente as coisas não estão prontas, então basicamente só rode os arquivos.
No caso do codigo em /lib voce precisa importa-lo utilizando (import (lib
<nome-da-lib>)), poderá então acessar os simbolos definidos neles.

* Testes
Para o rodar os testes chame no terminal:
#+BEGIN_SRC bash
chezscheme "test/<meu-teste>"
#+END_SRC
Ou no repl:
#+BEGIN_SRC scheme
(load "test/<meu-teste>")
#+END_SRC
