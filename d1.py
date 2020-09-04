import ply.lex
import math

slowa_kluczowe = {
    'pierw' : 'PIERW',
    'log' : 'LOG'
}

tokens = (
    'LICZBA_RZ', 'LICZBA_C', 'PLUS', 'MINUS', 'RAZY', 'DZIELENIE_C', 'DZIELENIE', 'DZIELENIE_M', 'POTEGA', 'NAW_L', 'NAW_P', 'NEGACJA'
, 'ROWNE', 'ROZNE', 'MNIEJSZE', 'MNIEJSZE_ROWNE', 'WIEKSZE_ROWNE', 'WIEKSZE', 'PRZECINEK', 'IDENT', 'PODSTAW', 'ENTER', 'SPACJA', 'USUN'
, 'SHOW') + tuple(slowa_kluczowe.values())

def t_LICZBA_RZ(t):
    '[0-9]+((\.[0-9]+)|([eE]([-+]?)[0-9]+))'
    t.value = int(t.value)
    return t

def t_LICZBA_C(t):
    '[0-9]+'
    t.value = int(t.value)
    return t

def t_PLUS(t):
    r'\+'
    return t

def t_MINUS(t):
    r'-'
    return t

def t_POTEGA(t):
    r'\*\*'
    return t

def t_RAZY(t):
    r'\*'
    return t

def t_DZIELENIE_C(t):
    r'//'
    return t

def t_DZIELENIE(t):
    r'/'
    return t

def t_DZIELENIE_M(t):
    r'%'
    return t

def t_NAW_L(t):
    r'\('
    return t

def t_NAW_P(t):
    r'\)'
    return t

def t_NEGACJA(t):
    r'\~'
    return t

def t_ROWNE(t):
    r'=='
    return t

def t_PODSTAW(t):
    r'='
    return t

def t_ROZNE(t):
    r'\!\='
    return t

def t_MNIEJSZE_ROWNE(t):
    r'\<\='
    return t

def t_MNIEJSZE(t):
    r'\<'
    return t

def t_WIEKSZE_ROWNE(t):
    r'\>\='
    return t

def t_WIEKSZE(t):
    r'\>'
    return t

def t_PRZECINEK(t):
    r'\,'
    return t

#def t_LOG(t):
#    r'log'
#    return t

#def t_PIERW(t):
#    r'pierw'
#    return t

def t_USUN(t):
    r'erase'
    return t

def t_SHOW(t):
    r'show'
    return t

def t_IDENT(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    if t.value in slowa_kluczowe:
        t.type = slowa_kluczowe[t.value]
    return t

def t_ENTER(t):
    r'\n'
    return t

def t_SPACJA(t):
    r'\ '
    return t

def t_error(t):
    print('NIESPODZIEWANY ZNAK %r!' % t.value[0])
    t.lexer.skip(1)

t_ignore = '\t\r'

lekser = ply.lex.lex()

def test_leksera():
    lekser.input(
    """3+/-*32987 4 + / 10 20
    1 10 - + 20
    ala ma kota ()
    -20"""
    )
    while True:
        token = lekser.token()
        if not token:
            break
        print(token)
        print('%r %r %r %r' % (token.type, token.value, token.lineno, token.lexpos))


################### parser

import ply.yacc

precedence = (
    ('left', 'ROWNE', 'ROZNE', 'MNIEJSZE_ROWNE', 'MNIEJSZE', 'WIEKSZE_ROWNE', 'WIEKSZE'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'RAZY', 'DZIELENIE_C', 'DZIELENIE', 'DZIELENIE_M'),
    ('right', 'NEGACJA'),
    ('right', 'POTEGA')
)

tab_sym = {}

def p_program_1(p):
    'program : linijka'
    p[0] = None

def p_program_2(p):
    'program : program ENTER linijka'
    p[0] = None

def p_liczba_rz(p):
    r'w : LICZBA_RZ'

    #print(p[1])
    p[0] = p[1]

def p_liczba_c(p):
    r'w : LICZBA_C'
    #print(p[1])
    p[0] = p[1]

def p_dodawanie(p):
    r'w : w PLUS w'
    #print(p[1], p[2], p[3])
    p[0] = p[1] + p[3]

def p_mnozenie(p):
    r'w : w RAZY w'
    #print(p[1], p[2], p[3])
    p[0] = p[1] * p[3]

def p_potega(p):
    r'w : w POTEGA w'
    #print(p[1], p[2], p[3])
    p[0] = p[1] ** p[3]

def p_dzielenie_c(p):
    r'w : w DZIELENIE_C w'
    #print(p[1], p[2], p[3])
    p[0] = p[1] // p[3]

def p_dzielenie(p):
    r'w : w DZIELENIE w'
    #print(p[1], p[2], p[3])
    p[0] = p[1] / p[3]

def p_dzielenie_m(p):
    r'w : w DZIELENIE_M w'
    #print(p[1], p[2], p[3])
    p[0] = p[1] % p[3]

def p_nawiasy(p):
    r'w : NAW_L w NAW_P'
    #print(p[1], p[2], p[3])
    p[0] = p[2]

def p_odejmowania(p):
    r'w : w MINUS w'
    #print(p[1], p[2], p[3])
    p[0] = p[1] - p[3]

def p_negacja(p):
    r'w : NEGACJA w'
    #print(p[1], p[2], p[3])
    p[0] = - p[2]

def p_rowne(p):
    r'w : w ROWNE w'
    #print(p[1], p[2], p[3])
    #p[0] = int(p[1] == p[3])
    if(p[1] == p[3]):
        p[0] = 1
    else:
        p[0] = 0

def p_rozne(p):
    r'w : w ROZNE w'
    #print(p[1], p[2], p[3])
    p[0] = int(p[1] != p[3])

def p_mniejsze_rowne(p):
    r'w : w MNIEJSZE_ROWNE w'
    #print(p[1], p[2], p[3])
    p[0] = int(p[1] <= p[3])

def p_mniejsze(p):
    r'w : w MNIEJSZE w'
    #print(p[1], p[2], p[3])
    p[0] = int(p[1] < p[3])

def p_wieksze_rowne(p):
    r'w : w WIEKSZE_ROWNE w'
    #print(p[1], p[2], p[3])
    p[0] = int(p[1] >= p[3])

def p_wieksze(p):
    r'w : w WIEKSZE w'
    #print(p[1], p[2], p[3])
    p[0] = int(p[1] > p[3])

def p_log(p):
    r'w : LOG NAW_L w PRZECINEK w NAW_P'
    #print(p[1], p[2], p[3])
    p[3] = int(p[3])
    p[1] = int(p[1])
    p[3] = float(p[3])
    p[1] = float(p[1])
    p[0] = math.log(p[3],p[1])

def p_pierw(p):
    r'w : PIERW NAW_L w PRZECINEK w NAW_P'
    #print(p[1], p[2], p[3])
    p[0] = pow(p[1],p[3])

def p_podstawienie(p):
    r'podstawienie : IDENT PODSTAW w'
    tab_sym[p[1]] = p[3]
    p[0] = None

def p_usuniecie(p):
    r'linijka : USUN SPACJA IDENT'
    p[0] = None

def p_pokaz(p):
    r'linijka : SHOW'
    p[0] = None

def p_linia_pusta(p):
    r'linijka : '
    p[0] = None

def p_linia_wyswietlenie(p):
    r'linijka : w'
    print(p[1])
    p[0] = None

def p_linia_podstawienia(p):
    r'linijka : podstawienie'
    p[0] = None

def p_wyrazenie(p):
    r'w : IDENT'
    p[0] = tab_sym[p[1]]

def p_error(p):
    print("Błąd składniowy: '%s'." % p)

parser = ply.yacc.yacc()

def kalkulator(adres):

    #adres = 'dane.txt'

    plik = open(adres, 'r')

    #print(adres)

    print('\n')
    print('Kalkulator:')
    print('Wczytany plik: ', adres)
    print('\n')
    for a in plik:

        if a[0] == 'r' and a[1] == 'e' and a[2] == 's' and a[3] == 'e' and a[4] == 't':
            if len(tab_sym) > 0:
                tab_sym.clear()
                #print('tab_sym: ', tab_sym)
                print('Usunięto zapisane zmienne')
            else:
                print('Nie zapisano żadnych zmiennych')
        elif a[0] == 'e' and a[1] == 'r' and a[2] == 'a' and a[3] == 's' and a[4] == 'e':

            if len(a[6]) > 0:
                aa = a.lstrip('erase ')
                aaa = aa.rstrip('\n')
                zmienne = aaa.split(', ')

                do_usun = []
                for i in zmienne:
                    #print('i: ',i)
                    for k,w in tab_sym.items():
                        #print('k: ', k)
                        if k == i:
                            do_usun.append(i)
                        #    print('k:i  ', k, ' : ', i)

                for d in do_usun:
                    info = tab_sym.pop(d)
                    print('Usunięto ',d, ' = ', info)

            else:
                print('Brak zmiennej')
        elif a[0] == 's' and a[1] == 'h' and a[2] == 'o' and a[3] == 'w':
            if len(tab_sym) > 0:
                for k,w in tab_sym.items():
                    print('zmienna: ', k, ' = ', w)
            else:
                print('Brak zapisanych zmiennych')
        elif a[0] == 'l' and a[1] == 'o' and a[2] == 'a' and a[3] == 'd':
            s = a.lstrip('load ')
            sciezka = s.rstrip('\n')

            #plik_n = open(sciezka, 'r')

            kalkulator(sciezka)


        elif a[0] == 'd' and a[1] == 'u' and a[2] == 'm' and a[3] == 'p':
            s = a.lstrip('dump ')
            sciezka = s.rstrip('\n')

            plik2 = open(sciezka, 'w')

            for k, w in tab_sym.items():
                linia = ''
                linia = str(k) + ' = ' + str(w) + '\n'
                linia = str(linia)
                plik2.write(linia)
            plik2.close()
        else:
            print(parser.parse(a))

    plik.close()

kalkulator('dane.txt')