congiunzione(and).
disgiunzione(or).
negazione(not).
implicazione(implies).
esiste(exist).
per_ogni(every).
 
fbf2cnf(FBF, CNFFBF):- atomic(FBF),
                       CNFFBF = FBF,
                       !.
fbf2cnf(FBF, CNFFBF):- FBF =.. [X|ListaArgs],
                       semplifica([X|ListaArgs], [], CNFFBF_3_p),
                       semplifica_2(CNFFBF_3_p, [], CNFFBF_4_p),
                       controlla_lista(CNFFBF_4_p, [], CNFFBF_2_p),
                       CNFFBF_2_p =.. [M|Ms],
                       controlla_lista([M|Ms], [], CNFFBF_2),
                       CNFFBF_2 =.. [Y|Ys],
                       semplifica([Y|Ys], [], CNFFBF_3),
                       semplifica_2(CNFFBF_3, [], CNFFBF_4),
                       CNFFBF_R_P =.. CNFFBF_4,
                       not(confronta(CNFFBF_2_p, CNFFBF_2)),
                       fbf2cnf(CNFFBF_R_P, CNFFBF),
                       !.
fbf2cnf(FBF, CNFFBF):- FBF =.. [X|ListaArgs],
                       semplifica([X|ListaArgs], [], CNFFBF_3_p),
                       semplifica_2(CNFFBF_3_p, [], CNFFBF_4_p),
                       controlla_lista(CNFFBF_4_p, [], CNFFBF_2_p),
                       CNFFBF_2_p =.. [M|Ms],
                       controlla_lista([M|Ms], [], CNFFBF_2),
                       CNFFBF_2 =.. [Y|Ys],
                       semplifica([Y|Ys], [], CNFFBF_3),
                       semplifica_2(CNFFBF_3, [], CNFFBF_4),
                       CNFFBF =.. CNFFBF_4,
                       !.
 
confronta(CNFFBF, CNFFBF).  
 
semplifica([], CNFFBF_R, CNFFBF_R).
semplifica([Y|Ys], CNFFBF, CNFFBF_R):- var(Y),
                                       append(CNFFBF, [Y], CNFFBF_1),
                                       semplifica(Ys, CNFFBF_1, CNFFBF_R),
                                       !.
semplifica([Y|Ys], CNFFBF, CNFFBF_R):- congiunzione(Y),
                                       togli_and(Ys, CNFFBF, CNFFBF_1),
                                       append([Y], CNFFBF_1, CNFFBF_R).
semplifica([Y|Ys], CNFFBF, CNFFBF_R):- compound(Y),
                                       Y =.. [Z|Zs],
                                       semplifica([Z|Zs], [], CNFFBF_1),
                                       semplifica(Ys, [], CNFFBF_2),
                                       R1 =.. CNFFBF_1,
                                       append(CNFFBF, [R1], CNFFBF_3),
                                       append(CNFFBF_3, CNFFBF_2, CNFFBF_R),
                                       !.
semplifica([Y|Ys], CNFFBF, CNFFBF_R):- append(CNFFBF, [Y], CNFFBF_1),
                                       semplifica(Ys, [], CNFFBF_2),
                                       append(CNFFBF_1, CNFFBF_2, CNFFBF_R),
                                       !.
 
semplifica_2([], CNFFBF_R, CNFFBF_R).
semplifica_2([Y|Ys], CNFFBF, CNFFBF_R):- var(Y),
                                         append(CNFFBF, [Y], CNFFBF_1), 
                                         semplifica_2(Ys, CNFFBF_1, CNFFBF_R),
                                         !.
semplifica_2([Y|Ys], CNFFBF, CNFFBF_R):- disgiunzione(Y),
                                       togli_or(Ys, CNFFBF, CNFFBF_1),
                                       append([Y], CNFFBF_1, CNFFBF_R).
semplifica_2([Y|Ys], CNFFBF, CNFFBF_R):- compound(Y),
                                       Y =.. [Z|Zs],
                                       semplifica_2([Z|Zs], [], CNFFBF_1),
                                       semplifica_2(Ys, [], CNFFBF_2),
                                       R1 =.. CNFFBF_1,
                                       append(CNFFBF, [R1], CNFFBF_3),
                                       append(CNFFBF_3, CNFFBF_2, CNFFBF_R),
                                       !.
semplifica_2([Y|Ys], CNFFBF, CNFFBF_R):- append(CNFFBF, [Y], CNFFBF_1),
                                       semplifica_2(Ys, [], CNFFBF_2),
                                       append(CNFFBF_1, CNFFBF_2, CNFFBF_R),
                                       !.
 
togli_and([], CNFFBF_R, CNFFBF_R).
togli_and([Y|Ys], CNFFBF, CNFFBF_R):- Y =.. [Z|Zs],
                                      congiunzione(Z),
                                      togli_and(Zs, CNFFBF, CNFFBF_1),
                                      togli_and(Ys, CNFFBF_1, CNFFBF_R),
                                      !.
togli_and([Y|Ys], CNFFBF, CNFFBF_R):- append(CNFFBF, [Y], CNFFBF_1),
                                      togli_and(Ys, CNFFBF_1, CNFFBF_R),
                                      !.
 
togli_or([], CNFFBF_R, CNFFBF_R).
togli_or([Y|Ys], CNFFBF, CNFFBF_R):- Y =.. [Z|Zs],
                                      disgiunzione(Z),
                                      togli_or(Zs, CNFFBF, CNFFBF_1),
                                      togli_or(Ys, CNFFBF_1, CNFFBF_R),
                                      !.
togli_or([Y|Ys], CNFFBF, CNFFBF_R):- append(CNFFBF, [Y], CNFFBF_1),
                                      togli_or(Ys, CNFFBF_1, CNFFBF_R),
                                      !.
 
%% lista vuota
controlla_lista([], CNFFBF_R, CNFFBF_R).
%% trova and
controlla_lista([X|ListaArgs], CNFFBF, CNFFBF_R):- congiunzione(X),
                            count_elements(ListaArgs, N),
                            N >= 2,
                            costruisci_and(ListaArgs, CNFFBF, CNFFBF_1),
                            CNFFBF_R =.. [X|CNFFBF_1],
                            !.
%% controllare se or (... and)
controlla_lista([X|ListaArgs], CNFFBF, CNFFBF_R):- disgiunzione(X),
                                count_elements(ListaArgs, N),
                                N >= 2,
                                cerca_and(ListaArgs),
                                estrai_parametri(ListaArgs, [], Lista_AND),
                                costruisci_or_and(Lista_AND, [], CNFFBF_R1),
                                append(CNFFBF, CNFFBF_R1, CNFFBF_R2),
                                CNFFBF_R =.. ['and'|CNFFBF_R2],
                                !.
 
%% trova or
controlla_lista([X|ListaArgs], CNFFBF, CNFFBF_R):- disgiunzione(X),
                                count_elements(ListaArgs, N),
                                N >= 2,
                                costruisci_or(ListaArgs, [], CNFFBF_1),
                                append(CNFFBF, CNFFBF_1, CNFFBF_2),
                                CNFFBF_R =.. [X|CNFFBF_2],
                                !.
%% trova not di not
controlla_lista([X,X1|ListaArgs], CNFFBF, CNFFBF_R):- negazione(X),
                                X1 =.. [Y,Y1|Ys],
                                count_elements(Ys, M),
                                M = 0,
                                count_elements(ListaArgs, N),
                                N = 0,
                                negazione(Y),
                                costruisci_not(Y1, CNFFBF, CNFFBF_R),
                                !.
%% trova not di atomo
controlla_lista([X,X1|ListaArgs], CNFFBF, CNFFBF_R):- negazione(X),
                                count_elements(ListaArgs, N),   
                                N = 0,
                                atomic(X1),
                                CNFFBF_1 =.. [X|[X1]],
                                append(CNFFBF, CNFFBF_1, CNFFBF_R),
                                !. 
%% CASO #1 : not (and (p, q)) = or (not (p), not (q))
controlla_lista([X,X1|ListaArgs], CNFFBF, CNFFBF_R):- negazione(X), 
                                count_elements(ListaArgs, N),
                                N = 0,
                                compound(X1),   
                                X1 =.. [Y|Ys],
                                congiunzione(Y),    
                                costruisci_not_and(Ys, CNFFBF, CNFFBF_1),
                                CNFFBF_R =.. ['or'|CNFFBF_1],   
                                !.
%% CASO #2 : not (or (p, q)) = and (not (p), not (q))
controlla_lista([X,X1|ListaArgs], CNFFBF, CNFFBF_R):- negazione(X), 
                                count_elements(ListaArgs, N),   
                                N = 0,
                                compound(X1),
                                X1 =.. [Y|Ys],
                                disgiunzione(Y),
                                costruisci_not_or(Ys, CNFFBF, CNFFBF_1),
                                CNFFBF_R =.. ['and'|CNFFBF_1],
                                !.
%% not(every) # not(exist(X, p(X))) = every(X, not(P(X)))
controlla_lista([X,X1|ListaArgs], CNFFBF, CNFFBF_R):- negazione(X),
                                count_elements(ListaArgs, N),   
                                N = 0,
                                X1 =.. [Y,V,F|Ys],
                                count_elements(Ys, M),
                                M = 0,
                                esiste(Y),
                                CNFFBF_F =.. ['not'|[F]],   
                                append([V], [CNFFBF_F], CNFFBF_1),
                                CNFFBF_2 =.. ['every'|CNFFBF_1],
                                CNFFBF_2 =.. [Z|Zs],
                                controlla_lista([Z|Zs], [], CNFFBF_R),
                                !.
%% not(every) # not(every(X, p(X))) = exist(X, not(P(X)))
controlla_lista([X,X1|ListaArgs], CNFFBF, CNFFBF_R):- negazione(X),
                                count_elements(ListaArgs, N),
                                N = 0,
                                X1 =.. [Y,V,F|Ys],
                                count_elements(Ys, M),
                                M = 0,
                                per_ogni(Y),
                                CNFFBF_F =.. ['not'|[F]],
                                append([V], [CNFFBF_F], CNFFBF_1),
                                CNFFBF_2 =.. ['exist'|CNFFBF_1],
                                CNFFBF_2 =.. [Z|Zs],
                                controlla_lista([Z|Zs], [], CNFFBF_R),
                                !.
%% trova un implies # implies(P, Q) = or(not(P), Q)
controlla_lista([X,P,Q|ListaArgs], CNFFBF, CNFFBF_R):- implicazione(X),
                                count_elements(ListaArgs, N),
                                N = 0,
                                costruisci_implicazione(P, Q, CNFFBF_R),
                                                       !.
%% trova un exist # exist(Y, every(Y,p(X,Y))) = every(Y, p(sf666(Y), Y))
controlla_lista([X,V,F|ListaArgs], CNFFBF, CNFFBF_R):- esiste(X),
                                count_elements(ListaArgs, N),
                                N = 0,
                                var(V),
                                F =.. [Y,V1,F1|Ys],
                                count_elements(Ys, M),
                                M = 0,
                                per_ogni(Y),
                                skolem_function([V],SF).
%% trova un exist # exist(X, p(X)) = p(skv+numero)
controlla_lista([X,V,F|ListaArgs], CNFFBF, CNFFBF_R):- esiste(X),
                                count_elements(ListaArgs, N),
                                N = 0,
                                var(V),
                                skolem_function(SK, V),
                                F =.. [Y|Ys],
                                controlla_lista([Y|Ys], [], CNFFBF_R),
                                !.
%% trova un every # every(X, p(X)) = p(skv+numero)
controlla_lista([X,V,F|ListaArgs], CNFFBF, CNFFBF_R):- per_ogni(X),
                                count_elements(ListaArgs, N),
                                N = 0,
                                var(V),
                                skolem_function(SK, V),
                                F =.. [Y|Ys],
                                controlla_lista([Y|Ys], [], CNFFBF_R),
                                !.
%% trova un predicato
controlla_lista([X|Xs], CNFFBF, CNFFBF_R):- count_elements(Xs, N),
                                N >= 1,
                                controlla_predicato([X|Xs], CNFFBF, [Y|Ys]),
                                CNFFBF_R =.. [Y|Ys],
                                !.
 
%% trovato un predicato, controlla i suoi elementi
controlla_predicato([], CNFFBF_R, CNFFBF_R).
controlla_predicato([X|Xs], CNFFBF, CNFFBF_R):- atomic(X),
                                append(CNFFBF, [X], CNFFBF_1),
                                controlla_predicato(Xs, CNFFBF_1, CNFFBF_R),
                                !.
controlla_predicato([X|Xs], CNFFBF, CNFFBF_R):- compound(X),
                                X =.. [Y|Ys],
                                controlla_lista([Y|Ys], [], CNFFBF_1),
                                append(CNFFBF, [CNFFBF_1], CNFFBF_2),
                                controlla_predicato(Xs, CNFFBF_2, CNFFBF_R),
                                !.
 
%% trova un and, nel caso gli elementi siano atomi fa l'append a CNFFBF_R
%% , altrimenti richiama controlla_lista
%% elementi, lista_a_cui_aggiungere, lista_risultato
costruisci_and([], CNFFBF_R, CNFFBF_R).
costruisci_and([X|Xs], CNFFBF, CNFFBF_R):- atomic(X),
                                append(CNFFBF, [X], CNFFBF_1),
                                costruisci_and(Xs, CNFFBF_1, CNFFBF_R),
                                !.
costruisci_and([X|Xs], CNFFBF, CNFFBF_R):- compound(X),
                                X =.. [Y|Ys],
                                controlla_lista([Y|Ys], [], CNFFBF_R1),
                                append(CNFFBF, [CNFFBF_R1], CNFFBF_R2),
                                costruisci_and(Xs, CNFFBF_R2, CNFFBF_R),
                                           !.
 
%% trova un or
costruisci_or([], CNFFBF_R, CNFFBF_R).
costruisci_or([X|Xs], CNFFBF, CNFFBF_R):- atomic(X),
                                append(CNFFBF, [X], CNFFBF_1),
                                costruisci_or(Xs, CNFFBF_1, CNFFBF_R),
                                !.
costruisci_or([X|Xs], CNFFBF, CNFFBF_R):- compound(X),
                                X =.. [Y|Ys],
                                controlla_lista([Y|Ys], [], CNFFBF_R1),
                                costruisci_or(Xs, [], CNFFBF_R2),
                                append(CNFFBF, [CNFFBF_R1], CNFFBF_2),
                                append(CNFFBF_2, CNFFBF_R2, CNFFBF_R3),
                                CNFFBF_R3 =.. [Z|Zs],
                                controlla_lista([Z|Zs], [], CNFFBF_R),
                                !.
 
%% trovo un or con dentro almeno un and, allora costruisco le coppie
%% Lista_parametri_da_accoppiare, CNFFBF_parziale, CNFFBF_finale
costruisci_or_and([], CNFFBF_R, CNFFBF_R).
costruisci_or_and([X|Xs], CNFFBF, CNFFBF_R):- 
                                accoppia(X, Xs, CNFFBF, CNFFBF_R1),
                                costruisci_or_and(Xs, CNFFBF_R1, CNFFBF_R),
                                !.
 
accoppia(Elem, [], CNFFBF_R, CNFFBF_R).
accoppia([], _, CNFFBF_R, CNFFBF_R).
accoppia([E|Es], [[Y|Ys]|Xs], CNFFBF, CNFFBF_R):- 
                                accoppia_and(E, [Y|Ys], CNFFBF, CNFFBF_1),
                                accoppia_and(E, Xs, CNFFBF_1, CNFFBF_2),
                                accoppia(Es, [[Y|Ys]|Xs], CNFFBF_2, CNFFBF_R),
                                !.
accoppia([E|Es], [Y|Ys], CNFFBF, CNFFBF_R):- 
                                accoppia_and(E, [Y|Ys], CNFFBF, CNFFBF_1),
                                elementi_lista(Es, [Y|Ys], CNFFBF_1, CNFFBF_R).
accoppia(Elem, [[Y|Ys]|Xs], CNFFBF, CNFFBF_R):- 
                                append([Elem], [Y], CNFFBF_1),
                                CNFFBF_2 =.. ['or'|CNFFBF_1],
                                append(CNFFBF, [CNFFBF_2], CNFFBF_3),
                                accoppia(Elem, [Ys|Xs], CNFFBF_3, CNFFBF_R),
                                !.
accoppia(Elem, [X|Xs], CNFFBF, CNFFBF_R):- 
                                accoppia(Elem, Xs, CNFFBF, CNFFBF_R),
                                !.
 
accoppia_and(_, [], CNFFBF_R, CNFFBF_R).
accoppia_and(Elem, [[Y|Ys]|Xs], CNFFBF, CNFFBF_R):- 
                                accoppia_and(Elem, [Y|Ys], CNFFBF, CNFFBF_1),
                                accoppia_and(Elem, Xs, CNFFBF_1, CNFFBF_R).
accoppia_and(Elem, [Y|Ys], CNFFBF, CNFFBF_R):- append([Elem], [Y], CNFFBF_1),
                                CNFFBF_2 =.. ['or'|CNFFBF_1],
                                append(CNFFBF, [CNFFBF_2], CNFFBF_3),
                                accoppia_and(Elem, Ys, CNFFBF_3, CNFFBF_R),
                                !.
 
elementi_lista([], _, CNFFBF_R, CNFFBF_R).
elementi_lista([E|Es], [Y|Ys], CNFFBF, CNFFBF_R):- 
                                accoppia_and(E, [Y|Ys], CNFFBF, CNFFBF_1),
                                elementi_lista(Es, [Y|Ys], CNFFBF_1, CNFFBF_R).
 
%% estrae i parametri dell'or
estrai_parametri([], Lista_R, Lista_R).
estrai_parametri([X|Xs], Lista_1, Lista_R):- atomic(X),
                                append(Lista_1, [X], Lista_2),
                                estrai_parametri(Xs, Lista_2, Lista_R),
                                !.
estrai_parametri([X|Xs], Lista_1, Lista_R):- compound(X),
                                X =.. [Y|Ys],
                                congiunzione(Y),
                                controlla_lista([Y|Ys], [], Lista_X),
                                Lista_X =.. [Z|Zs],
                                estrai_parametri(Xs, [], Lista_Xs),
                                append(Lista_1, [Zs], Lista_R1),
                                append(Lista_R1, Lista_Xs, Lista_R),
                                !.
 
estrai_parametri([X|Xs], Lista_1, Lista_R):- compound(X),
                                 X =.. [Y|Ys],
                                controlla_lista([Y|Ys], [], Lista_X),
                                estrai_parametri(Xs, [], Lista_Xs),
                                append(Lista_1, [Lista_X], Lista_R1),
                                append(Lista_R1, Lista_Xs, Lista_R),
                                !.
 
%% cerca se c'è un and
cerca_and([X|Xs]):- atomic(X),
                    cerca_and(Xs),
                    !.      
cerca_and([X|Xs]):- compound(X),
                    X =.. [Y|Ys],
                    count_elements(Ys, N),
                    N >= 2,
                    congiunzione(Y),
                    !.                                      
cerca_and([X|Xs]):- compound(X),
                    X =.. [Y|Ys],
                    cerca_and(Xs),
                    !.
 
%% not è stato trovato, guarda cosa ha come parametro
costruisci_not(Y1, CNFFBF, CNFFBF_R):- atomic(Y1),
                                       append(CNFFBF, Y1, CNFFBF_R),
                                       !.
costruisci_not(Y1, CNFFBF, CNFFBF_R):- compound(Y1),
                                Y1 =.. [Z|Zs],
                                controlla_lista([Z|Zs], CNFFBF, CNFFBF_R),
                                !.
costruisci_not(Y1, CNFFBF, CNFFBF_R):- Y1 =.. [X|Xs],
                                negazione(X),
                                controlla_lista([X|Xs], CNFFBF, CNFFBF_R),
                                !.
 
%% trova un not che ha come parametro un and
costruisci_not_and([], CNFFBF_R, CNFFBF_R).
costruisci_not_and([X|Xs], CNFFBF, CNFFBF_R):- atomic(X),
                                CNFFBF_1 =.. ['not'|[X]],
                                append(CNFFBF, [CNFFBF_1], CNFFBF_2),
                                costruisci_not_and(Xs, CNFFBF_2, CNFFBF_R),
                                !.
costruisci_not_and([X|Xs], CNFFBF, CNFFBF_R):- compound(X),
                                X =.. [Y|Ys],
                                controlla_lista([Y|Ys], CNFFBF_1, CNFFBF_2),
                                CNFFBF_3 =.. ['not'|[CNFFBF_2]],
                                append(CNFFBF, [CNFFBF_3], CNFFBF_4),
                                costruisci_not_and(Xs, CNFFBF_4, CNFFBF_R),
                                !.
 
%% trova un not che ha come parametro un or
costruisci_not_or([], CNFFBF_R, CNFFBF_R).
costruisci_not_or([X|Xs], CNFFBF, CNFFBF_R):- atomic(X),
                                CNFFBF_1 =.. ['not'|[X]],
                                append(CNFFBF, [CNFFBF_1], CNFFBF_2),
                                costruisci_not_or(Xs, CNFFBF_2, CNFFBF_R),
                                !.     
costruisci_not_or([X|Xs], CNFFBF, CNFFBF_R):- compound(X),
                                 X =.. [Y|Ys],
                                controlla_lista([Y|Ys], CNFFBF_1, CNFFBF_2),
                                CNFFBF_3 =.. ['not'|[CNFFBF_2]],
                                append(CNFFBF, [CNFFBF_3], CNFFBF_4),
                                costruisci_not_or(Xs, CNFFBF_4, CNFFBF_R),
                                    !.
 
%% trova un implies # implies(P, Q) = or(not(P), Q)
costruisci_implicazione(P, Q, CNFFBF_R):- atomic(P),
                                          atomic(Q),
                                          CNFFBF_P =.. ['not'|[P]], 
                                          append([CNFFBF_P], [Q], CNFFBF_1),
                                          CNFFBF_R =.. ['or'|CNFFBF_1].
costruisci_implicazione(P, Q, CNFFBF_R):- atomic(P),
                                compound(Q),
                                CNFFBF_P =.. ['not'|[P]],
                                Q =.. [Y|Ys],
                                controlla_lista([Y|Ys], [], CNFFBF_Q_1),
                                append([CNFFBF_P], [CNFFBF_Q_1], CNFFBF_1),
                                CNFFBF_R =.. ['or'|CNFFBF_1].
costruisci_implicazione(P, Q, CNFFBF_R):- compound(P),
                                atomic(Q),
                                CNFFBF_P =.. ['not'|[P]],
                                CNFFBF_P =.. [X|Xs],
                                controlla_lista([X|Xs], [], CNFFBF_P_1),
                                append([CNFFBF_P_1], [Q], CNFFBF_1),
                                CNFFBF_R =.. ['or'|CNFFBF_1].
costruisci_implicazione(P, Q, CNFFBF_R):- CNFFBF_P =.. ['not'|[P]],
                                CNFFBF_P =.. [X|Xs],
                                controlla_lista([X|Xs], [], CNFFBF_P_1),
                                Q =.. [Y|Ys],
                                controlla_lista([Y|Ys], [], CNFFBF_Q_1),
                                append([CNFFBF_P_1], [CNFFBF_Q_1], CNFFBF_1),
                                CNFFBF_R =.. ['or'|CNFFBF_1].
 
skolem_variable(V, SK):- var(V), gensym(skv, SK).
skolem_function([], SF):- skolem_variable(_, SF).
skolem_function([A | ARGS], SF) :- gensym(skf, SF_op),
                                   SF =.. [SF_op, A | ARGS].
 
%% conta gli elementi della lista
count_elements([],0).
count_elements([X|Xs],N):- count_elements(Xs,M), N is M + 1.
 
%%%% controlla se la FBF è clausola di Horn
horn(FBF):- atomic(FBF).
horn(FBF):- fbf2cnf(FBF, CNF),
            CNF =.. [X|Xs],
            negazione(X).
horn(FBF):- fbf2cnf(FBF, CNF),
            CNF =.. [X|Xs],
            conta(Xs, 0, N),
            N &lt; 2.
 
%%%% conta i letterali non negativi         
conta([], N, N).
conta([X|Xs], N, N):- negazione(X).
conta([X|Xs], N1, N):- compound(X),
                       X =.. [Y|Ys],
                       negazione(Y),
                       conta(Xs, 0, M),
                       N is N1 + M,
                       !.
conta([X|Xs], N1, N):- compound(X), 
                       X =.. [Y|Ys],
                       conta(Ys, 0, M0),
                       conta(Xs, 0, M1),
                       M2 is M0 + N1,
                       N is M2 + M1,
                       !.
conta([X|Xs], N1, N):- atomic(X),
                       M1 is N1 + 1,
                       conta(Xs, 0, M),
                       N is M1 + M,
                       !.
 
confronta_liste([],[]).                    
confronta_liste([X|Xs],[X|Ys]):- confronta_liste(Xs, Ys),
                                 !.
