(ns financeiro.interface
  (:require [financeiro.brapi :as api]
            [financeiro.db :as db]
            [clj-http.client :as http]
            [cheshire.core :as json]))

;;comeco do programa
(defn menu-inicial []
    (println "\nEscolha uma opcao (digite o numero):")
    (println "1. Mostrar todas as empresas")
    (println "2. Mostrar detalhes de uma empresa")
    (println "3. Mostrar detalhes de uma acao")
    (println "4. Menu de transacoes")
    (println "5. Sair")
    (read-line))

;;comeco opcao 2
(defn nome-empresa []
    (println "Digite o nome da empresa:")
    (read-line))

(defn detalhes-empresa [nome]
  (let [url (str "http://localhost:3000/companhias?nome=" nome)
        response (-> url 
                    (http/get)
                    :body
                    (json/parse-string true))]
    (if (:mensagem response)
      (println "\nEmpresa nao encontrada.")
      (do
        (println "\nInformacoes da Empresa:")
        (println "*Nome:" (:Nome-da-empresa response))
        (println "*Codigo da Acao:" (:Codigo-da-acao response))
        (println "*Preco de Fechamento:" (:Preco-de-fechamento response))
        (println "*Volume:" (:Volume response))
        (println "*Variacao do Dia:" (:Variacao-do-dia response))
        (println "*Variacao em Porcentagem:" (:Variacao-em-porcentagem response))))))
;;fim opcao 2

;;comeco opcao 3
(defn codigo-acao []
    (println "\nDigite o codigo da acao:")
    (read-line)
)

(defn detalhes-acao [acao]
    (let [url (str "http://localhost:3000/companhias/acao?acao=" acao)
          response (-> url 
                       (http/get)
                       :body
                       (json/parse-string true))]
        (if (:mensagem response)
            (println "\nAcao nao encontrada!")
            (do 
                (println "\nInformacoes da acao:")
                (println "*Nome:" (:Nome-da-acao response))
                (println "*Codigo da acao:" (:Codigo-da-acao response))
                (println "*Tipo de ativo:" (:tipo-de-ativo response))
                (println "*Variacao do dia:" (:Variacao-do-dia response))
                (println "*Variacao em porcentagem:" (:variacao-em-porcentagem response))
                (println "*Ultimo preco:" (:ultimo-preco response))
                (println "*Preco maximo:" (:preco-maximo response))
                (println "*Preco minimo:" (:preco-minimo response))
                (println "*Preco de abertura:" (:preco-de-abertura response))
                (println "*Preco de fechamento:" (:preco-de-fechamento response))
                (println "*Hora:" (:hora response))))))
;;fim opcao 3

;;comeco opcao 4
;;opcao de realizar operacao --- comeco ---
(defn realizar-transacao [tipo]
    (println "\nDigite o codigo da acao:")
    (let [codigo-acao (read-line)
          acao-info (api/buscar-acao codigo-acao)]
        (if (or (:mensagem acao-info) (nil? (:ultimo-preco acao-info)))
            (do
                (println "Acao nao encontrada ou nao possui um preco atualmente, digite outra valida")  
                (println "Voltando para o menu de transacao...") )
            (do
                (println "\nDigite a quantidade:")
                (let [quantidade (try 
                                    (Integer. (read-line))
                                 (catch Exception e nil))]
                    (if (or (nil? quantidade) (<= quantidade 0))
                        (println "Quantidade nao aceitavel, digite uma quantidade valida!")
                        (let [valor (:ultimo-preco acao-info)
                              corpo-da-transacao {:codigo-acao codigo-acao
                                                  :tipo tipo
                                                  :quantidade quantidade}]
                            (db/registrar corpo-da-transacao valor)
                            (println "\nTransacao realizada com sucesso!"))))))))

;;opcao de realizar operacao --- fim ---

;;opcao para mostrar todas as acoes ja feitas --- comeco ---
(defn mostrar-todas-transacoes []
    (let [url "http://localhost:3000/transacoes"
          response (-> url 
            (http/get)
            :body
            (json/parse-string true))
        todas-transacoes (:transacoes response)]
        (if (empty? todas-transacoes)
            (println "Sua conta ainda nao possui transacoes")
            (doseq [transacao todas-transacoes]
                (println "*ID:" (:id transacao))
                (println "*Tipo de transacao:" (:tipo transacao))
                (println "*Codigo:" (:codigo-acao transacao))
                (println "*Quantidade de acoes:" (:quantidade transacao))
                (println "*Valor da acao:" (:valor transacao))
                (println "*Horario da transacao:" (:horario-da-transacao transacao) "\n")))))
;;opcao para mostrar todas as acoes ja feitas --- fim ---

;;opcao para mostrar todas as acoes ja feitas (por tipo) --- comeco ---
(defn mostrar-transacoes-tipo [tipo]
    (let [url (if (= tipo "compra") "http://localhost:3000/compras" "http://localhost:3000/vendas")
          response (-> url
                (http/get)
                :body
                (json/parse-string true))
        transacoes-tipo (if (= tipo "compra") (:compras response) (:vendas response))]
        (if (empty? transacoes-tipo)
            (println (str "Ainda nao ha transacoes do tipo: " tipo))
            (doseq [transacao transacoes-tipo]
                (println "*ID:" (:id transacao))
                (println "*Codigo:" (:codigo-acao transacao))
                (println "*Quantidade de acoes:" (:quantidade transacao))
                (println "*Valor da acao (unidade):" (:valor transacao))
                (println "*Horario da transacao:" (:horario-da-transacao transacao) "\n")))))
;;opcao para mostrar todas as acoes ja feitas (por tipo) --- comeco ---
;;fim opcao 4

;;comeco da funcao para tomar acoes 
(defn opcao-escolhida [opcao]
  (cond 
    (= opcao "1") 
        (let [url "http://localhost:3000/companhias"
              response (-> url
                    (http/get)
                    :body
                    (json/parse-string true))]
            (doseq [companhia (:nomes-companhias response)]
                (print "Nome da Empresa:" (:nome-da-empresa companhia) " |  ")
                (println "Codigo da Acao:" (:codigo-da-acao companhia))))
    (= opcao "2") 
            (do
                (println "\n1. Escolher uma empresa")
                (println "2. Voltar")
                (let [sub-opcao (read-line)]
                    (cond
                        (= sub-opcao "1") (detalhes-empresa (nome-empresa))
                        (= sub-opcao "2") (println "Voltando para o menu principal...\n")
                        :else (println "Opcao invalida!"))))
    (= opcao "3")
            (do
                (println "\n1. Escolher uma acao")
                (println "2. Voltar")
                (let [sub-opcao (read-line)]
                    (cond
                        (= sub-opcao "1") (detalhes-acao (codigo-acao))
                        (= sub-opcao "2") (println "Voltando para o menu principal...\n")
                        :else (println "Opcao invalida!"))))
    (= opcao "4")
        (loop []
            (println "\n1. Realizar uma transacao")
            (println "2. Ver todas as transacoes")
            (println "3. Ver todas as acoes compradas")
            (println "4. Ver todas as acoes vendidas")
            (println "5. Ver meu saldo atual")
            (println "6. Voltar")
            (let [sub-opcao-transacao (read-line)]
                (cond
                    (= sub-opcao-transacao "1") 
                        (do
                            (println "\nGostaria de:")
                            (println "1. Comprar uma acao!")
                            (println "2. Vender uma acao!")
                            (println "3. Voltar!")
                            (let [sub-opcao2 (read-line)]
                                (cond 
                                    (= sub-opcao2 "1") (realizar-transacao "compra")
                                    (= sub-opcao2 "2") (realizar-transacao "venda")
                                    (= sub-opcao2 "3") (println "Voltando...")
                                    :else (println "Opcao invalida"))))
                    (= sub-opcao-transacao "2")
                        (do
                            (println "\nTodas as transacoes da conta:")
                            (mostrar-todas-transacoes))
                    (= sub-opcao-transacao "3")
                        (do
                            (println "\nAcoes compradas:")
                            (mostrar-transacoes-tipo "compra"))
                    (= sub-opcao-transacao "4")
                        (do
                            (println "\nAcoes vendidas:")
                            (mostrar-transacoes-tipo "venda"))
                    (= sub-opcao-transacao "5")
                        (let [url "http://localhost:3000/saldo"
                              response (-> url 
                              (http/get)
                              :body
                              (json/parse-string true))
                              saldo-total (:saldo response)]
                            (println "\nSeu saldo atual e: " (format"%.2f" (double saldo-total))))
                    (= sub-opcao-transacao "6") (println "Voltando para o menu principal...\n")
                    :else (println "Opcao invalida!"))
                (if (not (= sub-opcao-transacao "6")) (recur))))
    :else (println "Opcao invalida!")))
;;fim da funcao para tomar acoes 

(defn iniciar-menu []
    (let [opcao (menu-inicial)]
        (if (= opcao "5")
            (do 
                (println "Encerrando o programa...")
                (System/exit 0))
            (do
                (opcao-escolhida opcao)
                (recur)))))