(ns financeiro.db
    (:require [clj-time.core :as time]
              [clj-time.format :as f]
              [clj-time.coerce :as c]))
;;lista com os registros 
(def registros
    (atom []))

(defn transacoes []
    @registros)

;;cria lista por tipo de transacao
(defn transacoes-do-tipo [tipo]
    (filter #(= tipo (:tipo %)) (transacoes)))

;;funcao para limpar os registros feitos
(defn limpar []
    (reset! registros []))

;;funcao para adicionar um novo registro
(defn registrar [corpo-da-transacao valor]
    (let [horario (time/now)
          fuso-horario (time/time-zone-for-id "America/Fortaleza")
          horario-local (time/to-time-zone horario fuso-horario)
          formatar (f/formatter "dd-MM-yyyy HH:mm:ss")
          horario-transacao (f/unparse formatar horario-local)
          nova-transacao {:codigo-acao (:codigo-acao corpo-da-transacao)
                         :tipo (:tipo corpo-da-transacao)
                         :quantidade (:quantidade corpo-da-transacao)
                         :valor valor 
                         :id (inc (count @registros))
                         :horario-da-transacao horario-transacao}]
        (swap! registros conj nova-transacao)))

(defn- compra? [transacao]
    (= (:tipo transacao) "compra"))

;;funcao que calcula o saldo
(defn- calcular [acumulado transacao]
    (let [valor (:valor transacao)
          quantidade (:quantidade transacao)]
        (if (compra? transacao)
            (- acumulado (* valor quantidade))
            (+ acumulado (* valor quantidade)))))

;;funcao que devolve a saldo com o valor calculado
(defn saldo []
    (reduce calcular 0 @registros))    