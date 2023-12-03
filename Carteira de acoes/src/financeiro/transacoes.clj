(ns financeiro.transacoes)

(defn valida? [transacao valor]
    (and
        (not (nil? valor))
        (number? valor)
        (pos? valor)
        (contains? transacao :codigo-acao)
        (contains? transacao :tipo)
        (contains? transacao :quantidade)
        (or (= "compra" (:tipo transacao))
            (= "venda" (:tipo transacao)))))