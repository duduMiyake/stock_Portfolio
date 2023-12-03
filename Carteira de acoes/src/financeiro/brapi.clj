(ns financeiro.brapi
    (:require [clj-http.client :as http]
                [cheshire.core :as json]))

(defn lista-companhias [] 
    (let [url "https://brapi.dev/api/quote/list?sortBy=name&sortOrder=asc&token=mgjZ3zcy8Vmi9Lg6fzQRdq"]
        (-> url 
            (http/get)
            :body
            (json/parse-string true))))

(defn buscar-companhia [nome]
    (let [url "https://brapi.dev/api/quote/list?sortBy=name&sortOrder=asc&token=mgjZ3zcy8Vmi9Lg6fzQRdq"
        response (-> url
                (http/get)
                :body
                (json/parse-string true))
        stocks (:stocks response)
        nome-buscado (get nome :nome) ;;Usa quando e na url
        ;;nome-buscado nome           ;;Usa quando era direto na interface, chamando a funcao
        companhia (first (filter #(= nome-buscado (:name %)) stocks))]
        (if (not (nil? (:name companhia)))
            (let [variacao-do-dia (:change companhia)
                  ultimo-preco (:close companhia)]
                (if (and variacao-do-dia ultimo-preco)	;;se possui a variacao de preco
                    (let [variacao-porcentagem (/ (if (< variacao-do-dia 0) (- ultimo-preco variacao-do-dia) (+ ultimo-preco variacao-do-dia)) 100.0)]
                    {:Nome-da-empresa (:name companhia)
                     :Codigo-da-acao (:stock companhia)
                     :Preco-de-fechamento ultimo-preco
                     :Volume (:volume companhia)
                     :Variacao-do-dia variacao-do-dia
                     :Variacao-em-porcentagem (str (format"%.2f" variacao-porcentagem) "%")});;primeira resp if variacao

                    {:nome-da-acao (:name companhia)
                     :codigo-da-acao (:stock companhia)
                     :ultimo-preco "Nao possui essa informacao"
                     :variacao-do-dia "Nao possui essa informacao"
                     :variacao-porcentagem "Nao possui essa informacao"};;segunda resp if variacao
                )))))

(defn tipo-ativo [codigo-acao]
    (let [url (str "https://api.hgbrasil.com/finance/stock_price?key=f1cde2fc&symbol=" codigo-acao)]
        (let [response (-> url
                        (http/get)
                        :body
                        (json/parse-string true))]
            (if (and response (:results response))
                (let [informacoes (first (vals (:results response)))
                      tipo-ativo (:kind informacoes)]
                    (if (nil? tipo-ativo)
                        (let [ultima-letra (last codigo-acao)]
                            (cond 
                                (= (last tipo-ativo) "3") ("ON") 
                                (= (last tipo-ativo) "11") ("UNT")
                                :else "PN"))
                        tipo-ativo))
                {:error "Resposta da API não contém informações válidas."}))))


(defn buscar-acao [codigo-acao]
    (let [url (str "https://brapi.dev/api/quote/" codigo-acao "?range=1d&interval=1d&token=mgjZ3zcy8Vmi9Lg6fzQRdq")]
        (try 
            (let [response (-> url
                            (http/get)
                            :body
                            (json/parse-string true))]
                (if (:error response)
                    {:mensagem "Acao nao encontrada" :status 404}
                    (let [informacoes (first (:results response))]
                        (if (nil? informacoes)
                            {:mensagem "Informações não encontradas para a ação."}
                            (let [dados-historico (first (:historicalDataPrice informacoes))
                                  nome (:longName informacoes)
                                  codigo (:symbol informacoes)
                                  variacao-dia (:regularMarketChange informacoes)
                                  variacao-dia-porc (:regularMarketChangePercent informacoes)
                                  ultimo-preco (:regularMarketPrice informacoes)
                                  preco-max (:regularMarketDayHigh informacoes)
                                  preco-min (:regularMarketDayLow informacoes)
                                  preco-abertura (:regularMarketOpen informacoes)
                                  preco-fechamento (:close dados-historico)
                                  hora (:regularMarketTime informacoes)
                                  tipo-ativo-info (tipo-ativo codigo-acao)]
                                {:Nome-da-acao nome
                                 :Codigo-da-acao codigo
                                 :Variacao-do-dia variacao-dia
                                 :variacao-em-porcentagem (str (format"%.2f" variacao-dia-porc) "%")
                                 :ultimo-preco ultimo-preco
                                 :preco-maximo preco-max
                                 :preco-minimo preco-min
                                 :preco-de-abertura preco-abertura
                                 :preco-de-fechamento preco-fechamento
                                 :hora hora
                                 :tipo-de-ativo tipo-ativo-info})))))
        (catch Exception e {:mensagem "erro de requisicao"}))))