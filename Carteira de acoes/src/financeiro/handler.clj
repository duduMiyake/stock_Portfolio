(ns financeiro.handler
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [cheshire.core	:as	json]
            [ring.middleware.defaults :refer [wrap-defaults api-defaults]]
			[ring.middleware.json :refer [wrap-json-body]]
			[financeiro.db :as db]
			[financeiro.transacoes :as transacoes]
			[financeiro.brapi :as api]
			[financeiro.interface :as interface]))

(defn como-json [conteudo & [status]]
	{:status (or status 200)
	 :headers {"Content-Type" "application/json; charset=utf-8"}
	 :body (json/generate-string conteudo)})

(defroutes app-routes		

	(GET "/" [] (interface/iniciar-menu))

	(GET "/geral" [] (como-json (api/lista-companhias)))

	(GET "/companhias" {nome :params}	;;http://localhost:3000/companhias?nome=nomeDaAcao
 		(como-json 
    		(if (or (nil? nome) (empty? nome))
				(let [companhias (api/lista-companhias)]
        			{:nomes-companhias (map #(hash-map :nome-da-empresa (:name %) :codigo-da-acao (:stock %)) (:stocks companhias))})
      			(let [companhia (api/buscar-companhia nome)]
					(if (nil? companhia)
						{:mensagem "Empresa nao encontrada"}
						companhia)))))

	(GET "/companhias/acao" {:keys [query-params]}	;;http://localhost:3000/companhias/acao?acao=codigoDaAcao
		(como-json 
			(if (or (nil? query-params) (empty? query-params))
				{:mensagem "insira uma acao valida"}
				(let [acao (get query-params "acao")
					  response (api/buscar-acao acao)]
					(if (nil? response)
						({:mensagem "Acao nao encontrada"} 404)
						response)))))

	(POST "/transacoes" requisicao	;;codigo-acao, quantidade e tipo
		(let [corpo (:body requisicao)
			  codigo-acao (:codigo-acao corpo)
			  acao-informacoes (api/buscar-acao codigo-acao)]
			(if (:mensagem acao-informacoes)
				(como-json {:mensagem "Ação não encontrada"} 404)
				(let [valor (:ultimo-preco acao-informacoes)]
					(if (transacoes/valida? corpo valor)
						(let [nova-transacao (db/registrar corpo valor)]
							(como-json {:mensagem "Transacao registrada" :transacao nova-transacao}))
						(como-json {:mensagem "Requisicao invalida"} 422))))))

	(GET "/transacoes" [] (como-json {:transacoes (db/transacoes)}))

	(GET "/saldo" [] (como-json {:saldo (db/saldo)}))

	(GET "/vendas" [] (como-json {:vendas (db/transacoes-do-tipo "venda")}))

	(GET "/compras" [] (como-json {:compras (db/transacoes-do-tipo "compra")}))
		
	(route/not-found "Recurso nao encontrado"))

(def app
	(-> (wrap-defaults app-routes api-defaults) 
	(wrap-json-body {:keywords? true :bigdecimals? true})))