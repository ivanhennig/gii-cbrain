<?php

define('MONGO_HOST','127.0.0.1');
define('MONGO_DB','giicbrain');
require_once './metaphone_pt.php';
require_once './metaphone_es.php';
require_once './class.Db.php';
/**
 * giiCBRain Main class
 * @author Ivan Hennig
 */
class Main {
	/**
	 *
	 * @return string[] Retorna drivers de banco
	 */
	public function getDrivers() {
		return array("POSTGRESQL","MYSQL","MSSQL","ORACLE","FIREBIRD");
	}
	private function _MongoClient() {
		try {
			if (!MONGO_HOST) {
				return new MongoClient();
			} else {
				return new MongoClient(MONGO_HOST, array(
//					"readPreference"   => MongoClient::RP_PRIMARY_PREFERRED,
//					"connectTimeoutMS" => "500",
//					"socketTimeoutMS"  => "1500"
				));
			}
		} catch (MongoConnectionException $e) {
			throw new Exception("Cannot connect to internal database");
		} catch (Exception $e) {
			throw $e;
		}
	}
	/**
	 *
	 * @param string $case_json JSON das configurações geradas na GUI
	 * @param string $input_casedata_json JSON de um caso a ser armazenado
	 * @return boolean
	 * @throws Exception
	 */
	public function store($case_json, $input_casedata_json) {
		$input_casedata  = json_decode($input_casedata_json, true);
		$case_definition = json_decode($case_json, true);

		$Mongo      = $this->_MongoClient();
		$Collection = $Mongo->selectDB(MONGO_DB)->selectCollection($case_definition['casename']);
		$Collection->insert($input_casedata);

		return true;
	}
	/**
	 *
	 * @param string $case_json JSON das configurações geradas na GUI
	 * @param string $input_casedata_json JSON de um caso a ser apresentado
	 * @return string JSON de um array com até 5 dos melhores retornos
	 * @throws Exception
	 */
	public function query($case_json, $input_casedata_json) {
		//file_put_contents("case.json", $case_json);
		// file_put_contents("casedata.json", $casedata_json);



		$return = array();
		$case_definition = json_decode($case_json);
		if (!$case_definition) {
			throw new Exception("Case not found", 10000);
		}
		$input_casedata       = json_decode($input_casedata_json, true);

		$Mongo      = $this->_MongoClient();
		$DataObj    = $Mongo->selectDB(MONGO_DB)->selectCollection($case_definition->casename);

		//Executando na base confirmada de casos
		$return = $this->_exec($case_definition, $input_casedata, $DataObj, $return);

		if ($case_definition->casetype==="file") {//Usando CSV como base de dados
			$DataObj = new CSVIterator(
					$case_definition->casefilename,
					$case_definition->casecsvsep,
					'"',
					'\\',
					$case_definition->casecsvfirstlinetitle
					);
		} else {//Banco de dados...
			$con = Db::newInstance(
					$case_definition->project->projectdriver,
					$case_definition->project->projectconnectionname,
					$case_definition->project->projecthost,
					$case_definition->project->projectport,
					$case_definition->project->projectusername,
					$case_definition->project->projectpassword);
			$DataObj = (new we_Read($con))->select()->from($case_definition->caserelation);

		}

		//Executando na base de casos
		$return = $this->_exec($case_definition, $input_casedata, $DataObj, $return);

		//Ordenando do melhor ao pior
		usort($return, function($a, $b) {
			if ($a['_score'] == $b['_score']) {
				return 0;
			}
			return ($a['_score'] < $b['_score']) ? 1 : -1;
		});

		//Eliminando piores
		return array_slice($return, 0, 5);
	}
	protected function _exec($case_definition, $input_casedata, $It, $return) {


		//Criando um array das funcoes para aplicar em cada linha do resultado
		//Guardando os pesos pra enviar à similaridade global
		$functions    = [];

		$outputs        = [];
		$outputs['_id'] = 1;
		//Variavel para guardar a soma total de pesos
		$total_weight = 0.0;
		$index_filters  = [];
		foreach ($case_definition->attrs as $attr) {
			//Criar funcao para os atributos de entrada e marcados para indexar
			if (($attr->type === "input" || $attr->type === "input_output")) {
				if ($attr->function === "equal") {
					$functions[$attr->attrname] = [$this->createFunctionExact(),$attr->weight];
					if ($attr->index === true) {
						$index_filters[$attr->attrname] = $input_casedata[$attr->attrname];
					}
				} else if ($attr->datatype === "number" && $attr->function === "linear") {
					//Parametro necessario para a funcao linear
					$range = abs($attr->number_params->lowbound - $attr->number_params->highbound);
					$functions[$attr->attrname] = [$this->createFunctionNumberLinear($range),$attr->weight];
				} else if ($attr->datatype === "number" && $attr->function === "step") {
					$functions[$attr->attrname] = [$this->createFunctionNumberStep($attr->numberstep_params->threshold),$attr->weight];
				} else if ($attr->datatype === "string" && $attr->function === "case") {
					$functions[$attr->attrname] = [$this->createFunctionStringCase(),$attr->weight];
					if ($attr->index === true) {
						$index_filters[$attr->attrname] = new we_WhereOpLike($It,  $input_casedata[$attr->attrname] );
					}
				} else if ($attr->datatype === "string" && $attr->function === "icase") {
					$functions[$attr->attrname] = [$this->createFunctionStringCaseInsensitive(),$attr->weight];
					if ($attr->index === true) {
						$index_filters[$attr->attrname] = new we_WhereOpILike($It,  $input_casedata[$attr->attrname] );
					}
				} else if ($attr->datatype === "string" && $attr->function === "metaphone") {
					$functions[$attr->attrname] = [$this->createFunctionStringMetaphone($attr->metaphone_params->language),$attr->weight];
					//TODO Index
				} else if ($attr->datatype === "phrase" && $attr->function === "intersect_metaphone") {
					$functions[$attr->attrname] = [$this->createFunctionPhraseIntersectMetaphone($attr->metaphone_params->language),$attr->weight];
					//TODO Index
				} else if ($attr->datatype === "phrase" && $attr->function === "intersect") {
					$functions[$attr->attrname] = [$this->createFunctionPhraseIntersect(),$attr->weight];
					//TODO Index
				} else if ($attr->datatype === "symbolunordered") {//Utiliza apenas uma função
					$functions[$attr->attrname] = [$this->createFunctionSymbolUnorderedDefault($attr->symbolunordered_params),$attr->weight];
					//TODO Index
				} else if ($attr->datatype === "symbol") {//
					$functions[$attr->attrname] = [$this->createFunctionExact(),$attr->weight];
					if ($attr->index === true) {
						$index_filters[$attr->attrname] = $input_casedata[$attr->attrname];
					}
				} else {
				}
				$total_weight += floatval($attr->weight);
			}
			if ($attr->type === "output" || $attr->type === "input_output") {
				$outputs[$attr->attrname] = 1;
			}

		}

		if ($case_definition->caseglobalsim === 'nearest') {
			$globalSim = function($local_sim, $total_weight) {
				return $this->globalSimNearest($local_sim, $total_weight);
			};
		} else if ($case_definition->caseglobalsim === 'nearest') {
			$globalSim = function($local_sim, $total_weight) {
				return $this->globalSimEuclidean($local_sim, $total_weight);
			};
		} else {
			throw new Exception("Invalid global similarity function");
		}
		if ($It instanceof we_Read) {
			///* @var $It we_Read */
			$It = $It->whereAND($index_filters)->exec();

		} else if ($It instanceof MongoCollection) {
			///* @var $It MongoCollection */
			array_walk($index_filters, function(&$v){
				$v = $v->convert();
			});
			$It = $It->find($index_filters);
		}
		foreach ($It as $row) {//Para cada linha de dados
			$local_sim = [];
			foreach ($row as $key => $value) {
				//Processamento
				if (isset($functions[$key])) {
					//Posição 0 contem a funcao e, 1 contem o peso
					list($localSimFunction,$localSimWeight) = $functions[$key];
					$local_sim[] = [$localSimFunction($value, $input_casedata[$key]), $localSimWeight];
				}
			}
			$score      = $globalSim($local_sim, $total_weight);
			$perc_score = round( $score, 4);
			$min_score  = floatval($case_definition->casemin_score);
			if ($score && $perc_score > $min_score) {//Definição de score mínimo
				$output           = array_intersect_key($row, $outputs);
//				$output           = $row;
				$output['_score'] = $perc_score;
//				$output['_localsim'] = $local_sim;
				// $output['_outputs'] = $outputs;
				//Inclui no array os melhores
				$return[]         = $output;
			}
		}

		return $return;
	}
	private function globalSimNearest($local_sim, $total_weight) {
		$sum = 0;
		foreach ($local_sim as list($v, $w)) {
			$sum += $v * $w;
		}
		return $sum / $total_weight;
	}
	private function globalSimEuclidean($local_sim, $total_weight) {
		$sum = 0;
		foreach ($local_sim as list($v, $w)) {
			$sum += pow($v , 2) * $w;
		}
		return sqrt($sum / $total_weight);
	}
	private function createFunctionNumberLinear($range) {
		return function($a, $b) use ($range) {
			return (1 - (abs($a - $b) / $range));
		};
	}
	private function createFunctionExact() {
		return function($a, $b) {
			return ( $a === $b ) ? 1 : 0;
		};
	}
	private function createFunctionNumberStep($threshold) {
		return function($a, $b) use ($threshold) {
			return ( abs($a - $b) <= $threshold ) ? 1 : 0;
		};
	}
	private function createFunctionStringCase() {
		return function($a, $b) {
			if (strcmp($a, $b)===0) return 1;
			return 0;
		};
	}
	private function createFunctionStringCaseInsensitive() {
		return function($a, $b) {
			if (strcmp(mb_strtoupper($a), mb_strtoupper($b))===0) return 1;
			return 0;
		};
	}
	private function createFunctionStringMetaphone( $lang) {
		if ($lang === 'en') {
			return function ($a, $b) {
				if (strcmp(metaphone($a), metaphone($b))===0) return 1;
				return 0;
			};
		} else if ($lang === 'pt') {
			return function($a, $b) {
				if (strcmp(metaphone_pt($a), metaphone_pt($b))===0) return 1;
				return 0;
			};
		} else if ($lang === 'es') {
			return function($a, $b) {
				if (strcmp(metaphone_es($a), metaphone_es($b))===0) return 1;
				return 0;
			};
		}

	}
	private function createFunctionPhraseIntersectMetaphone( $lang) {
		return function ($a, $b) use ($lang) {
			//Quebrando a frase em palavras
			$phrase_a = preg_split("/[\s\.,:\(\);\?!]+/", $a,-1,PREG_SPLIT_NO_EMPTY);
			$phrase_b = preg_split("/[\s\.,:\(\);\?!]+/", $b,-1,PREG_SPLIT_NO_EMPTY);
			//Removando duplicatas
			$phrase_a = array_unique($phrase_a);
			$phrase_b = array_unique($phrase_b);
			//Removendo palavras curtas
			$phrase_a = array_filter($phrase_a,function($v){
				return strlen($v)>3;
			});
			$phrase_b = array_filter($phrase_b,function($v){
				return strlen($v)>3;
			});
			//Aplicando metaphone
			$phrase_a = array_map(function($v) use ($lang) {
				if ($lang === 'en') {
					return metaphone($v);
				} else if ($lang === 'pt') {
					return metaphone_pt($v);
				} else if ($lang === 'es') {
					return metaphone_es($v);
				}
			}, $phrase_a);
			$phrase_b = array_map(function($v) use ($lang) {
				if ($lang === 'en') {
					return metaphone($v);
				} else if ($lang === 'pt') {
					return metaphone_pt($v);
				} else if ($lang === 'es') {
					return metaphone_es($v);
				}
			}, $phrase_b);


			//Calculo similaridade
			$intersect = array_intersect($phrase_a, $phrase_b);

//			print_r($intersect);

			$union     = array_unique( array_merge($phrase_a, $phrase_b) );

//			print_r($union);

			return (count($intersect)/count($union));
		};
	}
	private function createFunctionPhraseIntersect() {
		return function ($a, $b) {
			//Quebrando a frase em palavras
			$phrase_a = preg_split("/[\s\.,:\(\);\?!]+/", $a,-1,PREG_SPLIT_NO_EMPTY);
			$phrase_b = preg_split("/[\s\.,:\(\);\?!]+/", $b,-1,PREG_SPLIT_NO_EMPTY);
			//Removendo duplicatas
			$phrase_a = array_unique($phrase_a);
			$phrase_b = array_unique($phrase_b);
			//Removendo palavras curtas
			$phrase_a = array_filter($phrase_a,function($v){
				return strlen($v)>2;
			});
			$phrase_b = array_filter($phrase_b,function($v){
				return strlen($v)>2;
			});
			//Calculo similaridade
			$intersect = array_intersect($phrase_a, $phrase_b);
			$union     = array_merge($phrase_a, $phrase_b);

			return (count($intersect)/count($union));
		};
	}
	private function createFunctionSymbolUnorderedDefault( $symbolunordered_params) {
		//Exemplo
		//"symbolunordered_params": [
		//	[""                       ,"Aprovado","Aguardando","N\u00c3\u00a3o Aprovado"],
		//	["Aprovado"               ,""        ,"0.9"       ,"0.1"                    ],
		//	["Aguardando"             ,"0.9"     ,""          ,"0.2"                    ],
		//	["N\u00c3\u00a3o Aprovado","0.1"     ,"0.2"       ,""                       ]
		//],
		$symbolunordered_associative = [];

		foreach ($symbolunordered_params as $i => $symbolunordered_param) {
			if (!$i) {//Primeiro item, coletar chaves
				$keys = array_slice($symbolunordered_param, 1);
				continue;
			}
			$symbolunordered_associative[$symbolunordered_param[0]] = array_combine($keys, array_slice($symbolunordered_param, 1));//Primeiro elemento vira chave
		}
		// var_dump($symbolunordered_param[0]);
		return function ($a, $b) use ( $symbolunordered_associative) {
			if ($a===$b) {//Se for igual não precisa procurar é 100% similar
				return 1;
			}
			// var_dump($a);
			// var_dump($b);
			return floatval( $symbolunordered_associative[$a][$b] );
		};
	}

}