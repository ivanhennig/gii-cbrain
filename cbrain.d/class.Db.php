<?php
if (!function_exists("_t")) {
	function _t($v, $p) {
		return $v . $p;
	}
}

class Db {

	/**
	 * @var string Armazena a identificação de qual banco de dados será usado na conexão.
	 */
	protected $sgbd;

	/**
	 * @var resource Conterá o identificador da conexão realizada com o banco.
	 */
	public $id;
	/**
	 * @var resource Objeto noSQL
	 */
	public $db_instance;

	/**
	 * @var string Nome do banco de dados para conexão.
	 */
	protected $nome_banco;

	/**
	 * @var string Endereço do servidor.
	 */
	protected $servidor;

	/**
	 * @var int Número da porta para conectar no servidor.
	 */
	protected $porta;

	/**
	 * @var string Nome do usuário para autenticação do acesso ao banco de dados.
	 */
	protected $usuario;

	/**
	 * @var string Senha do usuário especificado para acessar o banco.
	 */
	protected $senha;

	/**
	 * @var resource|bool Conforme o tipo de consulta ou operação realizada no banco.
	 */
	protected $res;

	/**
	 * @var int Mantém armazenado o índice da linha atual.
	 */
	protected $row;

	/**
	 * @var string Mensagem do último erro ocorrido.
	 */
	protected $msg_erro;

	/**
	 * @var DbSqlBuilder
	 */
	public $sql;

	/**
	 *
	 * @var boolean
	 */
	private $fInTransaction;

	/**
	 * Método construtor da classe de conexão.
	 * Não acessível, usar newInstance para garantir uma única instancia da conexão
	 * O construtor armazena as configurações necessárias e tenta conectar no banco. Caso seja possível conectar, será
	 * criada uma instância do {@link SqlBuilder} para o banco de dados configurado para permitir gerar comandos SQL
	 * multi-banco.
	 *
	 * @param string $sgbd   Indica o SGBD que será usado na conexão. Possíveis valores são definidos em AutoLoadImages{@link SGBD}.
	 * @param string $dbname Nome da base de dados na qual conectar.
	 * @param string $host   Caminho do servidor da base de dados. Exemplo: <tt>localhost</tt>.
	 * @param string $port   Porta para conexão com o banco, exemplo: <tt>5432</tt> no caso do PostgreSQL.
	 * @param string $user   Nome de usuário para acesso ao banco de dados.
	 * @param string $pass   Senha do usuário para acesso ao banco de dados.
	 * @return void
	 */
	private function __construct($sgbd, $dbname, $host, $port, $user, $pass, $force_new = FALSE, $custom_flags = NULL) {
		$sgbds_validos = array('MYSQL', 'MSSQL', 'POSTGRESQL', 'ORACLE', 'FIREBIRD','MONGO');
		if (!in_array($sgbd, $sgbds_validos)) {
			throw new Exception( _t("we_database_nao_suportado_%s", $sgbd) );
		}
		$this->sgbd = $sgbd;
		$this->conecta($dbname, $host, $port, $user, $pass, $force_new, $custom_flags);
	}

	/**
	 * Encerra transação se tiver
	 * Desconecta do BD
	 * @access private
	 * @return void
	 */
	public function __destruct() {
		if ($this->fInTransaction) {
			$this->commitTransaction();
		}
		$this->desconectar();
	}

	/**
	 * @access private
	 * @staticvar resource $Instance
	 * @param string $sgbd
	 * @param string $dbname
	 * @param string $host
	 * @param string $port
	 * @param string $user
	 * @param string $pass
	 * @return \Db
	 */
	public static function newInstance($sgbd = CFG_DRIVER, $dbname = CFG_DBNAME, $host = CFG_HOST, $port = CFG_PORT, $user = CFG_USER, $pass = CFG_PASS) {
		if (isset($GLOBALS['isForked']) && $GLOBALS['isForked']) {
			$pid = posix_getpid();
			static $Instance = null;
			if ($Instance === null) {
				$Instance = array();
			}
			if (!isset($Instance[$pid])) {
				$Instance[$pid] = new Db($sgbd, $dbname, $host, $port, $user, $pass, TRUE);
			}
			return $Instance[$pid];
		} else {
			static $Instance = null;
			if ($Instance === null) {
				$Instance = new Db($sgbd, $dbname, $host, $port, $user, $pass);
			}
			return $Instance;
		}
	}
	/**
	 * Gera um UID similar a 2c3e634d628c04792a551ba4d62f7574
	 * @param boolean $escape
	 * @return string
	 */
	public function genId($escape = false) {
		$id = md5(uniqid(rand(), true));
		if ($escape) {
			$id = $this->escape($id);
		}
		return $id;
	}

	/**
	 * TODO Testar e incluir outros bancos
	 * Retorna se conexão esta ok
	 * @return boolean
	 */
	protected function statusConexao() {
		if ($this->sgbd == "POSTGRESQL") {
			return pg_connection_status($this->id);
		} else if ($this->sgbd == "MYSQL") {
			return @mysqli_ping($this->id);
		} else if ($this->sgbd == "MSSQL") {

		} else if ($this->sgbd == "ORACLE") {

		} else if ($this->sgbd == "FIREBIRD") {

		} else {

		}
		return TRUE;
	}

	/**
	 * TODO Testar e incluir outros bancos
	 * Tenta reconectar no banco de dados, utilizando o identificador conexão já existente.
	 * @return boolean Retorna <tt>true</tt> quando conseguir reconectar com sucesso, senão retorna <tt>false</tt>.
	 */
	protected function reconectar() {
		if ($this->sgbd == "POSTGRESQL") {
			return pg_connection_reset($this->id);
		} else if ($this->sgbd == "MYSQL") {
			@mysqli_close($this->id);
			$this->id = @mysqli_connect($this->servidor, $this->usuario, $this->senha);
			if ($this->id) {
				mysqli_select_db($this->id, $this->nome_banco);
				mysqli_set_charset($this->id, "utf8");

				return true;
			}
			return false;
		} else if ($this->sgbd == "MSSQL") {

		} else if ($this->sgbd == "ORACLE") {

		} else if ($this->sgbd == "FIREBIRD") {

		} else {

		}
		return TRUE;
	}

	/**
	 * Desconecta do banco de dados.
	 * @return boolean
	 */
	protected function desconectar() {
		if (!$this->id)
			return true;
		switch ($this->sgbd) {
			case 'POSTGRESQL':
				return pg_close($this->id);
			case 'MYSQL':
				return mysqli_close($this->id);
			case 'MSSQL':
				return mssql_close($this->id);
			case 'ORACLE':
				return oci_close($this->id);
			case 'FIREBIRD':
				return ibase_close($this->id);
			case 'MONGO' :
				return $this->id->close();
		}
	}

	/**
	 * Método para a conexão com a base de dados.
	 *
	 * @param string $dbname Nome da base de dados na qual conectar.
	 * @param string $host   Caminho do servidor da base de dados. Exemplo: <tt>localhost</tt>.
	 * @param string $port   Porta para conexão com o banco, exemplo: <tt>5432</tt> no caso do PostgreSQL.
	 * @param string $user   Nome de usuário para acesso ao banco de dados.
	 * @param string $pass   Senha do usuário para acesso ao banco de dados.
	 * @param boolean $force_new Cria uma nova conexão, necessário em processos com fork
	 * @param mixed $custom_flags Permite passar flags especificas de conexão
	 * @return boolean Retorna <tt>true</tt> se conseguiu conectar, <tt>false</tt> em caso de falha.
	 */
	protected function conecta($dbname, $host, $port, $user, $pass, $force_new = FALSE, $custom_flags = NULL) {
		$this->nome_banco = $dbname;
		$this->servidor = $host;
		$this->usuario = $user;
		$this->senha = $pass;

		if ($this->sgbd == "POSTGRESQL") {
			if ($custom_flags === NULL) {
				$this->id = pg_connect("host={$host} port={$port} dbname={$dbname} user={$user} password={$pass} sslmode=disable connect_timeout=10");
			} else {
				if ($force_new) {
					$custom_flags |= PGSQL_CONNECT_FORCE_NEW;
				}
				$this->id = pg_connect("host={$host} port={$port} dbname={$dbname} user={$user} password={$pass} sslmode=disable connect_timeout=10", $custom_flags);
			}
			if ($this->id) {
				pg_set_client_encoding($this->id, "UNICODE");
			}
		} else if ($this->sgbd == "MYSQL") {
			if (!extension_loaded("mysqli")) {
				throw new Exception("Install mysqli module on PHP");
			}
//			ini_set('mysql.connect_timeout', '10');
			$this->id = mysqli_connect($host, $user, $pass, $dbname, $port);

			if ($this->id) {// Define o banco de dados ( $nome_banco ) ativo no servidor que é associado ao identificador da conexão( $this->id ) especificado.
//				mysqli_select_db($this->id, $dbname);
				mysqli_set_charset($this->id, "utf8");
				mysqli_autocommit($this->id, FALSE);
				mysqli_options($this->id, MYSQLI_OPT_CONNECT_TIMEOUT, 10);
			}
		} else if ($this->sgbd == "MSSQL") {// Verifica se o banco utilizado é MSSQL e realiza a conexão com o mesmo.
			ini_set('mssql.charset', 'UTF-8');
//			ini_set('mssql.datetimeconvert', 'Off');//Manter comentado
			$this->id = mssql_connect($host, $user, $pass, $force_new);

			if ($this->id) {
				mssql_select_db($dbname, $this->id); // Define o banco de dados ( $nome_banco ) ativo no servidor que é associado ao identificador da conexão( $this->id ) especificado.
			}
		} else if ($this->sgbd == "ORACLE") {
			if ($force_new) {
				$this->id = oci_new_connect($user, $pass, $host, 'AL32UTF8');
			} else {
				$this->id = oci_connect($user, $pass, $host, 'AL32UTF8');
			}
			if ($this->id) {
				$this->we_executa("ALTER SESSION SET NLS_DATE_FORMAT = 'YYYY-MM-DD'");
				$this->we_executa("ALTER SESSION SET NLS_TIMESTAMP_FORMAT = 'YYYY-MM-DD HH24:MI:SS'");
			}
		} else if ($this->sgbd == "FIREBIRD") {
			//dbname precisa contemplar o host e o db
			$this->id = ibase_connect($dbname, $user, $pass, "UTF-8");
		} else if ($this->sgbd == "MONGO") {
			if ($host) {
				$mongo = new MongoClient($host);
			} else {
				$mongo = new MongoClient();
			}
			$this->db_instance = $mongo->selectDB($dbname);
			$this->id = $mongo;
		}
		return (!!$this->id);
	}

	/**
	 * Retorna ultimo erro da conexão
	 * @param resource $res
	 * @return string Mensagem de erro
	 */
	protected function erro($res = NULL) {
		if ($this->msg_erro) {
			return $this->msg_erro;
		}
		switch ($this->sgbd) {
			case 'POSTGRESQL':
				return pg_last_error($this->id);
			case 'MYSQL':
				return mysqli_error($this->id);
			case 'MSSQL':
				return mssql_get_last_message();
			case 'ORACLE'://O erro pode estar na conexão ou na query
				$err = '';
				$err1 = oci_error($this->id);
				if ($err1) {
					$err .= $err1['message'] . "\n";
				}
				if ($res) {
					$err2 = oci_error($res);
					if ($err2) {
						$err .= $err2['message'] . "\n";
					}
				}
				return $err;
			case 'FIREBIRD':
				return ibase_errmsg();
			case 'MONGO':
				$lastError = $this->id->lastError();
				if ($lastError && $lastError['err']) {
					return $lastError['err'];
				} else {
					return "[MONGO] "._t("Undefined error");
				}
		}
	}

	/**
	 * Escapa valores de campos para serem usados no SQL
	 * @param string $value
	 * @return string
	 */
	public function escape($value) {
		if ($value === NULL) {
			return 'NULL';
		} else {
			return "'" . $this->quotedStr($value) . "'";
		}
	}

	/**
	 * Escapa nome de tabela e de campos para serem usados no SQL
	 * @deprecated
	 * @param string $key
	 * @return string
	 */
	public function escapeKey($key) {
		return preg_replace('/\s+/', '', $key);
	}

	/**
	 * Escapa chave e valor ao mesmo tempo
	 * @deprecated
	 * @param string $key
	 * @param string $value
	 * @return string[]
	 */
	public function escapeKeyValue($key, $value) {
		return array($this->escapeKey($key), $this->escape($value));
	}

	/**
	 * Utiliza a função específica do bd para arrumar o texto para inserir no banco de dados
	 *
	 * @param string $atext Texto para receber slash ou quote
	 * @return string
	 * @throws Exception
	 */
	public function quotedStr($atext) {
		switch ($this->sgbd) {
			case 'POSTGRESQL':
				return pg_escape_string($atext);
			case 'MYSQL':
				return mysqli_real_escape_string($this->id, $atext);
			case 'ORACLE':
			case 'MSSQL':
			case 'FIREBIRD':
				/* Não existe uma especifica para MSSQL e ORACLE, apenas substituir ' por '' deve resolver */
				return str_replace("'", "''", $atext);
			default :
				return $atext;
		}
	}

	/**
	 * Utiliza a função específica do bd para arrumar o texto para inserir no banco de dados
	 * @param string $atext Texto para receber slash ou quote
	 * @return string
	 */
	public function quot($atext) {
		return $this->sql->quotedStr($atext);
	}

	/**
	 * Retorna quotedStr em uppercase
	 * @param string $text
	 * @return string
	 */
	public function quotedStrUpper($text) {
		$upper = mb_strtoupper(trim($text), "UTF-8");
		return $this->quotedStr($upper);
	}

	/**
	 * Retorna quotedStr em lowercase
	 * @param string $text
	 * @return string
	 */
	public function quotedStrLower($text) {
		$lower = mb_strtolower(trim($text), "UTF-8");
		return $this->quotedStr($lower);
	}

	/**
	 * Começa uma transação
	 * @return boolean
	 */
	public function startTransaction() {
		$this->fInTransaction = TRUE; //Variavel que define se esta em transação
		switch ($this->sgbd) {
			case 'POSTGRESQL':
				return !!$this->execSql('BEGIN;');
			case 'MYSQL':
				return !!mysqli_begin_transaction($this->id);
//				return !!$this->execSql('BEGIN;');
			case 'MSSQL':
				return !!$this->execSql('BEGIN TRANSACTION;');
			case 'ORACLE':
				//Não há comando para iniciar
				return true;
			case 'FIREBIRD':
				return !!ibase_trans(IBASE_DEFAULT, $this->id);
			case 'MONGO':
				//TODO Mongo transaction
				throw new Exception(_t("Unsupported"));
//				return ;
		}
	}
	/**
	 * Inicia transação
	 * @return boolean
	 */
	public function commitTransaction() {
		$this->fInTransaction = FALSE; //Variavel que define se esta em transação
		switch ($this->sgbd) {
			case 'POSTGRESQL':
				return !!$this->execSql('COMMIT;');
			case 'MYSQL':
				return !!mysqli_commit($this->id);
			case 'MSSQL':
				return !!$this->execSql('COMMIT;');
			case 'ORACLE':
				return !!oci_commit($this->id);
			case 'FIREBIRD':
				return !!ibase_commit($this->id);

		}
	}
	/**
	 * Desfaz uma transação
	 * @return string
	 */
	public function rollbackTransaction() {
		$this->fInTransaction = FALSE; //Variavel que define se esta em transação
		switch ($this->sgbd) {
			case 'POSTGRESQL':
				return !!$this->execSql('ROLLBACK;');
			case 'MYSQL':
				return !!mysqli_rollback($this->id);
			case 'MSSQL':
				return !!$this->execSql('ROLLBACK;');
			case 'ORACLE':
				return !!oci_rollback($this->id);
			case 'FIREBIRD':
				return !!ibase_rollback($this->id);
			case 'MONGO':
				//TODO Mongo transaction
				throw new Exception(_t("Unsupported"));
//				return ;
		}
	}

	/**
	 * Função que faz o travamento de uma tabela de a nivel de registro.

	 * @param type $table - Tabela que será travada.
	 * @return type
	 *
	 * @notes
	 * 	- Este recurso deve ser usado apenas para SELECT.
	 * 	- Uma tabela só deve ser travada ao ser iniciada uma transação, pois este
	 * recurso é usado para que nenhuma outra transação atualize registros da
	 * tabela travada enquanto a transação corrente busca registros na tabela.
	 * 	- Ao termino da consulta a transação deve ser finalizada para que a tabela


	 * possa ser utilizada pelas outras transações.
	 *
	 * @todo
	 * 	- Implementar futuramente uma função que possa ser passado o modo de travamento
	 * da tabela e a função retorne a string com o respectivo comando montado.
	 */
	public function sqlLockTableModeRowExclusive($table) {
		switch ($this->sgbd) {
			case 'POSTGRESQL':
				return $this->execSql(" LOCK TABLE {$table} IN SHARE ROW EXCLUSIVE MODE; ");
			case 'MYSQL':
				return $this->execSql(" LOCK TABLE {$table} WRITE; ");
			case 'MSSQL':
				return $this->execSql(" LOCK TABLE {$table} IN SHARED ROWLOCK; ");
			case 'ORACLE':
				return $this->execSql(" LOCK TABLE {$table} IN SHARE ROW EXCLUSIVE MODE; ");
			case 'FIREBIRD':
				//TODO
				throw new Exception(_t("Unsupported"));
				break;
			case 'MONGO':
				//TODO Mongo lock
				throw new Exception(_t("Unsupported"));
//				return ;
		}
	}

	/**




	 * Função que faz o travamento de um registro de uma tabela pela PK.
	 * @param type $tabela - Tabela na qual será travado o registro.
	 * @param type $colunasConsulta - Nome das colunas do select separados por vírgula.
	 * @return type $nomePk - Nome da PK da tabela que será realizado o lock.
	 * @return type $valorPk - Valor da PK que será utilizado como filtro.
	 *
	 * @notes
	 * 	- Este recurso deve ser usado apenas para SELECT.
	 * 	- Um registro só deve ser travado ao ser iniciada uma transação.
	 *  - O travamento não permite que o valor do registro seja alterado por outra transação.
	 *  - Toda transação que requisitar o registro travado irá aguardar até que seja realizada
	 *  uma operação de commit ou rollback para liberar o registro.
	 */
	public function sqlLockRow($tabela, $colunasConsulta, $nomePk, $valorPk) {
		switch ($this->sgbd) {
			case 'POSTGRESQL':
			case 'MYSQL':
			case 'ORACLE':
				return "select {$colunasConsulta} from {$tabela} where $nomePk = '{$valorPk}' for update";
				break;
			case 'MSSQL':
				return "select {$colunasConsulta} from {$tabela} with(updlock) where $nomePk = '{$valorPk}'";
				break;
			case 'FIREBIRD':
				//TODO
				throw new Exception(_t("Unsupported"));
//				return ;
			case 'MONGO':
				//TODO Mongo lock
				throw new Exception(_t("Unsupported"));
//				return ;
		}
	}

	/**
	 * Retorna primeiro registro de forma associativa
	 * @param string $sql Consulta a ser executada.
	 * @return mixed[]
	 */
	public function fetch($sql) {
		return $this->getSqlAssoc($sql);
	}

	/**
	 * retorna primeira linha e primeira coluna do registro
	 * Ex: SELECT id FROM ARGOS_TERMI, vai retornar o id do terminal
	 * @param string $sql Consulta a ser executada.
	 * @return mixed
	 */
	public function fetchColumn($sql) {
		$res = $this->we_executa($sql);
		if ($res) {
			$return = $this->we_fetchNum($res);
			return $return[0];
		}
		return NULL;
	}

	/**
	 * Cria e executa uma consulta
	 * Deve ser usado em consultas que retornam apenas um registro
	 * @param string $sql Consulta a ser executada.
	 * @return mixed[]
	 * @throws Exception
	 */
	public function getSql($sql) {
		$res = $this->we_executa($sql);
		if ($res) {
			return $this->we_fetch($res);
		}
		return NULL;
	}

	/**
	 * Registros afetados por INSERT, UPDATE ou DELETE
	 * @param resource $res
	 * @return int
	 * @throws Exception
	 */
	private function affected($res) {
		if ($this->sgbd == "POSTGRESQL") {
			return pg_affected_rows($res);
		} elseif ($this->sgbd == "MYSQL") {
			return mysqli_affected_rows($this->id); //Atenção é usado o link de conexão
		} elseif ($this->sgbd == "MSSQL") {
			return mssql_rows_affected($this->id); //Atenção é usado o link de conexão
		} elseif ($this->sgbd == "ORACLE") {
			return oci_num_rows($res);
		} elseif ($this->sgbd == "FIREBIRD") {
			return ibase_affected_rows($this->id); //Atenção é usado o link de conexão
		} else {
			throw new Exception("Driver SGDB: " . get_class($this) . ">" . __FUNCTION__);
		}
	}

	/**
	 * Executa um SQL que não retorna dados
	 * @param string $sql Consulta a ser executada.
	 * @return int Registros afetados
	 * @throws Exception
	 */
	public function execSql($sql) {
		$res = $this->we_executa($sql);
		if ($res) {
			$affected = $this->affected($res);

			if (!$this->fInTransaction) {//Autocommit desabilitado controlar aqui depois de coletar affected
				if ($this->sgbd == "MYSQL") {
					mysqli_commit($this->id);
				}
			}

			return $affected;
		}
		return NULL;
	}

	/**
	 * Cria e executa uma consulta, retorna array associativo
	 * Deve ser usado em consultas que retornam apenas um registro
	 * @param string $sql Consulta a ser executada.
	 * @return mixed[]
	 * @throws Exception Quando houver erro na consulta
	 */
	public function getSqlAssoc($sql) {
		$res = $this->we_executa($sql);
		if ($res) {
			return $this->we_fetchAssoc($res);
		}
		return NULL;
	}

	/**
	 * Cria e executa uma consulta
	 * Retorna um array multi dimensional com todos os registros retornados pelo BD
	 * Utiliza o numeor como indice
	 * Utiliza o nome dos campos como indice
	 * Ex.: $rows[0][0] é igual a primeira linha e primeira coluna
	 *      $rows[0]['nome_campo'] é igual a primeira linha do campo 'nome_campo'
	 * @param string $sql Consulta a ser executada.
	 * @return mixed[]
	 * @throws Exception Quando houver erro na consulta
	 */
	public function getArrSql($sql) {
		$res = $this->we_executa($sql);
		if ($res) {
			$arr = array();
			while ($row = $this->we_fetch($res)) {
				$arr[] = $row;
			}
			$this->freeResult($res);
			return $arr;
		}
		return NULL;
	}

	/**
	 * Facilita o insert ja utilizando um array para insercao
	 *
	 * @param string[] $arr Array associativo com chave e valor do insert
	 * @param string $table Nome da tabela
	 * @param string $key Nome da chave da tabela
	 * @param boolean $html_safe Se escapa o html para evitar XSS
	 * @return boolean
	 */
	public function arrayToInsert($arr, $table, $key, $html_safe = true) {
		$fields = array($this->escapeKey($key));
		$values = array($this->genId(true));
		foreach ($arr as $k => $v) {
			$fields[] = $this->escapeKey($k);
			$values[] = $html_safe ? htmlspecialchars($this->escape($v)) : $this->escape($v);
		}
		$fields = implode(", ", $fields);
		$values = implode(", ", $values);
		return !!$this->executa("INSERT INTO {$this->escapeKey($table)} ({$fields}) VALUES ({$values})");
	}

	/**
	 * Cria e executa uma consulta, array associativo
	 * Retorna um array multi dimensional com todos os registros retornados pelo BD
	 * @param string $sql Consulta a ser executada.
	 * @return mixed[]
	 * @throws Exception Quando houver erro na consulta
	 */
	public function getArrSqlAssoc($sql) {
		$res = $this->we_executa($sql);
		if ($res) {
			$arr = array();
			while ($row = $this->we_fetchAssoc($res)) {
				$arr[] = $row;
			}
			$this->freeResult($res);
			return $arr;
		}
		return NULL;
	}

	/**
	 * Cria e executa uma consulta, array numerico
	 * Retorna um array multi dimensional com todos os registros retornados pelo BD
	 * @param string $sql Consulta a ser executada.
	 * @return mixed[]
	 * @throws Exception Quando houver erro na consulta
	 */
	public function getArrSqlNum($sql) {
		$res = $this->we_executa($sql);
		if ($res) {
			$arr = array();
			while ($row = $this->we_fetchNum($res)) {
				$arr[] = $row;
			}
			$this->freeResult($res);
			return $arr;
		}
		return NULL;
	}

	/**
	 * Realiza fetch do BD do ponteiro atual
	 * @param resource $res
	 * @return mixed[]
	 * @throws Exception
	 */
	public function we_fetch($res) {
		if ($this->sgbd == "POSTGRESQL") {
			return pg_fetch_array($res);
		} else if ($this->sgbd == "MYSQL") {
			return mysqli_fetch_array($res);
		} else if ($this->sgbd == "MSSQL") {
			return mssql_fetch_array($res);
		} else if ($this->sgbd == "ORACLE") {
			$data = oci_fetch_array($res, OCI_ASSOC | OCI_RETURN_LOBS | OCI_NUM);
			if ($data)
				$data = array_change_key_case($data, CASE_LOWER);
			return $data;
		} else if ($this->sgbd == "FIREBIRD") {
			$data = ibase_fetch_row($res, IBASE_TEXT);
			if ($data)
				$data = array_change_key_case($data, CASE_LOWER);
			return $data;
		} else {
			throw new Exception("Driver SGDB: " . get_class($this) . ">" . __FUNCTION__);
		}
	}

	/**
	 * Realiza fetch do BD do ponteiro atual
	 * @param resource $res
	 * @return mixed[]
	 * @throws Exception
	 */
	public function we_fetchAssoc($res) {
		if ($this->sgbd == "POSTGRESQL") {
			return pg_fetch_assoc($res);
		} else if ($this->sgbd == "MYSQL") {
			return mysqli_fetch_assoc($res);
		} else if ($this->sgbd == "MSSQL") {
			return mssql_fetch_assoc($res);
		} else if ($this->sgbd == "ORACLE") {
			$data = oci_fetch_array($res, OCI_RETURN_LOBS | OCI_ASSOC);
			if ($data)
				$data = array_change_key_case($data, CASE_LOWER);
			return $data;
		} else if ($this->sgbd == "FIREBIRD") {
			$data = ibase_fetch_assoc($res, IBASE_TEXT);
			if ($data)
				$data = array_change_key_case($data, CASE_LOWER);
			return $data;
		} else {
			throw new Exception("Driver SGDB: " . get_class($this) . ">" . __FUNCTION__);
		}
	}

	/**
	 * Realiza fetch do BD do ponteiro atual
	 * @param resource $res
	 * @return mixed[]
	 * @throws Exception
	 */
	public function we_fetchNum($res) {
		if ($this->sgbd == "POSTGRESQL") {
			return pg_fetch_row($res);
		} else if ($this->sgbd == "MYSQL") {
			return mysqli_fetch_row($res);
		} else if ($this->sgbd == "MSSQL") {
			return mssql_fetch_array($res, MSSQL_NUM);
		} else if ($this->sgbd == "ORACLE") {
			return oci_fetch_array($res, OCI_RETURN_LOBS | OCI_NUM);
		} else if ($this->sgbd == "FIREBIRD") {
			return ibase_fetch_row($res, IBASE_TEXT);
		} else {
			throw new Exception("Driver SGDB: " . get_class($this) . ">" . __FUNCTION__);
		}
	}

	/**
	 * Conta o resultado de uma consulta
	 * @param resource $res
	 * @param string $sql
	 * @return int
	 * @throws Exception
	 */
	public function we_recordCount($res, $sql = NULL) {
		if ($this->sgbd == "POSTGRESQL") {
			return pg_num_rows($res);
		} else if ($this->sgbd == "MYSQL") {
			return mysqli_num_rows($res);
		} else if ($this->sgbd == "MSSQL") {
			return mssql_num_rows($res);
		} else if ($this->sgbd == "ORACLE") {
			return $this->fetchColumn("select count(*) as we_count from ($sql) ");
//			return oci_num_rows($res);//Não funciona em selects
		} else if ($this->sgbd == "FIREBIRD") {
			return $this->fetchColumn("select count(*) as we_count from ($sql) ");
		} else {
			throw new Exception("Driver SGDB: " . get_class($this) . ">" . __FUNCTION__);
		}
	}

	/**
	 * Posiciona ponteiro no BD
	 * @param resource $res
	 * @param int $aPos
	 * @return boolean
	 * @throws Exception
	 */
	public function we_seek($res, $aPos) {
		if ($this->sgbd == "POSTGRESQL") {
			return pg_result_seek($res, $aPos);
		} else if ($this->sgbd == "MYSQL") {
			return mysqli_data_seek($res, $aPos);
		} else if ($this->sgbd == "MSSQL") {
			return mssql_data_seek($res, $aPos);
		} else if ($this->sgbd == "ORACLE") {
			//Oracle não tem, deste jeito fica unidirecional
			return true;
		} else if ($this->sgbd == "FIREBIRD") {
			return true;
		} else {
			throw new Exception("Driver SGDB: " . get_class($this) . ">" . __FUNCTION__);
		}
	}

	/**
	 * Método que executa o comando sql na base de dados.
	 * Todo o acesso ao BD passa aqui
	 * @param string $sql
	 * @return mixed
	 * @throws Exception
	 */
	public function we_executa($sql) {
		$res = null;
		if ($this->sgbd == "POSTGRESQL") {
			$res = @pg_query($this->id, $sql);
		} else if ($this->sgbd == "MYSQL") {
			$res = @mysqli_query($this->id, $sql);
		} else if ($this->sgbd == "MSSQL") {
			try {
				$res = mssql_query($sql, $this->id);
			} catch (Exception $e) {
				$sql = preg_replace("/\t+/", " ", $sql);
				$err = $e->getMessage();
				throw new Exception(_t("we_sql_error_%s", $err) . "\n{$sql}");
			}
		} else if ($this->sgbd == "ORACLE") {
			$res = oci_parse($this->id, $sql);

			if ($this->fInTransaction) {
				$mode = OCI_NO_AUTO_COMMIT;
			} else {
				$mode = OCI_COMMIT_ON_SUCCESS;
			}
			if (!@oci_execute($res, $mode)) {//Retorna boolean
				$sql = preg_replace("/\t+/", " ", $sql);
				$err = $this->erro($res);
				throw new Exception(_t("we_sql_error_%s", $err) . "\n{$sql}");
			}
		} else if ($this->sgbd == "FIREBIRD") {
			$res = @ibase_query($this->id, $sql);
		} else {
			throw new Exception("Driver SGDB: " . get_class($this) . ">" . __FUNCTION__);
		}
		if (!$res) {
			$err = $this->erro($res);
			throw new Exception(_t("we_sql_error_%s", $err) . "\n{$sql}");
		}
		return $res;
	}

	/**
	 * Cria e executa uma consulta, array associativo
	 * Retorna uma instancia de um Iterator
	 * @param string $sql Consulta a ser executada.
	 * @param mixed[] $controls
	 * @return mixed[]
	 * @throws Exception Quando houver erro na consulta
	 */
	public function we_getArrSqlAssoc3($sql, $controls = array()) {
		$res = $this->we_executa($sql);
		if ($res) {
			return new we_getArrSqlAssoc3Iterator($this, $res, $controls, $sql);
		}
		return NULL;
	}

	/**
	 * Libera os recursos usados
	 * @param resource $res
	 * @return void
	 */
	public function freeResult($res) {
		if (!$res)
			return;
		switch ($this->sgbd) {
			case 'POSTGRESQL':
				return pg_free_result($res);
			case 'MYSQL':
				return mysqli_free_result($res);
			case 'MSSQL':
				return mssql_free_result($res);
			case 'ORACLE':
				return oci_free_statement($res);
			case 'FIREBIRD':
				return ibase_free_result($res);
		}
	}


}
class we_getArrSqlAssoc3Iterator implements Countable, Iterator, ArrayAccess {

	/**
	 *
	 * @var mixed
	 */
	protected $fRes;

	/**
	 *
	 * @var int
	 */
	protected $fIdx;

	/**
	 *
	 * @var int
	 */
	protected $fCount;

	/**
	 *
	 * @var Db
	 */
	protected $fOwner;

	/**
	 *
	 * @var array
	 */
	protected $fControls = array();

	function __construct($owner, $res, $controls, $sql = NULL, $idusuario = NULL) {
		$this->fOwner = $owner;
		$this->fRes   = $res;
		$this->fCount = $this->fOwner->we_recordCount($this->fRes, $sql);
	}

	public function __destruct() {
		if (isset($this->fOwner))
			$this->fOwner->freeResult($this->fRes);
	}
	public function count() {
		return $this->fCount;
	}

	public function current() {
		$this->fOwner->we_seek($this->fRes, $this->fIdx);
		return $this->fOwner->we_fetchAssoc($this->fRes);
	}

	public function key() {
		return $this->fIdx;
	}

	public function next() {
		$this->fIdx++;
	}

	public function rewind() {//Começo do array...
		$this->fIdx = 0;
	}

	public function valid() {
		if ($this->fIdx < $this->fCount) {
			return True;
		} else {
			return False;
		}
	}

	public function offsetExists($offset) {
		return ($offset < $this->fCount && $offset >= 0);
	}

	public function offsetGet($offset) {
		if ($this->fOwner->we_seek($this->fRes, $offset)) {
			return $this->fOwner->we_fetchAssoc($this->fRes);
		} else {
			return null;
		}
	}

	public function offsetSet($offset, $value) {
		throw new Exception(__METHOD__ . ' não suportado.');
	}

	public function offsetUnset($offset) {
		throw new Exception(__METHOD__ . ' não suportado.');
	}

}
class we_WhereOp {
	/**
	 * @var Db
	 */
	protected $fCon;
	protected $v;
	public function __construct($aCon, $v) {
		if ($aCon instanceof we_Read) {
			$this->fCon = $aCon->fCon;
		} else {
			$this->fCon = $aCon;
		}
		$this->v    = $v;
	}
	public function convert() {
		return $this->v;
	}
}
class we_WhereOpLike extends we_WhereOp {

	public function __toString() {
		return "LIKE '%" . $this->fCon->quotedStr($this->v) . "%'";
	}
	public function convert() {
		return new MongoRegex("/".preg_quote($this->v,"/")."/");
	}
}
class we_WhereOpILike extends we_WhereOp {

	public function __toString() {
		return "ILIKE '%" . $this->fCon->quotedStr($this->v) . "%'";
	}
	public function convert() {
		return new MongoRegex("/".preg_quote($this->v,"/")."/i");
	}
}
class we_WhereOpLikeStarting extends we_WhereOp {
	public function __toString() {
		return "LIKE '%" . $this->fCon->quotedStr($this->v) . "'";
	}
	public function convert() {
		return new MongoRegex("/^".preg_quote($this->v,"/")."/");
	}
}
class we_WhereOpILikeStarting extends we_WhereOp {
	public function __toString() {
		return "ILIKE '%" . $this->fCon->quotedStr($this->v) . "'";
	}
	public function convert() {
		return new MongoRegex("/^".preg_quote($this->v,"/")."/i");
	}
}
abstract class we_DBCommon {
	/**
	 * @var Db
	 */
	public $fCon;
	protected $fWheres    = array();
	protected $fTablename = '';
	protected $fDic;
	protected $fDicFields = array();
	protected $fDicTablename;

	/**
	 * Permite adicionar where customizado
	 * @param mixed $param Ex1.: campo = 'valor' Ex.2: array("campo"=>"valor")
	 * @return \we_Read
	 */
	public function whereOR($param) {
		if (is_array($param)) {
			foreach ($param as $k=>$v) {
				if ($v instanceof we_WhereOp) {
					$this->fWheres[] = array("$k $v", 'AND');
				} else if ($v===NULL) {
					$this->fWheres[] = array("$k IS NULL", 'OR');
				} else {
					$this->fWheres[] = array("$k = ".$this->fCon->escape($v), 'OR');
				}
			}
		} else {
			$this->fWheres[] = array($param, 'OR');
		}
		return $this;
	}

	/**
	 * Permite adicionar where customizado
	 * @param mixed $param Ex1.: campo = 'valor' Ex.2: array("campo"=>"valor")
	 * @return \we_Read
	 */
	public function whereAND($param) {
		if (is_array($param)) {
			foreach ($param as $k=>$v) {
				if ($v instanceof we_WhereOp) {
					$this->fWheres[] = array("$k $v", 'AND');
				} else if ($v===NULL) {
					$this->fWheres[] = array("$k IS NULL", 'AND');
				} else {
					$this->fWheres[] = array("$k = ".$this->fCon->escape($v), 'AND');
				}
			}
		} else {
			$this->fWheres[] = array($param, 'AND');
		}
		return $this;
	}
	/**
	 * Limpa a construção atual do WHERE, permitindo reaproveitar o objeto multiplas vezes
	 * @return \we_DBCommon
	 */
	public function whereClear() {
		$this->fWheres = array();
		return $this;
	}

	public function whereIn($field, $instance, $op = "AND") {
		$method = "where" . $op;
		$this->$method("{$field} IN ({$instance->AsSQL()})");
		return $this;
	}

	public function whereNotIn($field, $instance, $op = "AND") {
		$method = "where" . $op;
		$this->$method("{$field} NOT IN ({$instance->AsSQL()})");
		return $this;
	}

	protected function _checkTable($aTablename) {
		if (!$this->fDic)
			return;
		if (strcasecmp($aTablename, $this->fDicTablename) <> 0) {
			throw new Exception("dic error tablename");
		}
	}

	protected function _checkField($aFieldname) {
		if (!$this->fDic)
			return;
		if (!in_array($aFieldname, $this->fDicFields)) {
			throw new Exception("dic error fieldname");
		}
	}

	protected function _where() {
		if (!empty($this->fWheres)) {
			$lWhere = "WHERE 1=1\n";
			foreach ($this->fWheres as $Where) {
				$lWhere .= "{$Where[1]} ($Where[0])\n";
			}
			return $lWhere;
		}
		return '';
	}
}

class we_Read extends we_DBCommon{

	public $num_results = 0;
	private $fControls;
	private $fFields = '';
	private $fGroup = '';
	private $fJoin = '';
	private $fOrder = '';
	private $fPageOffset;
	private $fPageCount;
	private $fCount;

	public function __construct($aCon = NULL, $aDic = NULL) {
		if ($aCon) {
			$this->fCon = $aCon;
		} else {
			$this->fCon = Db::newInstance();
		}

		$this->fDic = $aDic;
		if ($this->fDic) {
			foreach ($this->fDic->columns as $lDicField) {
				$this->fDicFields[] = $lDicField->name;
			}
			$this->fDicTablename = $this->fDic->name;
		}
	}

	private function _order() {
		if ($this->fOrder) {
			return " " . trim(trim($this->fOrder), ",") . " ";
		}
		return '';
	}

	/**
	 * Atribuir os controles de um layout para executar
	 * @param type $aControls
	 * @return \we_Read
	 */
	public function setControls($aControls) {
		$this->fControls = $aControls;
		return $this;
	}

	/**
	 * Seleção dos campos
	 * @param string|string[] $aFields
	 * @return \we_Read
	 */
	public function select($aFields = NULL) {
		if ($aFields === '*') {
			$this->fFields = '*';
			return $this;
		}

		if (is_string($aFields)) {
			$aFields = preg_split("/\s*,\s*/", $aFields, -1, PREG_SPLIT_NO_EMPTY);
		}
		if (is_array($aFields)) {
			if (empty($aFields)) {
				$this->fFields = '*';
			} else {
//				array_map( array($this,"_checkField"), $aFields);
				array_walk($aFields, function(&$item) {
					$this->_checkField($item);
					if ($item instanceof we_ReadCase) {
						$item = $item->AsSQL();
					}
				});
				$this->fFields = implode(',', $aFields);
			}
		} else {
			$this->fFields = '*';
		}
		return $this;
	}
	/**
	 * Utiliza função count do SGDB
	 * @return \we_Read
	 */
	public function selectCount() {
		$this->fFields = 'COUNT(*)';
		return $this;
	}
	/**
	 * Utiliza função SUM do SGDB
	 * @return \we_Read
	 */
	public function selectSum($fieldname) {
		$this->fFields = "SUM({$fieldname}) as f1";
		return $this;
	}

	public function join($aTablename, $on, $dir = "") {
		$dir = strtoupper($dir);
		$this->fJoin .= " {$dir} JOIN {$aTablename} ON {$on} ";
		return $this;
	}

	public function joinClear(){
		$this->fJoin = "";
		return $this;
	}

	public function leftJoin($aTablename, $on) {
		$this->fJoin .= " LEFT JOIN {$aTablename} ON {$on} ";
		return $this;
	}
	/**
	 * Define qual tabela será utilizada na construção do SQL
	 * @param string $aTablename
	 * @return \we_Read
	 */
	public function from($aTablename) {
		$this->_checkTable($aTablename);
		$this->fTablename = $aTablename;
		return $this;
	}

	public function filterAND($aFilter) {
		return $this->filter($aFilter, "AND");
	}

	public function filterOR($aFilter) {
		return $this->filter($aFilter, "OR");
	}

	/**
	 * Aceita string no seguinte formato:
	 * Ex.:
	 * 		'idevento'='xxxxx' 'valor'>='111' 'valor'<='211'
	 * 		'valor'<>'1', 'campo' like 'valor'
	 * @param string $aFilter
	 * @return \we_Read
	 */
	public function filter($aFilter, $aOp = 'AND') {
		$lWhere = '';
		$lfilters = preg_split("/[\s,]*\\\"([^\\\"]+)\\\"[\s,]*|" . "[\s,]*'([^']+)'[\s,]*|" . "[:\s+]+/", $aFilter, 0, PREG_SPLIT_NO_EMPTY | PREG_SPLIT_DELIM_CAPTURE);
		if (empty($lfilters)) {
			return $this;
		}
		$lState = 1; //Esperando Field
		$op = '='; //Operador default
		foreach ($lfilters as $i => $value) {
			if ($lState === 1) {
//				$this->_checkField($value);
				$orig_fieldname = $value;
				$fieldname = $value;
				$lState = 2; //Esperando op ou valor
//			$fieldname       = str_ireplace(array_keys($aFieldnames), array_values($aFieldnames), $value);
				continue;
			} else {
				if ($lState === 3) {
					$orig_fieldvalue = $value;
					$fieldvalue = $value;
				} else {
					if (preg_match("/\s*^(=|<>|!=|like|>=|<=|<|>|menor\s+que|lt|maior\s+que|gt)$\s*/i", $value)) {//Se for um operador
						if (preg_match("/^\s*<|menor\s+que|lt/i", $value, $m)) {
							$op = "<";
						} else if (preg_match("/^\s*>|maior\s+que|gt/i", $value, $m)) {
							$op = ">";
						} else {
							$op = $value;
						}

						$lState = 3; //Esperando valor
						continue;
					} else {
						if (preg_match("/\s*^(=|<>|!=|like|>=|<=|<|>|menor\s+que|lt|maior\s+que|gt)\s*(.+)/i", $value, $m)) {//Se um operador ficou colado no valor ex.: 'chave':'>valor'
							$op = $m[1];
							$value = $m[2];
						}
						$orig_fieldvalue = $value;
						$fieldvalue = $value;
					}
				}
			}

//			$fieldvalue = preg_replace("/^\s*<|>|maior\s+que|menor\s+que|gt|lt/i", "", $fieldvalue);
			$lWhere .= "AND $fieldname $op '$fieldvalue' ";
			/* Remove do filtro original o q entrar no where */
//			$q1 = preg_quote($orig_fieldname, '/');
//			$q2 = preg_quote($orig_fieldvalue, '/');
//			$ex = "/\s*\\\"*$q1\\\"*\s*:\s*\\\"*.*$q2\\\"*\s*/"; //Remove do filtro com ou sem aspas separando
//			$aFilter = preg_replace($ex, '', $aFilter);

			$lState = 1;
			$op = '=';
		}
		if ($lWhere) {
			$this->fWheres[] = array(substr($lWhere, 4, -1), $aOp);
		}
		return $this;
	}

	/**
	 * Ex.: array("nome_campo_desc"=>1,"nome_campo_asc"=>-1)
	 * @param string[] $aOrder
	 * @return \we_Read
	 */
	public function order($aOrder) {
		if (!is_array($aOrder)) {
			$aOrder = (array) @json_decode($aOrder);
		}
		if (empty($aOrder)) {
			return $this;
		}

		if (!$this->fOrder) {
			$this->fOrder = "ORDER BY ";
		}
		foreach ($aOrder as $f => $sentido) {
			if ((int) $sentido === 1) {
				$this->fOrder .= "{$f} DESC, ";
			} else {
				$this->fOrder .= "{$f}, ";
			}
		}
		return $this;
	}

	/**
	 * Adicionar campo ao order
	 * @param string $aFieldname
	 * @return \we_Read
	 */
	public function orderByASC($aFieldname, $null = null) {
		if (!$aFieldname || !is_string($aFieldname)) {
			return $this;
		}
		if (!$this->fOrder) {
			$this->fOrder = "ORDER BY ";
		}
		if ($null) {
			$this->fOrder .= " {$this->fCon->sql->nullsOrder($aFieldname, "ASC", $null)} ";
			return $this;
		}
		$this->fOrder .= " $aFieldname, ";
		return $this;
	}

	/**
	 * Adicionar campo ao order
	 * @param string $aFieldname
	 * @return \we_Read
	 */
	public function orderByDESC($aFieldname, $null = null) {
		if (!$aFieldname || !is_string($aFieldname)) {
			return $this;
		}
		if (!$this->fOrder) {
			$this->fOrder = "ORDER BY ";
		}
		if (!$this->fOrder) {
			$this->fOrder .= " {$this->fCon->sql->nullsOrder($aFieldname, "DESC", $null)} ";
			return $this;
		}
		$this->fOrder .= " $aFieldname DESC, ";
		return $this;
	}

	public function page($aPage, $aPageSize = 50) {
		if (is_numeric($aPage) && is_numeric($aPageSize) && $aPageSize > 1) {
			$this->fPageCount = (int) $aPageSize;
			$this->fPageOffset = $this->fPageCount * ((int) $aPage - 1);
		}
		return $this;
	}

	public function limit($aValue) {
		if (is_numeric($aValue)) {
			$this->fPageCount = (int) $aValue;
		}
		return $this;
	}

	public function skip($aValue) {
		if (is_numeric($aValue)) {
			$this->fPageOffset = (int) $aValue;
		}
		return $this;
	}

	public function exec($log = false) {
		$it = $this->fCon->we_getArrSqlAssoc3($this->AsSQL(), $this->fControls);

		$this->fCount = $it->count();

		if (isset($this->fPageOffset)) {
			$lPageCount = ( isset($this->fPageCount) && $this->fPageCount > 1) ? $this->fPageCount : -1;
			$it = new we_LimitIterator($it, $this->fPageOffset, $lPageCount);
		} else if (isset($this->fPageCount) && $this->fPageCount >= 1) {
			$it = new we_LimitIterator($it, 0, $this->fPageCount);
		}

		return $it;
	}

	public function count() {
		return $this->fCount;
	}

	/**
	 * Recomendado nao utilizar para retornos com muitos registros.
	 *
	 * @param boolean $log Se vai gravar log ou nao
	 * @return []
	 */
	public function execAsArray($log = false) {
		return iterator_to_array($this->exec($log));
	}

	/**
	 * Monta o SQL e o retorna
	 * @return string
	 * @throws Exception
	 */
	public function AsSQL() {
		$lSql = '';
		if (!$this->fFields)
			throw new Exception('no fields specified');
		$lSql .= "SELECT {$this->fFields} ";
		if (!$this->fTablename)
			throw new Exception(_t('we_no_table_specified'));
		$lSql .= "FROM {$this->fTablename} ";
		$lSql .= $this->fJoin;
		$lSql .= "{$this->_where()}";
		$lSql .= "{$this->_group()}";
		$lSql .= "{$this->_order()}";

		return $lSql;
	}
	/**
	 * Retorna a primeira coluna sem usar iterator, sem verificar permissao e sem registrar log.
	 * @return mixed
	 */
	public function fetchColumn() {
		return $this->fCon->fetchColumn($this->AsSQL());
	}
	/**
	 * Retorna um array associativo sem usar iterator, sem verificar permissao e sem registrar log.
	 *
	 * @return mixed[]
	 */
	public function fetch() {
		return $this->fCon->fetch($this->AsSQL());
	}

	private function _group() {
		if (!$this->fGroup) {
			return "";
		}
		return " GROUP BY " . $this->fGroup . " ";
	}

	public function groupBy($campos) {
		$this->fGroup = implode(", ", $campos);
		return $this;
	}

	/**
	 * Realiza unions entre instancias do we_Read.<br/>
	 * Como retorna tudo em array, recomenda-se utilizar com poucos resultados.
	 *
	 * @param we_Read...
	 * @return []
	 */
	static public function union($args) {
		$ret = array();
		$args = func_get_args();
		foreach ($args as $arg) {
			$ret = array_merge($ret, $arg->execAsArray());
		}
		return $ret;
	}

}

class we_ReadCase {

	protected $con;
	protected $alias;
	protected $field;
	protected $else = null;
	protected $when = array();

	public function __construct($field, $alias = null) {
		$this->con = Db::newInstance();
		$this->alias = $alias;
		$this->field = $field;
	}

	public function when(array $then) {
		foreach ($then as $cond => $val) {
			$this->when[$cond] = $this->con->escape($val);
		}
		return $this;
	}

	public function elseCase($val = null) {

		$this->else = (string) $val;
	}

	public function AsSQL() {
		$sql = "CASE {$this->field} ";

		foreach ($this->when as $cond => $val) {
			$sql .= "WHEN {$cond} THEN {$val} ";
		}

		if ($this->else) {
			$sql .= "ELSE {$this->escape($this->else)} ";
		}

		return $sql . ($this->alias ? " END AS {$this->alias}" : "");
	}

}

/**
 * Classe responsavel por executar as acoes de INSERT, UPDATE e DELETE no banco de dados.
 */
class we_Exec extends we_DBCommon {

	private $_ops    = array("INSERT", "UPDATE", "DELETE");
	private $_vals   = array();
	private $_field  = array();
	private $_val    = array();
	private $_where  = null;
	private $_op     = null;
	private $_escape = true;
	private $_checkSealed = true;

	/**
	 * Construtor, momento de definir a operacao.
	 *
	 * @param \Db $con Instancia de uma conexao
	 * @return \we_Exec
	 * @throws Exception
	 */
	public function __construct($con = null) {

		if ($con) {
			$this->fCon = $con;
		} else {
			$this->fCon = Db::newInstance();
		}


		return $this;
	}

	/**
	 * Se escapa ou não os campos
	 * @param boolean $b
	 * @return \we_Exec
	 */
	public function setEscape($b = true){
		$this->_escape = !!$b;
		return $this;
	}

	/**
	 * false para não checar campo lacrado
	 * @param boolean $b
	 * @return \we_Exec
	 */
	public function setCheckSealed($b) {
		$this->_checkSealed = !!$b;
		return $this;
	}

	private function _escape($str){
		if ($this->_escape){
			return $this->fCon->escape($str);
		}
		return $str;
	}

	/**
	 * Define operacao para insert.
	 *
	 * @return \we_Exec
	 */
	public function insert(){
		$this->_op = "INSERT";
		return $this;
	}

	/**
	 * Define operacao para update.
	 *
	 * @return \we_Exec
	 */
	public function update(){
		$this->_op = "UPDATE";
		return $this;
	}

	/**
	 * Define operacao para delete.
	 *
	 * @return \we_Exec
	 */
	public function delete(){
		$this->_op = "DELETE";
		return $this;
	}

	/**
	 * Array associativo com os campos e valores a serem inseridos ou atualizados.
	 * Nao disponivel para o delete.
	 *
	 * @param Array $vals Array associativo com os valores
	 * @return \we_Exec
	 * @throws Exception
	 */
	public function setValues($vals){
		$this->_vals = array();
		foreach ($vals as $field => $val){
			$fi = $this->fCon->escapeKey($field);
			$tv = is_array($val) ? implode(";", $val) : $val;
			$va = $this->_escape($tv);
			$this->_vals[$fi] = $va;
		}
		return $this;
	}

	/**
	 * Define a tabela para operacao.
	 *
	 * @param String $table Nome da tabela
	 * @return \we_Exec
	 */
	public function table($table){
		$this->_checkTable($table);
		$this->fTablename = $this->fCon->escapeKey($table);
		return $this;
	}

	/**
	 * Gera o SQL do comando.
	 *
	 * @return string
	 */
	public function AsSQL(){

		if (!$this->_op){
			throw new Exception(_t("we_exec_necessario_definir_operacao"));
		}

		$sql = $this->_op . " ";

		if (!$this->fTablename){
			throw new Exception(_t('we_no_table_specified'));
		}

		switch ($this->_op){
			case "INSERT":

				$this->_vals = array_merge($this->_vals, array(
					$this->fCon->escapeKey("ins_idusuario") => $this->fCon->escape( idusuario() ),
					$this->fCon->escapeKey("ins_dthr")      => $this->fCon->escape( now() )
				));

				$sql .= "INTO {$this->fTablename} (";
				foreach (array_keys($this->_vals) as $v1) {
					$sql .= $v1 . ",";
				}
				$sql = trim($sql, ",");
				$sql .= ") VALUES (";
				foreach ($this->_vals as $v2) {
					$sql .= $v2 . ",";
				}
				$sql = trim($sql, ",");
				$sql .= ")";
				break;

			case "UPDATE":
				$this->checkSealedRow();
				$this->_vals = array_merge($this->_vals, array(
					$this->fCon->escapeKey("alt_idusuario") => $this->fCon->escape( idusuario() ),
					$this->fCon->escapeKey("alt_dthr")      => $this->fCon->escape( now() )
				));

				$sql .= "{$this->fTablename} SET ";
				$sets = array();
				foreach ($this->_vals as $field => $val) {
					$sets[] = "{$field} = {$val}";
				}
				$sql .= implode(", ", $sets) . " " . $this->_where();
				break;

			case "DELETE":
				$this->checkSealedRow();
				$sql .= "FROM {$this->fTablename} ";
				$sql .= $this->_where();
				break;
		}

		return $sql;
	}

	/**
	 * Verifica se registro está lacrado
	 * @throws Exception
	 */
	private function checkSealedRow() {
		if(!$this->_checkSealed) {
			return;
		}
		$ddl = new DbDdlBuilder();
		list($pkName) = $ddl->getPK($this->fTablename);
		$sql = "SELECT {$pkName}, lacrado FROM {$this->fTablename} {$this->_where()}";
		list($pkValue, $lacrado) = $this->fCon->getSql($sql);
		if(is_true($lacrado)) {
			throw new Exception(_t('we_row_sealed_%s_%s', $pkValue, $this->fTablename));
		}
	}

	/**
	 * Executa o comando gerado.
	 *
	 * @param Boolean=false $log Se grava log ou nao
	 * @return type
	 */
	public function exec($log = false){
//		if ($log) {
//			$text = isset($this->_where) ? $this->_where() : json_encode($this->_vals);
//			logSistema($text, $this->_op, $this->fTablename);
//		}
		return $this->fCon->execSql($this->AsSQL());
	}
}

class we_ReadMongo extends we_DBCommon{
	/**
	 *
	 * @var Db
	 */
	public $fCon;
	/**
	 *
	 * @var MongoDB
	 */
	private $fDB;

	public $num_results = 0;
	private $fControls;
	private $fFields = '';
	private $fGroup = '';
	private $fJoin = '';
	private $fOrder = '';
	private $fPageOffset;
	private $fPageCount;
	private $fCount;

	public function __construct($aCon = NULL, $aDic = NULL) {
		if ($aCon) {
			$this->fCon = $aCon;
		} else {
			$this->fCon = Db::newInstance();
		}
		$this->fDB = $this->fCon->db_instance;
	}

	/**
	 * Seleção dos campos
	 * @param string|string[] $aFields
	 * @return \we_Read
	 */
	public function select($aFields = NULL) {
		$this->fFields = array();
		if ($aFields==='*') {
			return $this;
		}

		if (is_string($aFields)) {
			$aFields = preg_split("/\s*,\s*/", $aFields, -1, PREG_SPLIT_NO_EMPTY);
		}
		if (is_array($aFields)) {
			$this->fFields = array_fill_keys($aFields, 1);
		}
		return $this;
	}
	/**
	 * Utiliza função count do SGDB
	 * @return \we_Read
	 */
	public function selectCount() {
		return $this;
	}
	/**
	 * Utiliza função SUM do SGDB
	 * @return \we_Read
	 */
	public function selectSum($fieldname) {
		return $this;
	}

	public function join($aTablename, $on, $dir = "") {
		return $this;
	}

	public function joinClear(){
		return $this;
	}

	public function leftJoin($aTablename, $on) {
		return $this;
	}
	/**
	 * Define qual tabela será utilizada na construção do SQL
	 * @param string $aTablename
	 * @return \we_Read
	 */
	public function from($aTablename) {
		$this->fTablename = $aTablename;
		return $this;
	}

	public function filterAND($aFilter) {
		return $this->filter($aFilter, "AND");
	}

	public function filterOR($aFilter) {
		return $this->filter($aFilter, "OR");
	}


	/**
	 * Ex.: array("nome_campo_desc"=>1,"nome_campo_asc"=>-1)
	 * @param string[] $aOrder
	 * @return \we_Read
	 */
	public function order($aOrder) {
		if (!is_array($aOrder)) {
			$aOrder = (array) @json_decode($aOrder);
		}
		if (empty($aOrder)) {
			return $this;
		}
		$this->fOrder = array_merge($this->fOrder, $aOrder);
		return $this;
	}
	/**
	 * Adicionar campo ao order
	 * @param string $aFieldname
	 * @return \we_Read
	 */
	public function orderByASC($aFieldname, $null = null) {
		if (!$aFieldname || !is_string($aFieldname)) {
			return $this;
		}
		$this->fOrder[$aFieldname] = MongoCollection::ASCENDING;
		return $this;
	}

	/**
	 * Adicionar campo ao order
	 * @param string $aFieldname
	 * @return \we_Read
	 */
	public function orderByDESC($aFieldname, $null = null) {
		if (!$aFieldname || !is_string($aFieldname)) {
			return $this;
		}
		$this->fOrder[$aFieldname] = MongoCollection::DESCENDING;
		return $this;
	}

	public function page($aPage, $aPageSize = 50) {
		if (is_numeric($aPage) && is_numeric($aPageSize) && $aPageSize > 1) {
			$this->fPageCount = (int) $aPageSize;
			$this->fPageOffset = $this->fPageCount * ((int) $aPage - 1);
		}
		return $this;
	}

	public function limit($aValue) {
		if (is_numeric($aValue)) {
			$this->fPageCount = (int) $aValue;
		}
		return $this;
	}

	public function skip($aValue) {
		if (is_numeric($aValue)) {
			$this->fPageOffset = (int) $aValue;
		}
		return $this;
	}
	public function whereAND($param) {
//		parent::whereAND($param);
		$this->fWheres[] = array($param, 'AND');
		return $this;
	}
	public function whereOR($param) {
//		parent::whereOR($param);
		$this->fWheres[] = array($param, 'OR');
		return $this;
	}
	protected function _where() {
//		parent::_where();
		$query = array();
		foreach ($this->fWheres as $w) {

		}
	}

	public function exec($log = false) {
		if (!$this->fFields)
			throw new Exception(_t('Fields required'));
		if (!$this->fTablename)
			throw new Exception(_t('Table required'));

		$Collection = $this->fDB->selectCollection($this->fTablename);



		$Collection->find($query,
				$this->fFields);


		$lSql .= "FROM {$this->fTablename} ";
		$lSql .= $this->fJoin;
		$lSql .= "{$this->_where()}";
		$lSql .= "{$this->_group()}";
		$lSql .= "{$this->_order()}";

		return $lSql;

		$arrayfTablename = explode(" ", $this->fTablename);

		$it = $this->fCon->we_getArrSqlAssoc3($this->AsSQL(), $this->fControls);

		$this->fCount = $it->count();

		//Filtra as permissões processadas na etapa anterior. Elimina registros não permitidos
		$it = new we_FilterIterator($it);

		if (isset($this->fPageOffset)) {
			$lPageCount = ( isset($this->fPageCount) && $this->fPageCount > 1) ? $this->fPageCount : -1;
			$it = new we_LimitIterator($it, $this->fPageOffset, $lPageCount);
		} else if (isset($this->fPageCount) && $this->fPageCount >= 1) {
			$it = new we_LimitIterator($it, 0, $this->fPageCount);
		}

		if ($log) {
			logSistema($this->_where(), 'READ', $this->fTablename);
		}

		//$this->num_results = $it->num_results = count(iterator_to_array($it));

		return $it;
	}

	public function count() {
		return $this->fCount;
	}

	/**
	 * Recomendado nao utilizar para retornos com muitos registros.
	 *
	 * @param boolean $log Se vai gravar log ou nao
	 * @return []
	 */
	public function execAsArray($log = false) {
		return iterator_to_array($this->exec($log));
	}

	/**
	 * Retorna a primeira coluna sem usar iterator, sem verificar permissao e sem registrar log.
	 * @return mixed
	 */
	public function fetchColumn() {
		return $this->fCon->fetchColumn($this->AsSQL());
	}
	/**
	 * Retorna um array associativo sem usar iterator, sem verificar permissao e sem registrar log.
	 *
	 * @return mixed[]
	 */
	public function fetch() {
		return $this->fCon->fetch($this->AsSQL());
	}

	private function _group() {
		if (!$this->fGroup) {
			return "";
		}
		return " GROUP BY " . $this->fGroup . " ";
	}

	public function groupBy($campos) {
		$this->fGroup = implode(", ", $campos);
		return $this;
	}

	/**
	 * Realiza unions entre instancias do we_Read.<br/>
	 * Como retorna tudo em array, recomenda-se utilizar com poucos resultados.
	 *
	 * @param we_Read...
	 * @return []
	 */
	static public function union($args) {
		$ret = array();
		$args = func_get_args();
		foreach ($args as $arg) {
			$ret = array_merge($ret, $arg->execAsArray());
		}
		return $ret;
	}

}