<?php

/*
 * 	pt_metaphone() "Portuguese Metaphone"
 * 	version 1.0
 *
 * 	Essa função pega uma palavra em português do Brasil e a retorna
 * 	em uma chave metafônica.
 *
 * 	Copyright (C) 2008		Prefeitura Municipal de Várzea Paulista
 * 							<metaphone@varzeapaulista.sp.gov.br>
 *
 * 	Histórico:
 * 	2008-05-20		Versão 1.0
 * 					Initial Release
 *
 * 	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 *
 * 	RECONHECIMENTO :
 *
 * 	Essa função foi adaptada de uma função  chamada spanish_metaphone
 * 	do Israel J. Sustaita. O código fonte original pode ser obtido em
 * 	http://www.geocities.com/isloera/spanish_methaphone.txt (baseada
 * 	na versão original do DoubleMetaphone em inglês de Geoff Caplan).
 *
 * 	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 *
 * 	AGRADECIMENTOS :
 * 	Sílvia e Thaiza pela ajuda com a língua portuguesa
 *
 * 	EQUIPE DE DESENVOLVIMENTO :
 * 		Rodrigo Domingos Pinto Lotierzo
 * 		Giovanni dos Reis Nunes
 *
 * 		Estagiários:
 * 		  Caio Varlei Righi Schleich
 * 		  Diego Jorge de Souza
 * 		  Sueli Silvestre da Silva
 *
 * 	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 *
 * 	Funcionamento:
 *
 * 	1.	Esta função recebe a string contendo o nome, palavra ou frase
 * 		a ser criada a chave e retorna essa chave.
 *
 * 	2.	Receber a 'string', a primeira coisa a ser feita é substituir
 * 		os dígrafos e encontros consonantais pelas letras que corres-
 * 		pondem aos seus sons.
 *
 * 	3.	Os dígrafos "LH", "NH" e o encontro consonantal "RR" são con-
 * 		vertidos em números para facilitar a interpretação.
 *
 * 	4.	Os dígrafos "CH" e "PH" (questões históricas), o encontro con-
 * 		sonantal "SC" e o "Ç" são convertidos em seus fonemas corres-
 * 		pondentes.
 *
 * 	5.	Os acentos são removidos das vogais.
 *
 * 	6.	Letras cujos fonemas não se alteram não são mexidas ("B", "V",
 * 		"P",etc...).
 *
 * 	7.	Outras letras como "G" e "X" são tratadas de acordo com casos
 * 		específicos.
 */

function metaphone_pt($STRING, $LENGTH = 50) {
	$string_at = function ($STRING, $START, $STRING_LENGTH, $LIST) {
		if (($START < 0) || ($START >= strlen($STRING))) {
			return 0;
		}
		for ($I = 0; $I < count($LIST); $I++) {
			if ($LIST[$I] == substr($STRING, $START, $STRING_LENGTH)) {
				return 1;
			}
		}
		return 0;
	};
	$is_vowel = function ($string, $pos) {
		return preg_match("/A|E|I|O|U/", substr($string, $pos, 1));
	};
	/*
	 *    inicializa a chave metafônica
	 */
	$META_KEY = "";

	/*
	 *    configura o tamanho máximo da chave metafônica
	 */
	$KEY_LENGTH = (int) $LENGTH;

	/*
	 *    coloca a posição no começo
	 */
	$CURRENT_POS = (int) 0;

	/*
	 *    recupera o tamanho máximo da string
	 */
	$STRING_LENGTH = (int) strlen($STRING);

	/*
	 *    configura o final da string
	 */
	$END_OF_STRING_POS = $STRING_LENGTH - 1;
	$ORIGINAL_STRING = $STRING . "    ";

	/*
	 *    vamos repor alguns caracteres portugueses facilmente
	 *     confundidos, substituíndo os números não confundir com
	 *    os encontros consonantais (RR), dígrafos (LH, NH) e o
	 *    C-cedilha:
	 *
	 *    'LH' to '1'
	 *    'RR' to '2'
	 *    'NH' to '3'
	 *    'Ç'  to 'SS'
	 *    'CH' to 'X'
	 */
	$ORIGINAL_STRING = preg_replace('/\d/', ' ', $ORIGINAL_STRING);
	$ORIGINAL_STRING = preg_replace('/ã|á|â/', 'A', $ORIGINAL_STRING);
	$ORIGINAL_STRING = preg_replace('/é|ê/', 'E', $ORIGINAL_STRING);
	$ORIGINAL_STRING = preg_replace('/í|y/', 'I', $ORIGINAL_STRING);
	$ORIGINAL_STRING = preg_replace('/õ|ó|ô/', 'O', $ORIGINAL_STRING);
	$ORIGINAL_STRING = preg_replace('/ú|ü/', 'U', $ORIGINAL_STRING);
//	var_dump($ORIGINAL_STRING);
//	var_dump($ORIGINAL_STRING);
	$ORIGINAL_STRING = preg_replace('/ç/', 'SS', $ORIGINAL_STRING);
	$ORIGINAL_STRING = preg_replace('/Ç/', 'SS', $ORIGINAL_STRING);
//	var_dump($ORIGINAL_STRING);
	/*
	 *    Converte a string para caixa alta
	 */
	$ORIGINAL_STRING = strtoupper($ORIGINAL_STRING);

	/*
	 *    faz substituições
	 *    -> "olho", "ninho", "carro", "exceção", "cabaça"
	 */
	$ORIGINAL_STRING = preg_replace('/LH/', '1', $ORIGINAL_STRING);
	$ORIGINAL_STRING = preg_replace('/NH/', '3', $ORIGINAL_STRING);
	$ORIGINAL_STRING = preg_replace('/RR/', '2', $ORIGINAL_STRING);
	$ORIGINAL_STRING = preg_replace('/XC/', 'SS', $ORIGINAL_STRING);
//    print $ORIGINAL_STRING;
	/*
	 *    a correção do SCH e do TH por conta dos nomes próprios:
	 *    -> "schiffer", "theodora", "ophelia", etc..
	 * */
	$ORIGINAL_STRING = preg_replace('/SCH/', 'X', $ORIGINAL_STRING);
	$ORIGINAL_STRING = preg_replace('/TH/', 'T', $ORIGINAL_STRING);
	$ORIGINAL_STRING = preg_replace('/PH/', 'F', $ORIGINAL_STRING);

	/*
	 *    remove espaços extras
	 */
	$ORIGINAL_STRING = trim($ORIGINAL_STRING);

	/*
	 *    loop principal
	 */
	while (strlen($META_KEY) < $KEY_LENGTH) {
		/*
		 *    sai do loop se maior que o tamanho da string
		 */
		if ($CURRENT_POS >= $STRING_LENGTH) {
			break;
		}

		/*
		 *    pega um caracter da string
		 */
		$CURRENT_CHAR = substr($ORIGINAL_STRING, $CURRENT_POS, 1);

		/*
		 *    se é uma vogal e faz parte do começo da string,
		 *    coloque-a como parte da metachave
		 */
		if (($is_vowel($ORIGINAL_STRING, $CURRENT_POS)) &&
				( ($CURRENT_POS == 0) ||
				($string_at($ORIGINAL_STRING, $CURRENT_POS - 1, 1, " "))
				)
		) {
			$META_KEY .= $CURRENT_CHAR;
			$CURRENT_POS += 1;
		}
		/*
		 *    procurar por consoantes que tem um único som, ou que
		 *    que já foram substituídas ou soam parecido, como
		 *     'Ç' para 'SS' e 'NH' para '1'
		 */ elseif ($string_at($ORIGINAL_STRING, $CURRENT_POS, 1, array('1', '2', '3', 'B', 'D', 'F', 'J', 'K', 'L', 'M', 'P', 'T', 'V'))) {
			$META_KEY .= $CURRENT_CHAR;

			/*
			 *    incrementar por 2 se uma letra repetida for encontrada
			 */
			if (substr($ORIGINAL_STRING, $CURRENT_POS + 1, 1) == $CURRENT_CHAR) {
				$CURRENT_POS += 2;
			}

			/*
			 *    senão incrementa em 1
			 */
			$CURRENT_POS += 1;
		} else {
			/*
			 *    checar consoantes com som confuso e similar
			 */
			switch ($CURRENT_CHAR) {

				case 'G':
					switch (substr($ORIGINAL_STRING, ($CURRENT_POS + 1), 1)) {
						case 'E':
						case 'I':
							$META_KEY .= 'J';
							$CURRENT_POS += 2;
							break;

						case 'U':
							$META_KEY .= 'G';
							$CURRENT_POS += 2;

							break;

						case 'R':
							$META_KEY .='GR';
							$CURRENT_POS += 2;
							break;

						default:
							$META_KEY .= 'G';
							$CURRENT_POS += 2;
							break;
					}
					break;

				case 'U':
					if ($is_vowel($ORIGINAL_STRING, $CURRENT_POS - 1)) {
						$CURRENT_POS+=1;
						$META_KEY .= 'L';
						break;
					}
					/*
					 *    senão...
					 */
					$CURRENT_POS += 1;
					break;

				case 'R':
					if (($CURRENT_POS == 0) || (substr($ORIGINAL_STRING, ($CURRENT_POS - 1), 1) == ' ')) {
						$CURRENT_POS+=1;
						$META_KEY .= '2';
						break;
					} elseif (($CURRENT_POS == $END_OF_STRING_POS) || (substr($ORIGINAL_STRING, ($CURRENT_POS + 1), 1) == ' ')) {
						$CURRENT_POS+=1;
						$META_KEY .= '2';
						break;
					} elseif ($is_vowel($ORIGINAL_STRING, $CURRENT_POS - 1) && $is_vowel($ORIGINAL_STRING, $CURRENT_POS + 1)) {
						$CURRENT_POS+=1;
						$META_KEY .= 'R';
						break;
					}
					/*
					 *    senão...
					 */
					$CURRENT_POS += 1;
					$META_KEY .= 'R';
					break;

				case 'Z':
					if ($CURRENT_POS >= (strlen($ORIGINAL_STRING) - 1)) {
						$CURRENT_POS+=1;
						$META_KEY .= 'S';
						break;
					} elseif (substr($ORIGINAL_STRING, ($CURRENT_POS + 1), 1) == 'Z') {
						$META_KEY .= 'Z';
						$CURRENT_POS += 2;
						break;
					}
					/*
					 *    senão...
					 */
					$CURRENT_POS += 1;
					$META_KEY .= 'Z';
					break;


				case 'N':
					if (($CURRENT_POS >= (strlen($ORIGINAL_STRING) - 1))) {
						$META_KEY .= 'M';
						$CURRENT_POS += 1;
						break;
					} elseif (substr($ORIGINAL_STRING, ($CURRENT_POS + 1), 1) == 'N') {
						$META_KEY .= 'N';
						$CURRENT_POS += 2;
						break;
					}
					/*
					 *    senão...
					 */
					$META_KEY .= 'N';
					$CURRENT_POS += 1;
					break;

				case 'S':
					/*
					 *    caso especial 'assado', 'posse', 'sapato', 'sorteio'
					 */
					if ((substr($ORIGINAL_STRING, ($CURRENT_POS + 1), 1) == 'S') ||
							($CURRENT_POS == $END_OF_STRING_POS) ||
							(substr($ORIGINAL_STRING, ($CURRENT_POS + 1), 1) == ' ')
					) {
						$META_KEY .= 'S';
						$CURRENT_POS += 2;
					} elseif (($CURRENT_POS == 0) || (substr($ORIGINAL_STRING, ($CURRENT_POS - 1), 1) == ' ')) {
						$META_KEY .= 'S';
						$CURRENT_POS += 1;
					} elseif (($is_vowel($ORIGINAL_STRING, $CURRENT_POS - 1)) &&
							($is_vowel($ORIGINAL_STRING, $CURRENT_POS + 1))) {
						$META_KEY .= 'Z';
						$CURRENT_POS += 1;
					}
					/*
					 *  Ex.: Ascender, Lascivia
					 */ elseif (
							(substr($ORIGINAL_STRING, ($CURRENT_POS + 1), 1) == 'C') &&
							(
							(substr($ORIGINAL_STRING, ($CURRENT_POS + 2), 1) == 'E') ||
							(substr($ORIGINAL_STRING, ($CURRENT_POS + 2), 1) == 'I')
							)
					) {
						$META_KEY .= 'S';
						$CURRENT_POS += 3;
					}
					/*
					 * Ex.: Asco, Auscutar, Mascavo
					 */ elseif (
							(substr($ORIGINAL_STRING, ($CURRENT_POS + 1), 1) == 'C') &&
							(
							(substr($ORIGINAL_STRING, ($CURRENT_POS + 2), 1) == 'A') ||
							(substr($ORIGINAL_STRING, ($CURRENT_POS + 2), 1) == 'O') ||
							(substr($ORIGINAL_STRING, ($CURRENT_POS + 2), 1) == 'U')
							)
					) {
						$META_KEY .= 'SC';
						$CURRENT_POS += 3;
					} else {
						$META_KEY .= 'S';
						$CURRENT_POS += 1;
					}
					break;

				case 'X':
					/*
					 *    caso especial 'táxi', 'axioma', 'axila', 'tóxico'
					 */
					if ((substr($ORIGINAL_STRING, ($CURRENT_POS - 1), 1) == 'E') && ($CURRENT_POS == 1)) {
						$META_KEY .= 'Z';
						$CURRENT_POS += 1;
					} elseif ((substr($ORIGINAL_STRING, ($CURRENT_POS - 1), 1) == 'I') && ($CURRENT_POS == 1)) {
						$META_KEY .= 'X';
						$CURRENT_POS += 1;
					} elseif (($is_vowel($ORIGINAL_STRING, $CURRENT_POS - 1)) && ($CURRENT_POS == 1)) {
						$META_KEY .= 'KS';
						$CURRENT_POS += 1;
					} else {
						$META_KEY .= 'X';
						$CURRENT_POS += 1;
					}
					break;

				case 'C':
					/*
					 *    caso especial 'cinema', 'cereja'
					 */
					if ($string_at($ORIGINAL_STRING, $CURRENT_POS, 2, array('CE', 'CI'))) {
						$META_KEY .= 'S';
						$CURRENT_POS += 2;
					} elseif ((substr($ORIGINAL_STRING, ($CURRENT_POS + 1), 1) == 'H')) {
						$META_KEY .= 'X';
						$CURRENT_POS += 2;
					} else {
						$META_KEY .= 'K';
						$CURRENT_POS += 1;
					}
					break;

				/*
				 *    como a letra 'h' é silenciosa no português, vamos colocar
				 *    a chave meta como a vogal logo após a letra 'h'
				 */
				case 'H':
					if ($is_vowel($ORIGINAL_STRING, $CURRENT_POS + 1)) {
						$META_KEY .= $ORIGINAL_STRING[$CURRENT_POS + 1];
						$CURRENT_POS += 2;
					} else {
						$CURRENT_POS += 1;
					}
					break;

				case 'Q':
					if (substr($ORIGINAL_STRING, $CURRENT_POS + 1, 1) == 'U') {
						$CURRENT_POS += 2;
					} else {
						$CURRENT_POS += 1;
					}

					$META_KEY .= 'K';
					break;

				case 'W':
					if ($is_vowel($ORIGINAL_STRING, $CURRENT_POS + 1)) {
						$META_KEY .= 'V';
						$CURRENT_POS += 2;
					} else {
						$META_KEY .= 'U';
						$CURRENT_POS += 2;
					}
					break;

				default:
					$CURRENT_POS += 1;
			}
		}
	}

	/*
	 *    corta os caracteres em branco
	 */
	$META_KEY = trim($META_KEY);

	/*
	 *    retorna a chave matafônica
	 */
	return $META_KEY;
}

