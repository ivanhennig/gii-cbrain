<?php
header("Content-Type: text/html; charset=utf-8",true);

include_once './class.Main.php';
require_once './class.CSVIterator.php';
require_once './metaphone_pt.php';
require_once './metaphone_es.php';
echo "<pre>";

$RBC = new Main();
$r = $RBC->exec(
		file_get_contents("case.json"),
		file_get_contents("casedata.json") );
//
echo file_get_contents("casedata.json");
print_r($r);



//$t1 = "MELHRAR SISTEMA DE MONITORES PARA QUE NÃO OCORRAM FALHAS"
//$t2 =
