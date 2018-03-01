<?php

header("Access-Control-Allow-Origin: *");
header("Access-Control-Allow-Headers: Content-Type");
header("Content-Type: text/html; charset=utf-8",true);
header("Cache-Control: no-cache, must-revalidate");
header("Expires: Mon, 10 Aug 1979 05:00:00 GMT");
ini_set('soap.wsdl_cache_enabled', '1');
ini_set('soap.wsdl_cache_ttl', '60');
set_time_limit(0);
error_reporting(E_ALL & ~E_DEPRECATED & ~E_STRICT & ~E_NOTICE);
require_once './class.Main.php';
function wsTryRequireOnce($file) {
	if (file_exists($file)) {
		require_once __DIR__ . DIRECTORY_SEPARATOR . $file;
		return true;
	}
	return false;
}
spl_autoload_register(function($class_name) {
	wsTryRequireOnce("soap".DIRECTORY_SEPARATOR."{$class_name}.class.php");
},TRUE,TRUE);

$WSHelper = new WSHelper("http://hennig.net.br", 'Main');
$WSHelper->use = SOAP_ENCODED;
$WSHelper->useWSDLCache = false;
$WSHelper->setPersistence(SOAP_PERSISTENCE_REQUEST);

if (!defined("WSOcultarClasses")) {
	define("WSOcultarClasses",1);
}
if (!defined("WSIgnorarVerificacaoClasses")) {
	define("WSIgnorarVerificacaoClasses",1);
}
$WSClasses       = array ();
$WSStructures    = array ();
$WSClassRequires = array ();

try {
	$WSHelper->handle();
} catch(Exception $e) {
	$WSHelper->fault($e->getCode(), $e->getCode().": ".$e->getMessage(),"", $e->__toString());
}