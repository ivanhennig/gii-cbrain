<?php
header("Access-Control-Allow-Origin: *");
header("Access-Control-Allow-Headers: Content-Type");
header("Content-Type: text/html; charset=utf-8",true);
header("Cache-Control: no-cache, must-revalidate");
header("Expires: Mon, 10 Aug 1979 05:00:00 GMT");

if (! ini_get("date.timezone") ) {
	date_default_timezone_set("America/Sao_Paulo");
}

$request_id = NULL;

error_reporting(E_ALL & ~E_DEPRECATED & ~E_STRICT & ~E_NOTICE);
set_exception_handler(function($exception) use ($request_id) {

	$response = array (
		'id'     => $request_id,
		'result' => NULL,
		'error'  => array("code"=>$exception->getCode(), "message"=>$exception->getMessage()  )
	);
	echo json_encode($response,JSON_UNESCAPED_UNICODE);
});
set_error_handler(function($code, $string, $file, $line) use ($request_id) {
	throw new Exception("$string\n".trim(basename($file))."($line)", -1);
}, E_ALL & ~E_DEPRECATED & ~E_STRICT & ~E_NOTICE);


/* Se o client suporta gzip, podemos compactar a saida */
$supportsGzip = strpos($_SERVER['HTTP_ACCEPT_ENCODING'], 'gzip') !== false;
if ($supportsGzip) {
	header("Content-Encoding: gzip");
	ob_start("ob_gzhandler");
}
/* -------------- */
set_time_limit(0);

if("OPTIONS" == $_SERVER['REQUEST_METHOD']) {//Quando vem de outras origens...
	if ($supportsGzip) ob_flush();
	exit(0);
}
$class = filter_input(INPUT_GET, 'class');
if (!$class) {
	throw new Exception("Class was not defined");
}
spl_autoload_register(function($class_name) {
	if (file_exists("class.$class_name.php")) {
		require_once "class.$class_name.php";
	}
});
if ((new ReflectionClass($class))->isAbstract()) {
	$instance = $class;
} else {
	$instance = new $class();
}
$contents   = file_get_contents('php://input');
$request    = json_decode($contents, true);
// echo $contents;
// exit;
$request_id = $request['id'];
try {
	$result     = call_user_func_array(array($instance,$request['method']),$request['params']);
} catch (Exception $e) {
	throw $e;
}
$response   = array (
	'id'     => $request_id,
	'result' => $result,
	'error'  => NULL
);
// print_r($response);
echo json_encode($response,JSON_UNESCAPED_UNICODE);

if ($supportsGzip) ob_flush();