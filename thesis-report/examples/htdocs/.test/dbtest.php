<html>
<head><title>PostgreSQL Server Test</title></head>
<body bgcolor=white>
<h3>PostgreSQL Server Test</h3>
<?
$db = pg_connect("dbname=template1");
if (!$db)
	echo "<h3><font color=red>Server not responding.</font></h3>\n";
else {
	echo "<b>PostgreSQL Meta-data Tables</b><br>\n<pre>\n";
	$q = pg_query("select * from pg_tables");
	$n = pg_num_rows($q);
	for ($i = 0; $i < $n; $i++)
	{
		$t = pg_fetch_array($q);
		printf("%-20.20s %-20.20s\n",$t["tablename"],$t["tableowner"]);
	}
}
?>
</pre>
</body>
</html>
