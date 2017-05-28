<!DOCTYPE html>
   <html>
   <head>
       <title>Handbook Program List</title>
       <script src="tabcontent.js" type="text/javascript"></script>
       <link href="tabcontent.css" rel="stylesheet" type="text/css" />

      <script src="jquery.min.js"></script>
    	<link href="tablesorter.theme.custom.css" rel="stylesheet">
    	<script src="tablesorter/js/jquery.tablesorter.js"></script>
    	<script src="tablesorter/js/jquery.tablesorter.widgets.js"></script>
      <script src="tablesorter/js/widgets/widget-scroller.js"></script>
    	<script>
    	$(function(){
    		$('table').tablesorter({
    			widgets        : ['zebra', 'columns', 'scroller'],
    			usNumberFormat : false,
    			sortReset      : true,
    			sortRestart    : true,
      		widgetOptions : {
      			scroller_height : 400,
      			scroller_barWidth : 17,
      			scroller_jumpToHeader: true,
      			scroller_idPrefix : 's_'
      		}
    		});
    	});
    	</script>
   </head>
<?php
$con="dbname=rwcaAIMS";
$db=pg_connect($con) or die('connection failed');
//echo 'Connected to: ', pg_dbname($db), '<br>';
$id = "";
if (isset($_GET["id"])) {
   $id = $_GET["id"];
}
//echo 'ID to search: ', $id, '<br>';


$query = "SELECT * FROM r_programs where (fullcode ilike '%" .  $id . "%' or program_name ilike '%" .  $id . "%') ORDER BY fullcode";
$result = pg_exec($db, $query);



include('frame.php');
if ($result) {

    //echo "The query executed successfully.<br>";
    //echo $query, '<br>';
    //echo "Number of rows in result: " . pg_numrows($result) . "<br>";
    //echo "Number of fields in result: " . pg_numfields($result) . "<br>";

    echo pg_numrows($result), ' known programs.<br>';
    echo '<div style="width:800px;float:left">';
    echo "<table class=\"tablesorter-custom\">";
    echo '<col style="width:3em"><col>';
    echo '<thead>';
    echo '<tr><th style="text-align:left">Code<th style="text-align:left">Name</tr>';
    echo '</thead>';
    echo "\n", '<noscript></table><div style="height:400px;overflow:auto">
               <table class="tablesorter-custom">
                      <col style="width:3em"><col>
               </noscript>', "\n";
    echo '<tbody>';
    for ($row = 0; $row < pg_numrows($result); $row++) {

        echo "<tr>";

        echo "<td>";
        $catcode = pg_result($result, $row, 'code');
        echo "<a href = /program.php?id=",$catcode,">";
        echo $catcode;
        echo "</a>";
        echo "</td>";

        echo "<td>";
        echo pg_result($result, $row, 'program_name');
        echo "</td>";

        echo "</tr>\n";

    }
    echo '</tbody>';
    echo "</table></div>";
    echo '<noscript></div></noscript>';
    echo "</body></html>";

} else {      

    echo "The query failed with the following error:<br>";

    echo pg_errormessage($db);

}
pg_close($db);


?>
