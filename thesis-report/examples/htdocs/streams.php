<!DOCTYPE html>
   <html>
   <head>
       <title>Handbook Stream List</title>
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
$id = "";
if (isset($_GET["id"])) {
   $id = $_GET["id"];
}


$query = "SELECT * FROM r_streams ORDER BY fullcode";
$result = pg_exec($db, $query);

include('frame.php');
if ($result) {
    echo pg_numrows($result), ' known streams.<br>';
    echo '<div style="width:800px;float:left">';
    echo "<table class=\"tablesorter-custom\">";
    echo '<col style="width:5.5em"><col>';
    echo '<thead display:block>';
        echo "<th> Code";
        echo "<th style=\"text-align:left\"> Name";
    echo '</thead>';
    echo "\n", '<noscript></table><div style="height:400px;overflow:auto">
               <table class="tablesorter-custom">
                      <col style="width:5.5em"><col>
               </noscript>', "\n";
    echo '<tbody>';
    for ($row = 0; $row < pg_numrows($result); $row++) {

        echo "<tr>";

        echo "<td>";
        $catcode = pg_result($result, $row, 'subject_area') . pg_result($result, $row, 'strand') . pg_result($result, $row, 'stream_type');
        // this is equivalent to 'code', just using longer version so I understand how to construct it
        echo "<a href = /stream.php?id=",$catcode,">";
        echo $catcode;
        echo "</a>";
        echo "</td>";

        echo "<td>";
        echo pg_result($result, $row, 'stream_name');
        echo "</td>";

        echo "</tr>\n";

    }
    echo '</tbody>';
    echo "</table> </div>";
    echo '<noscript></div></noscript>';
    echo "</body></html>";

} else {      

    echo "The query failed with the following error:<br>";

    echo pg_errormessage($db);

}
pg_close($db);


?>
