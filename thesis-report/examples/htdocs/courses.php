<!DOCTYPE html>
   <html>
   <head>
       <title>Handbook Course List</title>
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

$query = "SELECT * FROM r_courses where (fullcode ilike '%" .  $id . "%') ORDER BY fullcode";
$result = pg_exec($db, $query);

include('frame.php');
if ($result) {

    //echo "The query executed successfully.<br>";
    //echo $query, '<br>';
    //echo "Number of rows in result: " . pg_numrows($result) . "<br>";
    //echo "Number of fields in result: " . pg_numfields($result) . "<br>";
    // display:block rules=groups style=\"background:#FCFFFF;border:1px solid black;\"
    echo pg_numrows($result), ' known courses.<br>';
    echo '<div style="width:800px;float:left">';
    echo '<table id="courseTable" class="tablesorter-custom">';
    echo '<col style="width:3em"><col style="width:6em"><col>';
    echo '<thead display:block>';
    echo "<tr style=\"background:#FCFFFF;border:1px solid black;\">";
        echo "<th> Avail";
        echo "<th> Code";
        echo "<th style=\"text-align:left\"> Name";
    echo "</tr>";
    echo '</thead>';
    echo "\n", '<noscript></table><div style="height:400px;overflow:auto">
               <table class="tablesorter-custom">
                      <col style="width:3em"><col style="width:6em"><col>
               </noscript>', "\n";
    echo '<tbody>';
/*    echo '</table>';
    echo '<div style="border:1px solid black;background:#FCFFFF;height:440px;overflow:auto">';
    echo '<table>';*/
    //echo '<col style="width:50px"><col style="width:50px"><col>';
        $avail = pg_exec($db, "select distinct (subj || cat) as code from timetable order by code");
        // change to store in array indexed by subj || cat and compare to fullcode, reducing query count
    $ttable = array();
    for ($row = 0; $row < pg_numrows($avail); $row++) {
        $ttable[pg_result($avail, $row, 'code')] = true;
    }
    for ($row = 0; $row < pg_numrows($result); $row++) {            
        $catcode = pg_result($result, $row, 'fullcode');

        echo "<tr>";

        echo "<td style=\"text-align:center\">";
        if (array_key_exists($catcode, $ttable)) {
           echo 'Yes';
        }

        echo "</td>";
        echo "<td>";
        echo "<a href = /course.php?id=",$catcode,">";
        echo $catcode;
        echo "</a>";
        echo "</td>";

        echo "<td>";
        echo pg_result($result, $row, 'course_name');
        echo "</td>";

        echo "</tr>\n";

    }
    echo "</tbody>";
    echo "</table>";
    echo '<noscript></div></noscript>';
    echo '</div>';
    echo "</body></html>";

} else {      

    echo "The query failed with the following error:<br>";

    echo pg_errormessage($db);

}      
pg_close($db);


?>
