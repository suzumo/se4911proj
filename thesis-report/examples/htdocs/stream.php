<?php
//$con="host=localhost port=5432 dbname=rwcaaims user=postgres password=password";
$con="dbname=rwcaAIMS";
$db=pg_connect($con) or die('connection failed');
//echo 'Connected to: ', pg_dbname($db), '<br>';
$code = "";
if (isset($_GET["id"])) {
   $code = $_GET["id"];
}
if (isset($_GET["c"])) {
   $category = $_GET["c"];
} else {
   $category = array();
}
//echo 'ID to search: ', $code, '<br>';


$query = "SELECT * FROM r_streams WHERE (fullcode = '" . $code . "') ORDER BY fullcode";
$result = pg_exec($db, $query);


if ($result and pg_numrows($result) > 0) {
   $id = pg_result($result, 0, 'id');
    /*echo "The query executed successfully.<br>";
    echo $query, '<br>';
    echo "Number of rows in result: " . pg_numrows($result) . "<br>";
    echo "Number of fields in result: " . pg_numfields($result) . "<br>";
    */

   echo ("<!DOCTYPE html>
   <html>
   <head>
       <title>Stream Information: " . pg_result($result, 0, 'fullcode') . " " . pg_result($result, 0, 'stream_name') . "</title>
       <script src=\"tabcontent.js\" type=\"text/javascript\"></script>
       <link href=\"tabcontent.css\" rel=\"stylesheet\" type=\"text/css\" />
   </head> 

   ");
   include ('functions.php');
   if (in_array('removeFrame', $category))
   {   echo "<body style=\"background:#F6F9FC; font-family:Arial;\">"; }
   else {
        include('frame.php');
   }
   if (!in_array('removeHeading', $category)) {
      echo "<h1> Stream " . pg_result($result, 0, 'fullcode') , ":<br><span style=\"color:#21702F\">", pg_result($result, 0, 'fullname'), '</span></h1>';
   }
   if (!in_array('removeTabs', $category)) {
        echo "    <div style=\"float:left;width: 700px; margin: 0 auto; padding: 00px 0px 40px;\">
        <ul class=\"tabs\">";
            if (!in_array('removeGeneral', $category)) {
               echo "<li><a href=\"#main\">General</a></li>";
            }
            if (!in_array('removeOutcomes', $category)) {
               echo "<li><a href=\"#outcomes\">Outcomes</a></li>";
            }
            if (!in_array('removeExternal', $category)) {
               echo "<li><a href=\"#external\">External</a></li>";
            }
        echo "</ul>
        <div class=\"tabcontents\">";
   } else {
       echo "<div><div>"; // to keep it balanced
   }
   if (!in_array('removeGeneral', $category)) {
     echo "<div id=\"main\">";
       if (in_array('addHeadings', $category)) {
          echo "\n<h3>General</h3>\n";
       } else {
          echo "\n<noscript><h3>General</h3></noscript>\n";
       }
       if (!in_array('removeUOC', $category)) {
          echo "<strong>UOC:</strong> " . pg_result($result, 0, 'uoc'), "<br>";
       }
       if (!in_array('removeDescription', $category)) {
          $desc = redirectAndLink(pg_result($result, 0, 'stream_description'));
          echo "<div style=\"color:#21702F\">"  . $desc . "</div><br>";
       }
     echo "</div>";
   }

   if (!in_array('removeOutcomes', $category)) {
    echo "<div id=\"outcomes\">";
       if (in_array('addHeadings', $category)) {
          echo "\n<h3>Outcomes</h3>\n";
       } else {
          echo "\n<noscript><h3>Outcomes</h3></noscript>\n";
       }
         $outquery = "SELECT * FROM shared_learning_outcomes WHERE instance = $id ORDER BY sequence";
         $outres = pg_exec($db, $outquery);
         echo "<ul>";
              for ($row = 0; $row < pg_numrows($outres); $row++) {
                 echo "<li> <span style=\"color:#21702F\">";
                 echo strip_tags(pg_result($outres, $row, 'descr'));
                 echo "</span> </li>";
              }

         echo "</ul>";
         echo "</div>";
    }

   if (!in_array('removeExternal', $category)) {      
        echo "<div id=\"external\">\n";
       if (in_array('addHeadings', $category)) {
          echo "\n<h3>External Service</h3>\n";
       } else {
          echo "\n<noscript><h3>External Service</h3></noscript>\n";
       }
       echo '<form action="stream.php" method="get">';
             echo 'Stream: <input type="text" name="id" value="', $code, '"><br>';
             echo stringCategoryCheckboxAlias($category, 'removeFrame', 'Remove the outer frame<br>');
             echo stringCategoryCheckboxAlias($category, 'removeHeading', 'Remove the stream heading<br>');
             echo stringCategoryCheckboxAlias($category, 'removeTabs', 'Remove the tabs and tab-box<br>');
             echo stringCategoryCheckboxAlias($category, 'addHeadings', 'Add relevant headings (useful when removing tabs)<br>');
             echo stringCategoryCheckboxAlias($category, 'removeGeneral', 'Remove the entire General tab and its contents<br>');
             echo stringCategoryCheckboxAlias($category, 'removeUOC', 'Remove the UOC information under the General tab<br>');
             echo stringCategoryCheckboxAlias($category, 'removeDescription', 'Remove the Description under the General tab<br>');
             echo stringCategoryCheckboxAlias($category, 'removeOutcomes', 'Remove the entire Outcomes tab and its contents<br>');
             echo stringCategoryCheckboxAlias($category, 'removeExternal', 'Remove the entire External tab and its contents<br>');

       echo '<input type="submit" value="Link"> </form>';
       echo "</div>\n";
    }

    echo "</div>";
    echo "</div>";
    echo "</body>";

}  else {

    echo ("<!DOCTYPE html>
   <html>
   <head>
       <title>Stream Information</title>
   </head> 
      <body style=\"background:#F6F9FC; font-family:Arial;\">
              ");
   include('frame.php');

    echo ("<div style=\"width: 700px; margin: 0 auto; padding: 120px 0 40px;\">");

    if (pg_numrows($result) < 1) {
       echo("No stream $id found. Please <a href='streams.php'>select a stream.</a>");
    } else {
       echo "The query failed with the following error:<br>";
   
       echo pg_errormessage($db);

    }
}
pg_close($db);


?>