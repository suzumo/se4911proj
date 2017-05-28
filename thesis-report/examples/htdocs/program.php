<?php
//$con="host=localhost port=5432 dbname=rwcaaims user=postgres password=password";
$con="dbname=rwcaAIMS";
$db=pg_connect($con) or die('connection failed');
//echo 'Connected to: ', pg_dbname($db), '<br>';
$code = "";
if (isset($_GET["id"])) {
   $code = $_GET["id"];
}
//echo 'ID to search: ', $code, '<br>';


$query = "SELECT * FROM r_programs WHERE fullcode = '$code'";
$result = pg_exec($db, $query);

$id = 0;
if ($result and pg_numrows($result) > 0) {
   $id = pg_result($result, 0, 'id');
   echo ("<!DOCTYPE html>
   <html>
   <head>
       <title>Program Information: " . $code . " " . pg_result($result, 0, 'program_name') . "</title>
       <script src=\"tabcontent.js\" type=\"text/javascript\"></script>
       <link href=\"tabcontent.css\" rel=\"stylesheet\" type=\"text/css\" />
   </head>
   ");                      
   include('functions.php');
   include('frame.php');
   echo "<h1>Program " . pg_result($result, 0, 'fullcode') , ":<br><span style=\"color:#21702F\">", pg_result($result, 0, 'program_name'), '</span></h1>';

        echo "    <div style=\"float: left;width: 700px; margin: 0 auto; padding: 0px 0 40px;\">
        <ul class=\"tabs\">
            <li><a href=\"#main\">General</a></li>
            <li><a href=\"#admission\">Admission Information</a></li>            
            <li><a href=\"#outcomes\">Outcomes</a></li>
            <li><a href=\"#scholarships\">Scholarships</a></li>
        </ul>
        <div class=\"tabcontents\">";

    /*
    echo "The query executed successfully.<br>";
    echo $query, '<br>';
    echo "Number of rows in result: " . pg_numrows($result) . "<br>";
    echo "Number of fields in result: " . pg_numfields($result) . "<br>";
    */

    echo "<div id=\"main\">";
       echo "\n<noscript><h3>General</h3></noscript>\n";
    echo '<table>';
    echo "<col style=\"width:160px\"><col>";
    //echo "Name: <span style=\"color:#21702F\">" . pg_result($result, 0, 'program_name'), "</span><br>";
    //echo "Code: " . pg_result($result, 0, 'code'), "<br>";
    echo "<tr><td><strong>Min UOC:</strong></td><td>" . pg_result($result, 0, 'minimum_uoc') . "</td>";
    echo '</tr>';
    $attendance = "SELECT * FROM pr_attendance_types INNER JOIN options_attendance_types ON attendance_type = options_attendance_types.id WHERE pr_attendance_types.program = " . $id;
    $attend  = pg_exec($db, $attendance);
    if ($attend) {    
       echo '<tr>';
       echo "<td><strong>Attendance types:</strong></td>";
       for ($row = 0; $row < pg_numrows($attend); $row++) {
           if ($row > 0) {
              echo '<tr><td></td>';
           }
           echo '<td>';
           if (pg_result($attend, $row, 'student_type') == 'DOM') {
              echo ("<em>Domestic Students:</em> ");
           } else { // == INT
              echo ("<em>International Students:</em> ");
           }
           echo pg_result($attend, $row, 'descr') . '</td></tr>';
       }
    }
    echo "<tr><td><strong>Campus:</strong></td><td>" . pg_result($result, 0, 'campusdescr') . "</td></tr></table>";
    $desc = redirectAndLink(pg_result($result, 0, 'program_description'));
    echo "<div style=\"color:#21702F\">"  .  $desc , "</div><br>";
    echo "</div>";


    echo "<div id=\"admission\">";
       echo "\n<noscript><h3>Admissions Information</h3></noscript>\n";
    //echo "Admissions Information: <br>";
    echo "<table><col style=\"width:160px\"><col><tr>";
    echo "<td><strong>UAC Code:</strong></td><td>" . pg_result($result, 0, 'uac_code') . "</td></tr><tr>";
    echo "<td><strong>Career:</strong></td><td>" .  pg_result($result, 0, 'career') . "</td></tr>";
    $admission = "SELECT * FROM pr_ugrd_admissions FULL JOIN pr_pgrd_admissions ON pr_ugrd_admissions.program = pr_pgrd_admissions.program WHERE pr_ugrd_admissions.program = " . $id . " OR pr_pgrd_admissions.program = " . $id;
    $admis  = pg_exec($db, $admission);
    //echo $admission, '<br>';
    if (pg_result($result, 0, 'career') == "UG") {
       echo "<tr><td><strong>Last year's ATAR:</strong></td><td>" . pg_result($admis, 0, 'prev_year_atar') . "</td></tr><tr>";
       echo "<td><strong>Min ATAR:</strong></td><td>" . pg_result($admis, 0, 'min_atar') . "</td></tr><tr>";
       echo "<td><strong>Max ATAR:</strong></td><td>" . pg_result($admis, 0, 'max_atar') . "</td></tr><tr>";
       echo "<td><strong>Additional requirements:</strong></td><td><span style=\"color:#21702F\">" . pg_result($admis, 0, 'add_requirements_for_selection') . "</span></td></tr><tr>";
       if (pg_result($admis, 0, 'has_assumed_knowl')) {
          echo "<td><strong>Assumed knowledge:</strong></td><td><span style=\"color:#21702F\">". pg_result($admis, 0, 'assumed_knowl_hsc_subjs') . "</span></td></tr><tr>";
       }
       if (pg_result($admis, 0, 'has_recommended_studies')) {
          echo "<td><strong>Recommended studies:</strong></td><td><span style=\"color:#21702F\">". pg_result($admis, 0, 'recommended_studies_hsc_subjs') . "</span></td></tr><tr>";
       }
       if (pg_result($admis, 0, 'is_avail_for_online_int_transfer')) {
          echo "<td><strong>Internal Transfer Entry Requirements:</strong></td><td><span style=\"color:#21702F\">". pg_result($admis, 0, 'int_transfer_entry_requirements') . "</span></td></tr>";
       }
    } elseif (pg_result($result, 0, 'career') == "PG") {

    }
    echo "</table></div>";


    echo "<div id=\"outcomes\">";
       echo "\n<noscript><h3>Outcomes</h3></noscript>\n";
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
                  
    echo "<div id=\"scholarships\">";
       echo "\n<noscript><h3>Scholarships</h3></noscript>\n";
    //echo "Scholarships: <br>";               

        $scholarships = "SELECT * FROM pr_scholarships INNER JOIN scholarships on pr_scholarships.scholarship = scholarships.id WHERE pr_scholarships.program = ".$id;
        $schol = pg_exec($db, $scholarships);
        //echo $scholarships . "<br>";
        //echo "Number of rows in result: " . pg_numrows($schol) . "<br>";
        //echo "Number of fields in result: " . pg_numfields($schol) . "<br>";
    if ($schol) {
        for ($row = 0; $row < pg_numrows($schol); $row++) {
            echo "<tr>";

            echo "<td>";
            echo $nsscode;
            echo "</td>";

            echo "<td>";
            echo "<div style=\"color:#21702F\">" . pg_result($result, $row, 'descr') . "</div>";
            echo "</td>";

            echo "</tr>";
        }
    }                   
    if (pg_result($result, 0, 'offers_coop_scholarship')) {
        echo "Offers Co-op Scholarship.<br>";
    } else {
       if (pg_numrows($schol) < 1) {
          echo "No scholarships offered.<br>";
       }
    }
    echo "</div>";

    echo "</div>";
    echo "</div>";
    echo "</body>";
    echo "</html>";


}  else {
    echo ("<!DOCTYPE html>
   <html>   <head>
       <title>Program Information</title>
   </head>
   <body style=\"background:#F6F9FC; font-family:Arial;\">
              ");
   include('frame.php');

    echo ("<div style=\"width: 700px; margin: 0 auto; padding: 120px 0 40px;\">");

    if (pg_numrows($result) < 1) {
       echo("No program $code found. Please <a href='programs.php'>select a program.</a>");
    } else {
       echo "The query failed with the following error:<br>";
       echo pg_errormessage($db);
    }
    echo (" </div> </body> </html>");

}
pg_close($db);


?>