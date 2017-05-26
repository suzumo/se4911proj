<?php
//$con="host=localhost port=5432 dbname=rwcaaims user=postgres password=password";
$con="dbname=rwcaAIMS";
$db=pg_connect($con) or die('connection failed');
//echo 'Connected to: ', pg_dbname($db), '<br>';
include('functions.php');
$id = "";
if (isset($_GET["id"])) {
   $id = sanitize($_GET["id"]);
}
//echo 'ID to search: ', $id, '<br>';


$query = "SELECT * FROM r_courses WHERE fullcode = '" . $id . "'";
$result = pg_exec($db, $query);
$coursenum = 0;
if ($result and pg_numrows($result) > 0)           {
   $coursenum = pg_result($result, 0, 'id');
   $coursecode = pg_result($result, 0, 'fullcode');
}







if ($result and pg_numrows($result) > 0 and $coursenum != 0) {
   echo ("<!DOCTYPE html>
   <html>
   <head>
       <title>Course Information: " . $coursecode . " " . pg_result($result, 0, 'course_name') . "</title>
       <script src=\"tabcontent.js\" type=\"text/javascript\"></script>
       <link href=\"tabcontent.css\" rel=\"stylesheet\" type=\"text/css\" />
       <link rel=\"stylesheet\" type=\"text/css\" href=\"mystyle.css\">

   </head>

   ");
   include('frame.php');
   echo "<h1>Course " . pg_result($result, 0, 'fullcode') , ":<br><span style=\"color:#21702F\">", pg_result($result, 0, 'course_name'), '</span></h1>';

    /*
    echo "<hr>";

    echo "The query executed successfully.<br>";
    echo $query, '<br>';
    echo "Number of rows in result: " . pg_numrows($result) . "<br>";
    echo "Number of fields in result: " . pg_numfields($result) . "<br>";
    */
    // used to be padding: 120px 0 40px
        echo "    <div style=\"float: left;width: 700px; margin: 0 auto; padding: 0px 0 40px;\">
        <ul class=\"tabs\">
            <li><a href=\"#main\">General</a></li>
            <li><a href=\"#outcomes\">Aims and Outcomes</a></li>
            <li><a href=\"#assessments\">Assessments</a></li>
            <li><a href=\"#timetable\">Timetable</a></li>
        </ul>
        <div class=\"tabcontents\">";

    $timetable = "select * from timetable FULL JOIN options_class_types ON component = options_class_types.id WHERE (subj || cat) = '" . $coursecode . "'
                  ORDER BY component,
                        CASE days
                             WHEN 'Mon' THEN 1
                             WHEN 'Tue' THEN 2
                             WHEN 'Wed' THEN 3
                             WHEN 'Thu' THEN 4
                             WHEN 'Fri' THEN 5
                             WHEN 'Sat' THEN 6
                             WHEN 'Sun' THEN 7
                             ELSE 8
                         END
    ;";
    $timeres = pg_exec($db, $timetable);


        echo "<div id=\"main\">";
        echo "\n<noscript><h3>General</h3></noscript>\n";
        echo '<table>';
        echo '<col><col>';
        //echo "<hr>";
        echo '<tr>';
        echo "<td><strong>Available:</strong></td><td>";
        if (pg_numrows($timeres) > 0) {
           echo "Yes.";
        } else {
           echo "No.";
        }
        echo '</td></tr><tr>';
        echo "<td><strong>UOC:</strong></td><td>", pg_result($result, 0, 'uoc');
        echo '</td></tr><tr>';
        echo "<td><strong>Prerequisites:</strong></td><td>";
        if (pg_result($result,0,'pre_req')) {
           $prereq = pg_exec($db, "SELECT fullcode from r_courses WHERE fullcode = " . pg_result($result,0,'pre_req'));
           if (pg_numrows($prereq) > 0) {
               echo redirectAndLink(pg_result($prereq, $row, 'fullcode'));
           }
        }
        echo ' <span style="color:#21702F">' . redirectAndLink(pg_result($result, 0, 'pre_req_conditions')) . '</span></td>';
        echo '</tr><tr>';
        $equivnum = pg_exec($db, "SELECT equiv_group FROM co_equiv_group_members WHERE course = $coursenum");
        echo "<td><strong>Equivalent courses:</strong></td><td>";
        if ($equivnum) {
            for ($row = 0; $row < pg_numrows($equivnum); ++$row) {
                $num = pg_result($equivnum, $row, 'equiv_num');
                $equivcourses = pg_exec($db, "SELECT fullcode FROM co_equiv_group_members INNER JOIN r_courses ON course = id WHERE equiv_group = $equivnum AND course != $id");
                for ($i = 0; $i < pg_numrows($equivcourses); ++$i) {
                    echo(' ' . pg_result($equivcourses, $i, 'fullcode'));
                }
            }
        }
        echo '</td></tr><tr>';
        echo "<td><strong>Excluded courses:</strong></td><td>";
        $exclnum = pg_exec($db, "SELECT excl_group FROM co_excl_group_members WHERE course = $coursenum");
        if ($exclnum) {
            for ($row = 0; $row < pg_numrows($exclnum); ++$row) {
                $num = pg_result($exclnum, $row, 'excl_num');
                $exclcourses = pg_exec($db, "SELECT fullcode FROM co_excl_group_members INNER JOIN r_courses ON course = id WHERE excl_group = $exclnum AND course != $id");
                for ($i = 0; $i < pg_numrows($exclcourses); ++$i) {
                    echo(' ' . pg_result($exclcourses, $i, 'fullcode'));
                }
            }
        }
        echo '</td></tr><tr>';
        $delivery = "select * from co_delivery_modes JOIN options_delivery_modes ON delivery_mode = options_delivery_modes.id WHERE co_delivery_modes.course = " . $coursenum;
        $devres = pg_exec($db, $delivery);
        if ($devres) {
            //echo "<hr>";
           /*
           echo "The query executed successfully.<br>";
           echo $delivery, '<br>';
           echo "Number of rows in result: " . pg_numrows($devres) . "<br>";
           echo "Number of fields in result: " . pg_numfields($devres) . "<br>";
           */
           echo "<td><strong>Delivery modes:</strong></td><td>";
            for ($row = 0; $row < pg_numrows($devres); $row++) {
               echo pg_result($devres, $row, 'descr');
               if ($row + 1 < pg_numrows($devres)) {echo ',';};
               echo ' ';
            }
            echo '</td></tr><tr>';
        }
        echo "<td><strong>Website:</strong></td><td><span style=\"color:#21702F\">", pg_result($result, 0, 'websites'), '</span></td>';
        echo '</tr><tr>';
        $hours= "select * from co_standard_offering_hours JOIN options_learning_activities ON learning_activity = options_learning_activities.id  WHERE course = " . $coursenum ;
        $hoursres = pg_exec($db, $hours);
        if ($hoursres) {
            /*
            echo "The query executed successfully.<br>";
            echo $hours, '<br>';
            echo "Number of rows in result: " . pg_numrows($hoursres) . "<br>";
            echo "Number of fields in result: " . pg_numfields($hoursres) . "<br>";
            */
            echo "<td><strong>Hours per week:</strong></td>";
            for ($row = 0; $row < pg_numrows($hoursres); $row++) {
                if ($row > 0) {
                   echo '<tr><td></td>';
                }
                echo '<td>';
                echo pg_result($hoursres, $row, 'hours_per_week'), ' ';
                echo pg_result($hoursres, $row, 'descr'), ' ';
                echo '</td>';
                echo '</tr>';
                }
        }
        echo '</table>';
        $desc = redirectAndLink(pg_result($result, 0, 'description'));
        echo "<div style=\"color:#21702F\">", $desc, '</div><br>';
        //echo "Teaching rationale: ", pg_result($result, 0, 'teaching_rationale'), '<br>';



    echo "</div>";

    $outcomes = "select * from shared_learning_outcomes WHERE instance = " . $coursenum . " ORDER BY sequence";
    $outres = pg_exec($db, $outcomes);
    if ($outres) {
        echo "<div id=\"outcomes\">";
       echo "\n<noscript><h3>Aims and Outcomes</h3></noscript>\n";
        //echo "<hr>";
        /*
        echo "The query executed successfully.<br>";
        echo $outcomes, '<br>';
        echo "Number of rows in result: " . pg_numrows($outres) . "<br>";
        echo "Number of fields in result: " . pg_numfields($outres) . "<br>";
        */
        echo "<strong>Aims:</strong> <div style=\"color:#21702F\">", pg_result($result, 0, 'course_aims'), '</div><br>';
        echo "<strong>Outcomes:</strong> <br> <ul>";
        for ($row = 0; $row < pg_numrows($outres); $row++) {
            echo '<li><span style="color:#21702F">' . strip_tags(pg_result($outres, $row, 'descr')), '</span></li>';
            }
        echo "</ul></div>";
    }

    $assess = "select * from co_assessment_items JOIN options_assessment_types ON co_assessment_items.assessment_type = options_assessment_types.id JOIN semester_categories ON sem_cat = semester_categories.id where course = " . $coursenum;
    $assessres = pg_exec($db, $assess);
    if ($assessres) {
        echo "<div id=\"assessments\">";
       echo "\n<noscript><h3>Assessments</h3></noscript>\n";
        //echo "<hr>";
        /*
        echo "The query executed successfully.<br>";
        echo $assess, '<br>';
        echo "Number of rows in result: " . pg_numrows($assessres) . "<br>";
        echo "Number of fields in result: " . pg_numfields($assessres) . "<br>";
        */
        echo '<ul>';
        for ($row = 0; $row < pg_numrows($assessres); $row++) {
            echo '<li style="margin-bottom:1em">';
            echo ("<strong>Assessment " . ($row + 1) . "</strong><br>");
            echo '<table>';
            echo '<tr>';
            echo '<td>Name:</td> <td style="color:#21702F">' . pg_result($assessres, $row, 'name'), '</td>';
            echo '</tr> <tr>';
            echo '<td>Percentage:</td> <td>' . pg_result($assessres, $row, 'percentage'), '</td>';
            echo '</tr> <tr>';
            echo '<td>Description:</td> <td style="color:#21702F">' . pg_result($assessres, $row, 'description'), '</td>';
            echo '</tr> <tr>';
            echo '<td>Week Due:</td> <td>' . pg_result($assessres, $row, 'week_due'), '</td>';
            echo '</tr> <tr>';
            echo '<td>Max Mark:</td> <td>' . pg_result($assessres, $row, 'max_mark'), '</td>';
            echo '</tr> <tr>';
            echo '<td>Type:</td> <td>' . pg_result($assessres, $row, 'descr'), '</td>';
            echo '</tr>';
            //echo pg_result($assessres, $row, 'category'), '<br>';
            echo '</table>';
            echo '</li>';
        }
        echo '</ul>';
        echo "</div>";
    } else {
        echo "The query failed with the following error:<br>";
        echo pg_errormessage($db);
    }


    if ($timeres) {
        echo "<div id=\"timetable\">";
       echo "\n<noscript><h3>Timetable</h3></noscript>\n";
        //echo "<hr>";
        /*
        echo "The query executed successfully.<br>";
        echo $timetable, '<br>';
        echo "Number of rows in result: " . pg_numrows($timeres) . "<br>";
        echo "Number of fields in result: " . pg_numfields($timeres) . "<br>";
        */
        echo "<table>";
        echo "<col style=\"width:80px\"><col style=\"width:60px\"><col style=\"width:80px\"><col style=\"width:80px\">";
        echo '<tr>';
        echo "<th>Type<th>Day<th>Start<th>End";
        echo "</tr>";
        for ($row = 0; $row < pg_numrows($timeres); $row++) {
            echo "<tr>";
            echo '<td>' . pg_result($timeres, $row, 'descr') . '</td>';
            echo '<td>' . pg_result($timeres, $row, 'days') . '</td>';
            echo '<td>' . pg_result($timeres, $row, 'time_start'). '</td>';
            echo '<td>' . pg_result($timeres, $row, 'time_end'). '</td>';
            echo "</tr>\n";

        };
        echo "</table>";
        echo "</div>\n";
    }
    echo "</div>";
    echo "</div>";
    echo "</body>";
    echo "</html>";


}  else {
    echo ("<!DOCTYPE html>
   <html>
   <head>
       <title>Course Information</title>

   </head>

             ");
   include('frame.php');

    echo ("<div style=\"width: 700px; margin: 0 auto; padding: 120px 0 40px;\">");
    if (pg_numrows($result) < 1 or coursenum == 0) {
       echo("No course $id found. Please <a href='courses.php'>select a course.</a>");
    } else {
       echo "The query failed with the following error:<br>";
       echo pg_errormessage($db);
    }

    echo (" </div> </body> </html>");
}
pg_close($db);


?>