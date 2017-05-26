
<!DOCTYPE html>
   <html>
   <head>
       <title>Handbook Search</title>

       <script src="tabcontent.js" type="text/javascript"></script>
       <link href="tabcontent.css" rel="stylesheet" type="text/css" />

      <script src="jquery.min.js"></script>
    	<link href="tablesorter.theme.custom.css" rel="stylesheet">
    	<script src="tablesorter/js/jquery.tablesorter.js"></script>
    	<script src="tablesorter/js/jquery.tablesorter.widgets.js"></script>
      <script src="tablesorter/js/widgets/widget-scroller.js"></script>
   </head>


<?php
//$con="host=localhost port=5432 dbname=rwcaaims user=postgres password=password";
include('frame.php');
include('functions.php');
$con="dbname=rwcaAIMS";
$db=pg_connect($con) or die('connection failed');
//echo 'Connected to: ', pg_dbname($db), '<br>';
$query = "";
if (isset($_GET["query"])) {
   $query = sanitize($_GET["query"]);
}
$category = "";
if (isset($_GET["c"])) {
   $category = $_GET["c"];
} else {
  $category = array();
  $category[] = "Courses";
  $category[] = "Programs";
  $category[] = "Streams";
  $category[] = "Undergrad";
  $category[] = "Postgrad";
  $category[] = "Research";
  $category[] = "Nonaward"; 
  $category[] = "Hybrid";
  $category[] = "KENS";
  $category[] = "COFA";
  $category[] = "ADFA";
  $category[] = "Code";
  $category[] = "Name";
}
$courses = array();
$programs = array();
$streams = array();


   echo '<form action="search.php" method="get">';
   echo '<input type="text" name="query"';
if (isset($_GET["query"])) {
   echo ' value="' . $query . '"';
}
   echo '>';
   echo '<input type="submit" value="Search">';
   echo '<table>';
   echo '<tr>';
      echo '<td>Search for: </td>';
      //echo stringCategoryCheckbox($category, 'Courses');
      echo '<td>', stringCategoryCheckboxAlias($category, 'Programs', '<img src="program2.gif" alt="Programs">'),  '</td>';
      echo '<td>', stringCategoryCheckboxAlias($category, 'Streams', '<img src="stream2.gif" alt="Streams">'),  '</td>';
      echo '<td>', stringCategoryCheckboxAlias($category, 'Courses', '<img src="course2.gif" alt="Courses">'),  '</td>';
      //echo stringCategoryCheckbox($category, 'Programs');
      //echo stringCategoryCheckbox($category, 'Streams');
   echo '</tr>';
   echo '<tr>';
      echo '<td></td>';;
      echo '<td>', stringCategoryCheckbox($category, 'Undergrad'),  '</td>';
      echo '<td>', stringCategoryCheckbox($category, 'Postgrad'),  '</td>';
      echo '<td>', stringCategoryCheckbox($category, 'Research'),  '</td>';
      echo '<td>', stringCategoryCheckboxAlias($category, 'Nonaward', 'Non-Award'),  '</td>';
      echo '<td>', stringCategoryCheckbox($category, 'Hybrid'),  '</td>';
   echo '</tr>';
   echo '<tr>';
      echo '<td>Offered at: </td>';
      echo '<td>', stringCategoryCheckboxAlias($category, 'KENS', 'Kensington'),  '</td>';
      echo '<td>', stringCategoryCheckboxAlias($category, 'COFA', 'Paddington'),  '</td>';
      echo '<td>', stringCategoryCheckboxAlias($category, 'ADFA', 'Canberra'),  '</td>';
   echo '</tr>';
   echo '<tr>';
      echo '<td>Search in: </td>';
      echo '<td>', stringCategoryCheckbox($category, 'Code'),  '</td>';
      echo '<td>', stringCategoryCheckbox($category, 'Name'),  '</td>';
      echo '<td>', stringCategoryCheckbox($category, 'Description'),  '</td>';
   echo '</tr>';

   echo '</table>';
      echo stringCategoryCheckboxAlias($category, 'reqAvail', 'Require course availability<br>');
      echo stringCategoryCheckboxAlias($category, 'reqCOOP', 'Require programs with CO-OP scholarships<br>');
      echo stringCategoryCheckboxAlias($category, 'reqSchol', 'Require programs with other scholarships<br>');

   echo '
        </form>';
if (isset($_GET["query"]) and strlen($query) > 0) {
//   if (!isset($_GET["c"])) {
//      // basic search results
//      echo "</tr>";
//      $avail = pg_exec($db, "select distinct (subj || cat) as code from timetable order by code");
//      $ttable = array();
//      for ($row = 0; $row < pg_numrows($avail); $row++) {
//          $ttable[pg_result($avail, $row, 'code')] = true;
//      }
//      $courses = pg_exec($db, "SELECT * FROM r_courses where ((fullcode || ' ' || course_name || ' ' || fullcode) ilike '%" .  $query . "%') ORDER BY not fullcode ilike '%$query%', fullcode");
//      $programs = pg_exec($db, "SELECT * FROM r_programs where ((fullcode || ' ' || program_name || ' ' || fullcode) ilike '%" .  $query . "%') ORDER BY fullcode");
//      $streams = pg_exec($db, "SELECT * FROM r_streams where ((fullcode || ' ' || stream_name || ' ' || fullcode) ilike '%" .  $query . "%') ORDER BY not fullcode ilike '%$query%', fullcode");
 //  } else {
      // advanced search results
      //foreach ($category as $x=>$y) {
      //   echo $x . ' : ' . $y . '</br>';
      //}
      $orstring = "false";
      $andstring = "true";
      if (!in_array('Undergrad', $category)) {
         $andstring = "$andstring and not career = 'UG'";
      }
      if (!in_array('Postgrad', $category)) {
         $andstring = "$andstring and not career = 'PG'";
      }
      if (!in_array('Research', $category)) {
         $andstring = "$andstring and not career = 'RS'";
      }
      if (!in_array('Nonaward', $category)) {
         $andstring = "$andstring and not career = 'NA'";
      }
      if (!in_array('Hybrid', $category)) {
         $andstring = "$andstring and not career = 'HY'";
      }
      if (!in_array('KENS', $category)) {
         $andstring = "$andstring and not campus = '1' and not campus ='2'";
      }
      if (!in_array('COFA', $category)) {
         $andstring = "$andstring and not campus = '3'";
      }
      if (!in_array('ADFA', $category)) {
         $andstring = "$andstring and not campus = '4'";
      }
      if (in_array('Code', $category)) {
         $orstring = "$orstring or fullcode ilike '%$query%'";
      }
      if (in_array('Name', $category)) {
         $orstring = "$orstring or fullname ilike '%$query%'";
      }
      if (in_array('Description', $category)) {
         $orstring = "$orstring or description ilike '%$query%'";
      }               
      $wherestring = "$andstring and ($orstring)";
      if (in_array('Courses', $category)) {
         $tstring = "";
         if (in_array('reqAvail', $category)) {
            $tstring = " AND EXISTS (select * from timetable where subj = subject_area and cat = catalogue_code)";
         }
         // requiring availability is done in the print section
         $courses = pg_exec($db, "SELECT * FROM r_courses where $wherestring $tstring ORDER BY not fullcode ilike '%$query%', fullcode");
      }
      if (in_array('Programs', $category)) {
         $tstring = "";
         if (in_array('reqCOOP', $category)) {
            $tstring = "$tstring AND offers_coop_scholarship = true";
         }
         if (in_array('reqSchol', $category)) {
            $tstring = "$tstring AND exists (select * from pr_scholarships where r_programs.id = pr_scholarships.program)";
         }
         $programs = pg_exec($db, "SELECT * FROM r_programs where $wherestring $tstring ORDER BY not fullcode ilike '%$query%', fullcode");
      }
      if (in_array('Streams', $category)) {
         $streams = pg_exec($db, "SELECT * FROM r_streams where $wherestring ORDER BY not fullcode ilike '%$query%', fullcode");
      }
      //echo $orstring;
//   }
     $numCourses = 0;
     if (count($courses) > 0) {
        $numCourses = pg_numrows($courses);
     }
     $numPrograms = 0;
     if (count($programs) > 0) {
        $numPrograms = pg_numrows($programs);
     }
     $numStreams = 0;
     if (count($streams) > 0) {
        $numStreams = pg_numrows($streams);
     }
    echo "<div style=\"float: left;width: 700px; margin: 1em auto; padding: 0px;\">
    <ul class=\"tabs\">";
    if (in_array('Courses', $category)) {
       echo "<li><a href=\"#courses\">Courses ($numCourses)</a></li>";
    }
    if (in_array('Programs', $category)) {
        echo "<li><a href=\"#programs\">Programs ($numPrograms)</a></li>";
    }
    if (in_array('Streams', $category)) {
        echo "<li><a href=\"#streams\">Streams ($numStreams)</a></li>";
    }
    echo "</ul>
    <div class=\"tabcontents\">\n";
    if (in_array('Courses', $category)) {
      echo "\n\n", '<div id="courses">';
       echo "\n<noscript><h3>Courses</h3></noscript>\n";
        echo '<table id="courseTable" class="tablesorter-custom" style="margin:0">';
        echo '<col style="width:3em"><col style="width:6em"><col>';
        echo "<thead><tr>";
        echo "<th> Avail";
        echo "<th> Code";
        echo "<th> Name";
        echo '</tr></thead></table>';
        echo '<div style="height:200px;overflow:auto"> <table class="tablesorter-custom" style="margin:0"><col style="width:3em"><col style="width:6em"><col><tbody>';
   
       if ($numCourses > 0) {
            $avail = pg_exec($db, "select distinct (subj || cat) as code from timetable order by code");
            $ttable = array();
            for ($row = 0; $row < pg_numrows($avail); $row++) {
                $ttable[pg_result($avail, $row, 'code')] = true;
            }
          for ($row = 0; $row < $numCourses; $row++) {
              $catcode = pg_result($courses, $row, 'fullcode');
              echo "<tr>";
   
              echo "<td style=\"text-align:center\">";
              if (array_key_exists($catcode, $ttable)) {
                 echo 'Yes';
              }
              echo "</td>";
   
              echo "<td>";
              echo "<a href = \"course.php?id=",$catcode,"\">";
              echo $catcode;
              echo "</a>";
              echo "</td>";
      
              echo "<td>";
              echo pg_result($courses, $row, 'course_name');
              echo "</td>";
      
              echo "</tr>\n";
         }
      }
       echo "</tbody></table></div></div>";
    }
    if (in_array('Programs', $category)) {

    echo "\n\n<div id=\"programs\">";
       echo "\n<noscript><h3>Programs</h3></noscript>\n";
       echo '<table id="programTable" class="tablesorter-custom" style="margin:0">';
           echo '<col style="width:6em"><col>';
       echo "<thead><tr>";
           echo "<th> Code";
           echo "<th style=\"text-align:left\"> Name";
       echo "</tr></thead></table>";
        echo '<div style="height:200px;overflow:auto"> <table class="tablesorter-custom" style="margin:0"><col style="width:6em"><col><tbody>';
       if (count($programs) > 0) {
          for ($row = 0; $row < pg_numrows($programs); $row++) {
              echo "<tr>";

              echo "<td>";
              echo "<a href = \"program.php?id=",pg_result($programs, $row, 'fullcode'),"\">";
              echo pg_result($programs, $row, 'fullcode');
              echo "</a>";
              echo "</td>";
      
              echo "<td>";
              echo pg_result($programs, $row, 'program_name');
              echo "</td>";
      
              echo "</tr>\n";
         }
       }
       echo "</tbody></table></div></div>";
    }

    if (in_array('Streams', $category)) {
    echo "\n\n<div id=\"streams\">";
       echo "\n<noscript><h3>Streams</h3></noscript>\n";
       echo '<table id="streamTable" class="tablesorter-custom" style="margin:0">';
       echo '<col style="width:6em"><col>';
       echo "<thead><tr>";
           echo "<th> Code";
           echo "<th style=\"text-align:left\"> Name";
       echo "</tr></thead></table>";
        echo '<div style="height:200px;overflow:auto"> <table class="tablesorter-custom"  style="margin:0"><col style="width:6em"><col><tbody>';
       if (count($streams) > 0) {
          for ($row = 0; $row < pg_numrows($streams); $row++) {
              echo "<tr>";

              echo "<td>";
              echo "<a href = \"stream.php?id=",pg_result($streams, $row, 'fullcode'),"\">";
              echo pg_result($streams, $row, 'fullcode');      
              echo "</a>";
              echo "</td>";

              echo "<td>";
              echo pg_result($streams, $row, 'stream_name');
              echo "</td>";

              echo "</tr>\n";
         }
      }
      echo "</tbody></table></div></div></div>";
    }

    if (!in_array('Courses', $category) and !in_array('Programs', $category) and !in_array('Streams', $category)) {
        echo "You didn't select at least one of Courses, Programs or Streams to search for.";
    }
   echo "</div>\n";
   echo "</body> </html>";
}