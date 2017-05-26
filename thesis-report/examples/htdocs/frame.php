<?php

   echo "<body style=\"background:#F6F9FC; font-family:Arial;\">";
     echo '<div id="header" style="height:120px;margin:20px 0px 00px">
      <span style="white-space:nowrap;font-size:3em"><a href="index.php" style="color:#000;text-decoration:none"><img src="unswlogo2.gif" alt="UNSW"> AIMS Handbook 2013</a></span>
      </div>';
     echo '<div id="sidebar" style="width:180px;float:left;margin:00px 40px 0px 40px">';


     echo '<br>';
     echo "<nav>";
        echo "<a href='programs.php'><img src=\"program2.gif\" alt=\"Programs\"></a><br>";
        echo "<a href='streams.php'><img src=\"stream2.gif\" alt=\"Streams\"></a><br>";
        echo "<a href='courses.php'><img src=\"course2.gif\" alt=\"Courses\"></a><br>";
        echo '<form action="search.php" method="get">';
        echo '<input type="text" style="width:120px;margin:10px 0px 0px" name="query"';
        echo '>';
        echo '<input type="submit" value="Search">
              </form>';
        echo "<a href='search.php'>Advanced Search.</a><br>";

    echo '</nav>';
    echo '</div>';
?>