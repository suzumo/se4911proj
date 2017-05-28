<?php

     function redirectAndLink($desc) {
        $desc = preg_replace('/\_mce\_href\=\"(.*?)\"/', '', $desc);
        $desc = preg_replace('/http:\/\/www.handbook.unsw.edu.au\/\w+\/programs\/\w+\/(\d+)\.html/', 'program.php?id=$1', $desc);
        $desc = preg_replace('/http:\/\/www.handbook.unsw.edu.au\/\w+\/courses\/\w+\/(\w+)\.html/', 'course.php?id=$1', $desc);
        $desc = preg_replace('/https:\/\/cms4.comms.unsw.edu.au\/\w+\/courses\/\w+\/(\w+)\.html/', 'course.php?id=$1', $desc);
        $desc = preg_replace('/http:\/\/www.handbook.unsw.edu.au\/\w+\/plans\/\w+\/(\w+)\.html/', 'stream.php?id=$1', $desc);
        $desc = preg_replace("/\.\.\/\.\.\/\w+\/courses\/\d+\/(\w+)\.html/", 'course.php?id=$1', $desc);
        $desc = preg_replace( '/(?<!\=)\b([A-Z]{4}\d{4})\b/', '<a href="course.php?id=$0">$0</a>', $desc);
        //$desc = preg_replace( '/\b(\d{4})\b/', '<a href=/program.php?id=$0>$0</a>', $desc);
        $desc = preg_replace( '/\"\/(\w+)\.php\?id\=\<a href\=\/\w+\.php\?id\=\w+\>(\w+)\<\/a\>\"/', '$1.php?id=$2', $desc);
        return $desc;
     }


     function sanitize($query) {


          $query = trim($query);
          $query = stripslashes($query);
          $query = htmlspecialchars($query);
          $query = pg_escape_string($query);
          $query = preg_replace('/\*/', '%', $query);
          return $query;
     }

     function isChecked($cat, $box) {
        if (in_array($box, $cat)) {
           return 'checked = "checked"';
        }
        return "";
     }
     
     function stringCategoryCheckbox($category, $name) {
        return '<input type="checkbox" name="c[]" value="' . $name . '" ' . isChecked($category, $name) . " /> $name";
     }

     function stringCategoryCheckboxAlias($category, $name, $alias) {
        return '<input type="checkbox" name="c[]" value="' . $name . '" ' . isChecked($category, $name) . " /> $alias";
     }
?>