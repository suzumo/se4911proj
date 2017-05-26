<?php
$con="dbname=rwcaAIMS";
$db=pg_connect($con) or die('connection failed');

$filepath = "./schedule.sched";
$filecontents = file_get_contents($filepath);

$table = "
  CREATE TABLE timetable (
         id             integer PRIMARY KEY,
         term           integer,
         session        varchar(3),
         subj           varchar(4),
         cat            varchar(4),
         descrip        text,
         admin          text,
         career         varchar(4),
         acad_org       text,
         component      varchar(3),
         class_nbr      integer,
         class_section  varchar(4),
         class_type     varchar(3),
         assoc_class    integer,
         gr_basis       varchar(3),
         class_stat     varchar(1),
         consent        varchar(3),
         instr_mode     varchar(2),
         campus         varchar(4),
         location       text,
         autoenrlsec1   varchar(8),
         autoenrlsec2   varchar(8),
         start_date     date,
         end_date       date,
         enrl_tot       integer,
         enrl_cap       integer,
         room_cap       integer,
         comb_sec       text,
         mtg_rows       integer,
         rsv_rows       integer,
         cln_rows       integer,
         days           text,
         time_start     time,
         time_end       time,
         weeks          text,
         oddeven        varchar(5),
         clash          varchar(10),
         facility_id    text,
         facility_descr text,
         instructors    text
  ) ;
";

$r_records = "DROP VIEW IF EXISTS r_records CASCADE;CREATE VIEW r_records AS
           SELECT id, code as fullcode, name as recordname
           FROM record_instances
           WHERE enddate IS NULL
           AND (proposal_status = 'AP')
           AND is_skeleton_record IS FALSE;
";

$r_programs = "DROP VIEW IF EXISTS r_programs;CREATE VIEW r_programs AS
            SELECT p.program_name as fullname, p.program_description as description, p.*, r.fullcode, r.recordname, c.descr AS campusdescr
            FROM program_records AS p
            INNER JOIN r_records AS r USING (id)
            LEFT JOIN campuses AS c ON c.id = p.campus;
";

$r_courses = "DROP VIEW IF EXISTS r_courses;CREATE VIEW r_courses AS
            SELECT c.course_name as fullname, *
            FROM course_records AS c
            INNER JOIN r_records AS r
            USING (id);
";


$r_streams = "DROP VIEW IF EXISTS r_streams;CREATE VIEW r_streams AS
            SELECT s.stream_name as fullname, s.stream_description as description, *
            FROM stream_records AS s
            INNER JOIN r_records AS r
            USING (id);
";


pg_exec($db, $r_records);
pg_exec($db, $r_programs);
pg_exec($db, $r_courses);
pg_exec($db, $r_streams);         
echo("Views created.<br>");

pg_exec($db, "DROP TABLE IF EXISTS timetable;");
echo($table . "<br>");
pg_exec($db, $table);

if ($filecontents) {
  echo("File opened<br>");
  $filearray = explode("\n", $filecontents);
  $filearray = str_replace("'", "", $filearray);
  $filearray = str_replace("\t", "','", $filearray); // WEEKS is given with a comma in, so don't use that as a basic delim (or introduce quotation marks)
  $entries = array();
  echo(count($filearray) . " entries in array.<br>");
  // for each row:
  // if it's headed with CLS, save to $class
  // if it's headed with CLN or RSV, do nothing
  // if it's headed with MTG, add to $schedule array an entry of $class + line
  $class = "";
  $schedule = "";
  $count = 0;
  foreach ($filearray as $line) {
     $header = substr($line, 0, 3);
     if ($header == "CLS") {
        $class = substr($line, 4); // use 4 here to trim the first tab
     }
     if ($header == "MTG") {
        $entries[] = ("'" . $count . "'" .  $class . substr($line, 3) . "'");
        $count = $count + 1;
     }
  }
  $entries = preg_replace("/\s*'\s*/", "'", $entries);
  //$entries = str_replace('","', '", "', $entries);
  unset($entries[0]);
  foreach($entries as $line) {
     //echo $line . "<br>";
     pg_exec($db, "INSERT INTO timetable VALUES ( " . $line . " );");
     //echo("INSERT INTO timetable VALUES ( " . $line . " );" . "<br>");
  }
  echo("Schedule imported.<br>");
} else {
   echo("Opening file failed<br>");
}


?>