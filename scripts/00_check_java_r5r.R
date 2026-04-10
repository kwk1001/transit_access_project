cat('R version: ', R.version.string, '
', sep = '')
cat('System: ', Sys.info()[['sysname']], ' ', Sys.info()[['release']], '
', sep = '')

if (Sys.info()[['sysname']] == 'Darwin') {
  out <- tryCatch(system('/usr/libexec/java_home -V', intern = TRUE, ignore.stderr = FALSE), error = function(e) character())
  cat('java_home output:
')
  if (length(out) > 0) cat(paste(out, collapse = '
'), '
') else cat('No JDK detected by /usr/libexec/java_home
')
}

cat('JAVA_HOME env: ', Sys.getenv('JAVA_HOME', unset = '<unset>'), '
', sep = '')
cat('rJava installed: ', requireNamespace('rJava', quietly = TRUE), '
', sep = '')
cat('r5r installed: ', requireNamespace('r5r', quietly = TRUE), '
', sep = '')
