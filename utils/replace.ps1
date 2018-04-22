param([string] $from, [string] $to)

$input | %{$_ -replace $from, $to}
