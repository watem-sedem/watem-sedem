if ($Env:DRONE_NETRC_MACHINE) {
@"
machine $Env:DRONE_NETRC_MACHINE
login $Env:DRONE_NETRC_USERNAME
password $Env:DRONE_NETRC_PASSWORD
"@ |Out-File -FilePath (Join-Path $Env:USERPROFILE '_netrc') -Encoding ascii;
}
