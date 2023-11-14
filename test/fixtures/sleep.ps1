set seconds=%1
ping 127.0.0.1 -n %seconds% > nul
echo "Finish"
