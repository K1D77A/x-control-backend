# x-control
backend for x-control app

Comes with two functions: make-client and get-interface-ips. 
Use get-interface-ips to find out what your machines IP is, choose the interface which
your android device will be able to communicate with, ie whichever is connected to your wifi.

call (make-client <ip>) to setup the listening server that your android device can connect to.

