# Raspberry Pi GPIO Web Interface

# Introduction
A web based Raspberry Pi interface to control the GPIO Pin(s). Possible uses can be determing is soldering worked; or control of gpio pins for other applications. Developed using [Nitrogen Web Framework](https://github.com/nitrogen/nitrogen) and [Erlang Ale](https://github.com/esl/erlang_ale)

# Getting running

This program requires has a dependency on erlang which can be pulled using.
   sudo apt-get -y install erlang 

Then pull the repo and make
    git clone https://github.com/stuart-thackray/pi_gpio_web.git
    make
    make run

Then open up the browser to control the Pi.
   http://<IP-ADDRESS>:8000/

# More Info
The configuration is saved between restarts of the PI; it is automatically saved every 5 minutes; or manually when saved via the GUI button. If this is not desired remove the cfg file using the below
    sudo rm gpio.config

# TODO
- [ ] Determine if naming the pins would be worthwhile; i.e. doorbell, red led, e.t.c.
- [ ] Make more todos

# Screensh/Demo

Wire up the pi and have fun
![Demo setup of Board]
(https://github.com/stuart-thackray/pi_gpio_web/blob/master/doc/smaller_example_setup.jpg)

Control via the web
![Screenshot]
(https://github.com/stuart-thackray/pi_gpio_web/blob/master/doc/example_web_page.jpg)
