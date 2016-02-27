# Raspberry Pi GPIO Web Interface

# Introduction
A web based Raspberry Pi interface to control the GPIO Pin(s). Possible uses can be determing is soldering worked; or control of gpio pins for other applications. Developed using [Nitrogen Web Framework](https://github.com/nitrogen/nitrogen) and [Erlang Ale](https://github.com/esl/erlang_ale)

# Getting running

This program has a dependency on erlang which can be installed like below; or downloaded From [Erlang Solutions](https://www.erlang-solutions.com/resources/download.html)

    sudo apt-get -y install erlang 

Then clone the repo and make. The default `make` uses the inet as the webserver you may wish to use a different weberver in such you could use one of the following `make cowboy` | `make inets`| `make mochiweb` | `make webmachine` | `make yaws`

    git clone https://github.com/stuart-thackray/pi_gpio_web.git
    make
    make run

Then open up the browser to control the Pi. (to get your IP address on the Rasberry pi type `ifconfg| grep inet`)

    http://<IP-ADDRESS>:8000/

# More Info
The configuration is saved between restarts of the PI; it is automatically saved every 5 minutes; or manually when saved via the GUI button. If this is not desired remove the cfg file using the below

    sudo rm gpio.config

# TODO
- [x] Custom naming of the GPIO ; i.e. doorbell, red led, e.t.c.


# Screenshot/Demo

Wire up the pi and have fun
![Demo setup of Board]
(https://github.com/stuart-thackray/pi_gpio_web/blob/master/doc/smaller_example_setup.jpg)

Control via the web
![Screenshot]
(https://github.com/stuart-thackray/pi_gpio_web/blob/master/doc/example_web_page.jpg)
