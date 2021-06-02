$(document).ready( function() {
    function displayTime(){
        var currentTime = new Date();
        var hours = currentTime.getHours();
        var minutes = currentTime.getMinutes();
        var seconds = currentTime.getSeconds();
        var meridian = "AM";
        var clockDiv = document.getElementById("clock");
        if(hours < 10){
            hours = "0" + hours;
        }
        if(minutes < 10){
            minutes = "0" + minutes
        }
        if(seconds < 10){
            seconds = "0" + seconds;
        }
        if(hours > 12){
            hours = hours - 12;
            meridian = "PM";
        }
        if(hours === 0){
            hours = 12;
        }
        clockDiv.innerText = hours + ":" + minutes + ":" + seconds + " " + meridian;
    }

    setInterval(displayTime, 1000);
    function displayTab(){
        var currentTime = new Date();
        var hours = currentTime.getHours();
        var minutes = currentTime.getMinutes();
        var seconds = currentTime.getSeconds();
        var meridian = "AM";
        var clockDiv = document.getElementById("tab");
        if(hours < 10){
            hours = "0" + hours;
        }
        if(minutes < 10){
            minutes = "0" + minutes
        }
        if(seconds < 10){
            seconds = "0" + seconds;
        }
        if(hours >+ 12){
            hours = hours - 12;
            meridian = "PM";
        }
        if(hours === 0){
            hours = 12;
        }
        clockDiv.innerText = hours + ":" + minutes + ":" + seconds + " " + meridian;
    }

    setInterval(displayTab, 1000);
});