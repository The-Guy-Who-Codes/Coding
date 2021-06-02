var a = 0;
var b = 0;
var c = 0;
var d = 0;
function time(){
    d++;
    if (d === 10){
        d = 0
        c++;
    }
    if (c === 6){
        c = 0
        b++;
    }
    if (b === 10){
        b = 0;
        a++;
    }


}