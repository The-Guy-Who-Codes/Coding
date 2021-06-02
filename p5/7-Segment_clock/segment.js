

function bar(x, y, colour, z) {
    var w = 25;
    var h = 100;
    if(z === "hori"){
        w = 100;
        h = 25;
    }
    fill(colour);
    rect(x, y, w, h);
    noFill();
}

function number(number, areax, areay){

    if (number === 0){
        number = 10
    }
    //top
    bar(100 + areax, 100 + areay, (numbers_chart[number])[0], "hori");
    //top left
    bar(75 + areax, 125 + areay, (numbers_chart[number])[1]);
    //top right
    bar(200 + areax, 125 + areay, (numbers_chart[number])[2]);
    //middle
    bar(100 + areax, 225 + areay, (numbers_chart[number])[3], "hori");
    //bottom left
    bar(75 + areax, 250 + areay, (numbers_chart[number])[4]);
    //bottom right
    bar(200 + areax, 250 + areay, (numbers_chart[number])[5]);
    //bottom
    bar(100 + areax, 350 + areay, (numbers_chart[number])[6], "hori");
}