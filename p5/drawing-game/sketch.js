var circle = {
    x : 300,
    y : 300,
    width : 20
}

function setup(){
    createCanvas(600, 600);
    background(255, 200, 90);
}

function draw(){
    noStroke();
    fill(255, 175, 90);
    ellipse(mouseX, mouseY, circle.width);
    
}
function mouseClicked(){
    background(255, 200, 90);
}