
var f = 0
function setup(){
    createCanvas(850, 1024);
    noStroke();
}
function keyTyped(){
    if(key === "q"){
        background(255, 255, 255);
    }
    if(key === "w"){
        noFill();
    }
    if(key === "s"){
        fill(f);
    }
}
function draw(){
    ellipse(mouseX, mouseY, 20);
}
