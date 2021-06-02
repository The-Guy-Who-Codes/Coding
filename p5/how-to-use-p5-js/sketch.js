function setup() { // what happens when the script is started
  createCanvas(600, 600);   //creates canvas and size in pixels
  background(255, 250, 220);    //defines colour of canvas in RGB
}

function draw() {
  fill(255, 0, 0); // this defines what all the shapes below it will be filled with
  rect(100, 100, 75, 75); // makes a rectangle (x, y, width, height)
  fill(255);  // the fill statement lasts forever unless if you do a fill statement again
  rect(200, 200, 30, 30);
}