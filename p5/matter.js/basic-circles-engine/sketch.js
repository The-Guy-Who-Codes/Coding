// module aliases
var Engine = Matter.Engine,
    //Render = Matter.Render,
    World = Matter.World,
    Bodies = Matter.Bodies;
var engine;
var circles = [];
var boundaries = [];
var world;

function setup(){
    createCanvas(500, 500);      
    // create an engine
    engine = Engine.create();
    world = engine.world;
    Engine.run(engine);
    boundaries.push(new Boundary(250, 440, width, 50, -0.3));
    boundaries.push(new Boundary(150, 175, width, 30, 0.3)); // top one
    World.add(world, boundaries);
}

function draw(){
    background(51);
    circles.push(new Circle(200, 75, 10));
    for(var i = 0; i < circles.length; i++){
        circles[i].show();
        if(circles[i].isoffscreen()){  // this only removes the p5.js circles
            circles[i].removefromworld();
            circles.splice(i, 1);
            i--; // this removes the filickering
        }
    }
    for(var x = 0; x < boundaries.length; x++){
        boundaries[x].show();
    }
}

/*function mouseDragged(){
    circles.push(new Circle(mouseX, mouseY, random(15, 25)));
}*/
