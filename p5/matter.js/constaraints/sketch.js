
var Engine = Matter.Engine,
    World = Matter.World,
    Bodies = Matter.Bodies,
    constraint = Matter.Constraint;
var engine;
var world;
var bondaries;
var circles = [];

function setup(){
    createCanvas(500, 500);
    engine = Engine.create();
    world = engine.world;
    Engine.run(engine);
    boundaries = new Ground(250, 490, width, 50);
    var c1 = circles.push(new Circles(250, 100, 15));
    var c2 = circles.push(new Circles(240, 150, 15));

    var options = {
        bodyA: c2.body,
        bodyB: c1.body,
        length: 50,
        stiffness: 0.4

    }
    var Constraint = constraint.create(options);
    World.add(world, constraint)

}

function draw(){
    background(70); 
    boundaries.show();
    for(var i = 0; i < circles.length; i++){
        circles[i].show();
    }
}
