function Circle(x, y, r){
    this.body = Bodies.circle(x, y, r);
    this.body.friction = 0.0;
    this.body.restitution = 0.8; // sets how much the body will bounce back
    this.r = r * 2; // p5 uses a diamiter matter.js uses a radius
    World.add(world, this.body);
    strokeWeight(1);
    stroke(255);
    fill(170);
    
    this.isoffscreen = function(){
        var pos = this.body.position;
        return(pos.y > height + 100);
    }
    this.removefromworld = function(){
        World.remove(world, this.body);  // this removes the circles from the physics engine
    }

    this.show = function(){
        var pos = this.body.position;
        var angle = this.body.angle;
        push();
        translate(pos.x, pos.y);
        rotate(angle);
        rectMode(CENTER);
        ellipse(0, 0, this.r);
        pop();
    }
}