function Circles(x, y, r){
    var options = {
        restitution: 0.8,
        friction: 0.0
    }
    this.body = Bodies.circle(x, y, r, options);
    World.add(world, this.body);
    this.r = r * 2;
    
    this.show = function(){
        var pos = this.body.position;
        var angle = this.body.angle;
        push();
        translate(pos.x, pos.y);
        rotate(angle);
        ellipse(0, 0, this.r);
        pop();
    }
}