function Boundary(x, y, w, h, a){
    var options = {
        friction : 0.0,
        restitution : 0.6,
        isStatic : true,
        angle : a

    }
    fill(170);
    this.body = Bodies.rectangle(x, y, w, h, options);
    this.w = w;
    this.h = h;
    World.add(world, this.body);

    this.show = function(){
        var pos = this.body.position;
        var angle = this.body.angle;
        push();
        translate(pos.x, pos.y);
        rotate(angle);
        rectMode(CENTER);
        rect(0, 0, this.w, this.h);
        pop();
    }

}