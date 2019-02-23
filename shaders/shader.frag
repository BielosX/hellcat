#version 330 core

uniform vec3 point_light_pos;
uniform float point_light_intensity;

in vec4 vert_pos;
out vec3 color;

void main() {
    float dist = length(vert_pos, (point_light_pos, 1));
    float coef = (point_light_intensity/(dist*dist));
    color = clamp(coef * vec3(1,0,0), vec3(0,0,0), vec3(1,1,1));
}
