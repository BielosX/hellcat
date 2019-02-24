#version 430 core

layout(std430, binding = 1) buffer pointLightPos {
    vec4 pLightPos[];
};
layout(std430, binding = 2) buffer pointLightIntensity {
    float intensity[];
};

flat in vec4 vert_pos;
out vec3 color;

void main() {
    float dist = length(pLightPos[0] - vert_pos);
    float coef = (intensity[0]/(dist*dist));
    color = coef * vec3(1,0,0);
}
