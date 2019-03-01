#version 430 core

layout(std430, binding = 2) buffer pointLightPos {
    vec4 pLightPos[];
};
layout(std430, binding = 3) buffer pointLightIntensity {
    float intensity[];
};

in vec4 vert_pos;
in vec4 norm_pos;
out vec3 color;

void main() {
    vec3 vertToLight = pLightPos[0].xyz - vert_pos.xyz;
    float dist = length(vertToLight);
    vec3 unit = normalize(vertToLight);
    float cosinus = clamp(dot(unit, normalize(norm_pos.xyz)), 0, 1);
    float coef = (intensity[0]/(dist*dist)) * cosinus;
    color = coef * vec3(1,0,0);
}
