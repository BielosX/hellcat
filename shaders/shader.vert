#version 430 core

layout(location = 0) in vec3 vertexPosition;
layout(location = 1) in vec3 normalPosition;
//layout(std430, binding = 1) buffer pointLightPos {
//    vec4 pLightPos[];
//};
//layout(std430, binding = 2) buffer pointLightIntensity {
//    float intensity;
//};

uniform mat4 projection;
uniform mat4 view;
uniform mat4 model;

out vec4 vert_pos;
out vec4 norm_pos;

void main() {
    gl_Position = projection * view * model * vec4(vertexPosition, 1);
    vert_pos = model * vec4(vertexPosition, 1);
    norm_pos = model * vec4(normalPosition, 1);
}
