#version 330 core

layout(location = 0) in vec3 vertexPosition;

uniform mat4 projection;
uniform mat4 word;
uniform mat4 model;

void main() {
    gl_Position = projection * word * model * vec4(vertexPosition, 1);
}
