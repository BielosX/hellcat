#version 430 core

layout(location = 0) in vec3 vertexPosition;
layout(location = 1) in vec3 normalPosition;
layout(location = 2) in vec2 vertexUV;

uniform mat4 view;
uniform mat4 projection;
uniform vec3 position;

out vec2 UV;

mat3 rotateY(float angle) {
    return mat3(
                vec2(cos(angle), 0), sin(angle),
                vec2(0, 1), 0,
                vec2(-sin(angle), 0), cos(angle)
            );
}

float calcAngle(vec3 v1, vec3 v2) {
    return acos(dot(normalize(v1), normalize(v2)));
}

void main() {
    vec4 vertInCam = view * vec4(vertexPosition, 1);
    vec3 toCamera = vec3(0,0,0) - vertInCam.xyz;
    vec3 norm = mat3(view) * normalPosition;
    float angle = calcAngle(norm, toCamera);
    mat3 rotMat = rotateY(angle);
    vec3 newNorm = rotMat * norm;
    float newAngle = calcAngle(newNorm, toCamera);
    if (newAngle > angle) {
        rotMat = rotateY(-angle);
    }
    gl_Position = projection * view * vec4((rotMat * vertexPosition) + position, 1);
    UV = vertexUV;
}
