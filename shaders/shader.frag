#version 430 core

layout(std430, binding = 3) buffer pointLightPos {
    vec4 pLightPos[];
};
layout(std430, binding = 4) buffer pointLightIntensity {
    float intensity[];
};


uniform sampler2D textureSampler;

in vec4 vert_pos;
in vec4 norm_pos;
in vec2 UV;
out vec3 color;

void main() {
    vec3 vertToLight = pLightPos[0].xyz - vert_pos.xyz;
    float dist = length(vertToLight);
    vec3 unit = normalize(vertToLight);
    float cosinus = clamp(dot(unit, normalize(norm_pos.xyz)), 0, 1);
    float coef = (intensity[0]/(dist*dist)) * cosinus;
    vec3 l = coef * vec3(1,1,1);
    color = l + vec3(0.1,0,0) + texture(textureSampler, UV).rgb;
}
