#version 430 core

uniform sampler2D textureSampler;

in vec2 UV;
out vec3 color;

void main() {
    vec4 tcolor = texture(textureSampler, UV);
    if (tcolor.a < 0.1) {
        discard;
    }
    color = tcolor.rgb;
}
