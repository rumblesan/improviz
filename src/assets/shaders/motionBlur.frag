#version 410 core

in vec2 Texcoord;
out vec4 outColor;

uniform sampler2D texFramebuffer;
uniform sampler2D lastFrame;
uniform float mixRatio;

void main()
{
    vec4 texel1 = texture(texFramebuffer, Texcoord);
    vec4 texel2 = texture(lastFrame, Texcoord);

    outColor = mix(texel1, texel2, mixRatio);
}
