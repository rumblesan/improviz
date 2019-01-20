#version 410 core

in vec2 Texcoord;
out vec4 outColor;
uniform sampler2D texFramebuffer;
void main()
{
    outColor = texture(texFramebuffer, Texcoord);
}
