#version 410 core

in vec2 Texcoord;
out vec4 outColor;
uniform sampler2D inputTexture;

void main()
{
    outColor = texture(inputTexture, Texcoord);
}
