#version 330 core
in vec2 TexCoords;
out vec4 color;

uniform sampler2D text;
uniform vec4 textColor;
uniform vec4 textBGColor;

void main()
{    
    color = mix(textBGColor, textColor, texture(text, TexCoords).r);
}
