#version 410 core

layout(location = 0) in vec4 vPosition;
layout(location = 1) in vec2 texcoord;

uniform mat4 MVPMat;
out vec2 Texcoord;

void
main()
{
    Texcoord = texcoord;
    gl_Position = MVPMat * vPosition;
}
