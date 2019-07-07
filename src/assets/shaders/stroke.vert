#version 330 core

layout(location = 0) in vec4 vPosition;
uniform vec4 vertexColor;

uniform mat4 MVPMat;
out vec4 fragmentColor;

void
main()
{
  fragmentColor = vertexColor;
  gl_Position = MVPMat * vPosition;
}
