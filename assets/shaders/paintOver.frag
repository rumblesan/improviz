#version 410 core

in vec2 Texcoord;
out vec4 outColor;

uniform sampler2D texFramebuffer;
uniform sampler2D lastFrame;
uniform sampler2D depth;

void main()
{
    float z = texture(depth, Texcoord).r;         // fetch the z-value from our depth texture
    //float n = 0.1;                                // the near plane
    //float f = 100.0;                              // the far plane
    //float c = (2.0 * n) / (f + n - z * (f - n));  // convert to linear values 

    vec4 texel1 = texture(texFramebuffer, Texcoord);
    vec4 texel2 = texture(lastFrame, Texcoord);

    if (z < 1.0) {
        outColor = texel1;
    } else {
        outColor = texel2;
    }

}
