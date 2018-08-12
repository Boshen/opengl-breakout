#version 410 core

in vec2 TexCoords;
out vec4 color;

uniform sampler2D image;
uniform vec3 blockColor;

void main() {
  color = vec4(blockColor, 1.0) * texture(image, TexCoords);
}
