#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_OUTLINE_H

FT_EXPORT(FT_Error) FT_Outline_Decompose_Face(FT_Face face, const FT_Outline_Funcs* funcs, void* user) {
  return FT_Outline_Decompose(&face->glyph->outline, funcs, user);
}


FT_EXPORT(void) FT_Get_Glyph_Metrics(FT_Face face, FT_Pos* advance, FT_Pos* extents_x, FT_Pos* extents_y, FT_Pos* extents_w, FT_Pos* extents_h) {
  FT_Glyph_Metrics metrics = face->glyph->metrics;

  *advance = metrics.horiAdvance;
  *extents_x = metrics.horiBearingX;
  *extents_y = metrics.horiBearingY;
  *extents_w = metrics.width;
  *extents_h = metrics.height;
}

FT_EXPORT(void) FT_Get_Font_Metrics(FT_Face face, FT_Pos* ascent, FT_Pos* descent, FT_Pos* line_gap) {
  FT_Size_Metrics metrics = face->size->metrics;

  *ascent = metrics.ascender;
  *descent = metrics.descender;
  *line_gap = metrics.height - (metrics.ascender - metrics.descender);
}
