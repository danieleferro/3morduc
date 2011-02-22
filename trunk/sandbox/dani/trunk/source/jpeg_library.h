/*
 * jpeg_library.h    
 *
 * This file is part of REAR.
 * Copyright (C) 2010 Daniele Ferro (daniele.ferro86@gmail.com) 
 *                    Loris Fichera (loris.fichera@gmail.com)
 *
 * REAR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * REAR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with REAR.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef __JPEG_LIBRARY__
#define __JPEG_LIBRARY__

#include "../jpeg-8c/jinclude.h"
#include "../jpeg-8c/jerror.h"
#include "../jpeg-8c/jpeglib.h"

typedef struct {

  struct jpeg_source_mgr pub;	/* public fields */
  JOCTET eoi_buffer[2];		/* a place to put a dummy EOI */

} my_source_mgr;

typedef my_source_mgr * my_src_ptr;

METHODDEF(void)
init_source (j_decompress_ptr cinfo);

METHODDEF(boolean)
fill_input_buffer (j_decompress_ptr cinfo);

METHODDEF(void)
skip_input_data (j_decompress_ptr cinfo, long num_bytes);

METHODDEF(void)
term_source (j_decompress_ptr cinfo);

GLOBAL(void)
jpeg_memory_src (j_decompress_ptr cinfo, const JOCTET * buffer, size_t bufsize);

#endif

