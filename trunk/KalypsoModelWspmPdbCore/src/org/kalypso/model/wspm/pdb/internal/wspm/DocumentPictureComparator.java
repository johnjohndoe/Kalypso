/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.model.wspm.pdb.internal.wspm;

import java.util.Comparator;

import javax.activation.MimeType;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.deegree.framework.util.MimeTypeMapper;
import org.kalypso.commons.java.activation.MimeTypeUtils;
import org.kalypso.model.wspm.pdb.db.mapping.Document;

/**
 * Sorts {@link Document}s by their file ending. Pictures come first.
 *
 * @author Gernot Belger
 */
public class DocumentPictureComparator implements Comparator<Document>
{
  private static final int CATEGORY_IMAGE = 0;

  private static final int CATEGORY_NO_IMAGE = 1;

  @Override
  public int compare( final Document o1, final Document o2 )
  {
    final int category1 = findCategory( o1 );
    final int category2 = findCategory( o2 );

    if( category1 == category2 )
      return compareByFilename( o1, o2 );

    return category1 - category2;
  }

  /**
   * Brings images to top.<br/>
   * TODO: we might define a map from mime-types to categories.
   */
  private int findCategory( final Document doc )
  {
    final MimeType mt = MimeTypeUtils.createQuietly( doc.getMimetype() );

    if( mt == null )
      return CATEGORY_NO_IMAGE;

    final boolean isImage = MimeTypeMapper.isImageType( mt.getBaseType() );
    if( isImage )
      return CATEGORY_IMAGE;

    return CATEGORY_NO_IMAGE;
  }

  /**
   * Sorts by extension, than by filename.
   */
  private int compareByFilename( final Document o1, final Document o2 )
  {
    final String filename1 = o1.getFilename();
    final String filename2 = o2.getFilename();

    final String ext1 = FilenameUtils.getExtension( filename1 );
    final String ext2 = FilenameUtils.getExtension( filename2 );

    if( ObjectUtils.equals( ext1, ext2 ) )
      return filename1.compareToIgnoreCase( filename2 );

    return ext1.compareTo( ext2 );
  }
}