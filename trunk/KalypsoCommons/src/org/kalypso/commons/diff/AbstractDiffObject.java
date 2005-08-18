/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.commons.diff;

import java.io.InputStream;

import org.kalypso.contribs.java.io.StreamUtilities;

/**
 * @author doemming
 */
public abstract class AbstractDiffObject implements IDiffObject
{
  /**
   * @see org.kalypso.commons.diff.IDiffObject#getDiffComparator(java.lang.String)
   */
  public IDiffComparator getDiffComparator( final String path )
  {
    DiffComparatorRegistry instance = DiffComparatorRegistry.getInstance();
    final String suffix = "." + path.replaceAll( ".+\\.", "" );
    if( instance.hasComparator( suffix ) )
      return instance.getDiffComparator( suffix );
    return new IDiffComparator()
    {
      /**
       * @see org.kalypso.commons.diff.IDiffComparator#diff(org.kalypso.commons.diff.IDiffLogger, java.lang.Object,
       *      java.lang.Object)
       */
      public boolean diff( IDiffLogger logger, Object content, Object content2 ) throws Exception
      {
        final InputStream c1 = (InputStream)content;
        final InputStream c2 = (InputStream)content2;
        final boolean hasDiff = !StreamUtilities.isEqual( c1, c2 );
        if( hasDiff )
          logger.log( IDiffComparator.DIFF_CONTENT, path );
        else
          logger.log( IDiffComparator.DIFF_OK, path );
        return hasDiff;
      }
    };
  }
}
