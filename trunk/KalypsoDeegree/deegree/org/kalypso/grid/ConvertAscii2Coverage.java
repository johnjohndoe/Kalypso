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
package org.kalypso.grid;

import java.io.File;

import org.kalypsodeegree.model.coverage.GridRange;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain.OffsetVector;
import org.kalypsodeegree_impl.model.cv.GridRange_Impl;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * Class for reading and writing ascii-Grid files
 * 
 * @author Dirk Kuch
 */
public abstract class ConvertAscii2Coverage
{
  /**
   * Creates an RectifiedGridCoverage against the given .asc file.
   * <p>
   * Reads the header and links to the given file.
   * </p>
   * imports an ascii-grid file
   * 
   * @param in
   *            input file (*.asc)
   * @param cs
   *            the coordinate system for the geometric data of the ascii-grid
   * @return RectifiedGridCoverage
   */
  public static RectifiedGridDomain importGridArc( final File in, final CS_CoordinateSystem cs ) throws Exception
  {
    final AscciiGridReader reader = new AscciiGridReader( in );

    final int nCols = new Integer( reader.getCols() ).intValue();
    final int nRows = new Integer( reader.getRows() ).intValue();

    final Double cellSize = reader.getCellSize();
    final OffsetVector offsetX = new OffsetVector( cellSize, 0 );
    final OffsetVector offsetY = new OffsetVector( 0, -cellSize );

    final Double originCornerX = reader.getOriginCornerX();
    final Double originCornerY = reader.getOriginCornerY();

    final Double adjustedCornerY = originCornerY + nRows * cellSize;

    final GM_Point origin = GeometryFactory.createGM_Point( originCornerX, adjustedCornerY, cs );

    final String noDataValue = reader.getNoDataValue();

    final double[] low = { 0.0, 0.0 };
    final double[] high = { nCols, nRows };
    final GridRange gridRange = new GridRange_Impl( low, high );
    final RectifiedGridDomain gridDomain = new RectifiedGridDomain( origin, offsetX, offsetY, gridRange );
    gridDomain.setNoDataValue( noDataValue );

    return gridDomain;
  }
}