/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.gis.doubleraster;

import org.eclipse.core.runtime.IProgressMonitor;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * @author belger
 */
public abstract class AbstractDoubleRaster implements DoubleRaster
{
  /**
   * @return Midpoint of Rasterposition x,y and sets its value to the corresponding cell value.
   */
  public final Coordinate calcCoordinate( final int x, final int y, final Coordinate c )
  {
    final Coordinate coordinate = DoubleRasterUtilities.rasterCellToCoordinate( this, x, y, c );

    final double value = getValueChecked( x, y );
    coordinate.z = value;
    return coordinate;
  }

  public final Object walk( DoubleRasterWalker pwo, final IProgressMonitor monitor ) throws DoubleRasterException
  {
    final int sizeX = getSizeX();
    final int sizeY = getSizeY();
    if( monitor != null )
      monitor.beginTask( "Raster wird durchlaufen", sizeY );

    pwo.start( this );

    final Coordinate tmpCrd = new Coordinate();

    for( int y = sizeY - 1; y >= -1; y-- )
    {
      for( int x = 0; x < sizeX; x++ )
        pwo.operate( x, y, calcCoordinate( x, y, tmpCrd ) );

      if( monitor != null )
        monitor.worked( 1 );

      pwo.afterLine( y );

      if( monitor != null && monitor.isCanceled() )
        throw new DoubleRasterException( "Abbruch durch Benutzer", null );
    }

    return pwo.getResult();
  }

}
