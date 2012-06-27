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
package org.kalypso.kalypsosimulationmodel.core.wind;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.kalypso.grid.GeoGridException;
import org.kalypso.grid.IGeoGrid;
import org.kalypso.grid.IGeoGridWalker;
import org.kalypso.grid.IGeoWalkingStrategy;
import org.kalypso.grid.areas.IGeoGridArea;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.GMRectanglesClip;
import org.kalypso.kalypsosimulationmodel.internal.i18n.Messages;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * @author ig
 * 
 */
public class SimpleWindDataGridWalkingStrategy implements IGeoWalkingStrategy
{

  public SimpleWindDataGridWalkingStrategy( )
  {
    super();
  }

  /**
   * @see org.kalypso.grid.IGeoWalkingStrategy#walk(org.kalypso.grid.IGeoGrid, org.kalypso.grid.IGeoGridWalker,
   *      org.kalypso.grid.areas.IGeoGridArea, org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public Object walk( final IGeoGrid grid, final IGeoGridWalker pWalker, final IGeoGridArea walkingArea, IProgressMonitor monitor ) throws GeoGridException, OperationCanceledException
  {
    try
    {
      IWindGeoGridWalker lWalker = (IWindGeoGridWalker) pWalker;
      /* Monitor. */
      if( monitor == null )
        monitor = new NullProgressMonitor();

      /* Start the processing. */
      pWalker.start( grid );

      final RectifiedGridDomain lGridDescriptor = lWalker.getGridDescriptorOrig();
      final String lStrCRS = lWalker.getEnvelopeToVisit().getCoordinateSystem();
      GM_Envelope lGmEnvelope = null;
      try
      {
        lGmEnvelope = lGridDescriptor.getGM_Envelope( lStrCRS );
      }
      catch( Exception e2 )
      {
        throw new GeoGridException( Messages.getString("SimpleWindDataGridWalkingStrategy_0"), e2 ); //$NON-NLS-1$
      }
      final GM_Envelope env = GMRectanglesClip.getIntersectionEnv( lWalker.getEnvelopeToVisit(), lGmEnvelope );
      GM_Position lGmPointOrigin = null;
      double lDoubleCellSizeX = 0;
      double lDoubleCellSizeY = 0;

      try
      {
        lGmPointOrigin = lGridDescriptor.getOrigin( lStrCRS ).getPosition();
        lDoubleCellSizeX = lGridDescriptor.getOffsetX( lStrCRS );
        lDoubleCellSizeY = lGridDescriptor.getOffsetY( lStrCRS );
      }
      catch( Exception e1 )
      {
        e1.printStackTrace();
      }
      final double xmin = env.getMin().getX();
      int colStart = (int) Math.round( (xmin - lGmPointOrigin.getX()) / lDoubleCellSizeX );
      if( colStart < 0 )
        colStart= 0;
      final double ymin = env.getMin().getY();
      int rowStart = (int) Math.round( (ymin - lGmPointOrigin.getY()) / lDoubleCellSizeY );
      if( rowStart < 0 )
        rowStart = 0;
      int lIntScale = 1;
      

      if( colStart < lGridDescriptor.getNumColumns() && rowStart < lGridDescriptor.getNumRows() /*&& col >= 0 && row >= 0 */)
      {

        int N_COL_ENV = (int) Math.round( env.getWidth() / lDoubleCellSizeX ) + colStart + 2;
        int N_ROW_ENV = (int) Math.round( env.getHeight() / lDoubleCellSizeY ) + rowStart + 2;
        if( N_COL_ENV >= lGridDescriptor.getNumColumns() )
          N_COL_ENV = lGridDescriptor.getNumColumns() - 1;
        if( N_ROW_ENV >= lGridDescriptor.getNumRows() )
          N_ROW_ENV = lGridDescriptor.getNumRows() - 1;

        /* Monitor. */
        monitor.beginTask( Messages.getString("SimpleWindDataGridWalkingStrategy_1"), N_COL_ENV * N_ROW_ENV ); //$NON-NLS-1$

        for( int i = rowStart; i < N_ROW_ENV; i += lIntScale )
        {
          for( int j = colStart; j < N_COL_ENV; j += lIntScale )
          {
            try
            {
              Coordinate coordinate = new Coordinate( lGmPointOrigin.getX() + lDoubleCellSizeX * j, lGmPointOrigin.getY() + lDoubleCellSizeY * i );
              lWalker.operate( j, i, coordinate );
              /* Monitor. */
              monitor.worked( 1 );
            }
            catch( Exception e )
            {
              e.printStackTrace();
            }
          }
        }
        lGmPointOrigin = GeometryFactory.createGM_Position( lGmPointOrigin.getX() + ( colStart ) * lDoubleCellSizeX, lGmPointOrigin.getY() + ( rowStart )* ( lDoubleCellSizeY ) );
        RectifiedGridDomain lWrittenGridDesc = NativeWindDataModelHelper.createGridDescriptor( GeometryFactory.createGM_Point( lGmPointOrigin, lStrCRS ), ( N_COL_ENV - colStart ) / lIntScale, ( N_ROW_ENV - rowStart ) / lIntScale, lDoubleCellSizeX, lDoubleCellSizeY );
        lWalker.setGridDescriptorVisited( lWrittenGridDesc );
      }

      /* Monitor. */
      if( monitor.isCanceled() )
        throw new OperationCanceledException( Messages.getString("SimpleWindDataGridWalkingStrategy_2") );  //$NON-NLS-1$

      /* Finish the processing. */
      return lWalker.finish();
    }
    finally
    {
      /* Monitor. */
      monitor.done();
    }
  }


}
