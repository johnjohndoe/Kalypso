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
package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URL;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.i18n.Messages;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree.model.geometry.ISurfacePatchVisitable;
import org.kalypsodeegree.model.geometry.ISurfacePatchVisitor;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.bce.gis.io.hmo.HMOReader;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LinearRing;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.index.quadtree.Quadtree;
import com.vividsolutions.jts.io.ParseException;

/**
 * An {@link IElevationProvider} based on an hmo file
 * 
 * @author Patrice Congo
 * @author Madanagopal
 */
public class HMOTerrainElevationModel implements IElevationProvider, ISurfacePatchVisitable<GM_SurfacePatch>
{
  public static final double[][] NO_INTERIOR = {};

  public static final GM_Position[][] NO_INTERIOR_POS = {};

  private double minElevation;

  private double maxElevation;

  private Envelope union;

  private Quadtree triangles;

  private final String crs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

  public HMOTerrainElevationModel( final URL hmoFileURL ) throws IOException, ParseException
  {
    parseFile( hmoFileURL );
  }

  private final void parseFile( final URL hmoFileURL ) throws IOException, ParseException
  {
    final HMOReader hmoReader = new HMOReader( new GeometryFactory() );
    final Reader r = new InputStreamReader( hmoFileURL.openStream() );
    final LinearRing[] rings = hmoReader.read( r );

    this.triangles = new Quadtree();

    TriangleData triangleData;
    minElevation = Double.MAX_VALUE;
    maxElevation = -Double.MAX_VALUE;
    double extremum;

    union = rings[0].getEnvelopeInternal();
    for( final LinearRing ring : rings )
    {
      triangleData = new TriangleData( ring, crs );
      final Envelope envelopeInternal = ring.getEnvelopeInternal();
      triangles.insert( envelopeInternal, triangleData );
      // set min
      extremum = triangleData.getMinElevation();
      if( minElevation > extremum )
      {
        minElevation = extremum;
      }
      // set max
      extremum = triangleData.getMaxElevation();
      if( maxElevation < extremum )
      {
        maxElevation = extremum;
      }

      union.expandToInclude( envelopeInternal );
    }
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#getBoundingBox()
   */
  @Override
  public GM_Envelope getBoundingBox( )
  {
    try
    {
      // GM_Position min = JTSAdapter.wrap( union. ).getEnvelope();
      return org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Envelope( union.getMinX(),// minx,
      union.getMinY(),// miny,
      union.getMaxX(),// maxx,
      union.getMaxY(),// maxy
      getCoordinateSystem() );
    }
    catch( final Throwable th )
    {
      th.printStackTrace();
      return null;
    }
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#getCoordinateSystem()
   */
  @Override
  public String getCoordinateSystem( )
  {
    return crs;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#getElevation(org.kalypsodeegree.model.geometry.GM_Point)
   */
  @Override
  public double getElevation( final GM_Point location )
  {
    try
    {
      final double x = location.getX();
      final double y = location.getY();
      final Point jtsPoint = (Point) JTSAdapter.export( location );
      final Envelope searchEnv = jtsPoint.getEnvelopeInternal();

      final List<TriangleData> list = triangles.query( searchEnv );

      if( list.isEmpty() )
        return Double.NaN;

      for( final TriangleData data : list )
      {
        if( data.contains( jtsPoint ) )
          return data.computeZOfTrianglePlanePoint( x, y );
      }
      return Double.NaN;
    }
    catch( final Throwable th )
    {
      throw new RuntimeException( Messages.getString( "org.kalypso.kalypsosimulationmodel.core.terrainmodel.HMOTerrainElevationModel.0" ), th ); //$NON-NLS-1$
    }
  }

  /**
   * @see org.kalypsodeegree.model.geometry.ISurfacePatchVisitable#acceptSurfacePatches(org.kalypsodeegree.model.geometry.GM_Envelope,
   *      org.kalypsodeegree.model.geometry.ISurfacePatchVisitor, org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public void acceptSurfacePatches( final GM_Envelope envToVisit, final ISurfacePatchVisitor<GM_SurfacePatch> surfacePatchVisitor, final IProgressMonitor monitor ) throws CoreException
  {
    Assert.throwIAEOnNullParam( envToVisit, "envToVisit" ); //$NON-NLS-1$
    Assert.throwIAEOnNullParam( surfacePatchVisitor, "surfacePatchVisitor" ); //$NON-NLS-1$
    // TODO; export the whole env at once
    final Coordinate max = JTSAdapter.export( envToVisit.getMax() );
    final Coordinate min = JTSAdapter.export( envToVisit.getMin() );
    final Envelope jtsEnv = new Envelope( min, max );
    final List< ? > triToVisit = triangles.query( jtsEnv );

    monitor.beginTask( "", triToVisit.size() ); //$NON-NLS-1$

    final IProgressMonitor nullMonitor = new NullProgressMonitor(); // we reuse the same null-monitor here to avoid
    // production of thousands of sub-monitor, that do
    // nothing...
    for( final Object tri : triToVisit )
    {
      ((TriangleData) tri).acceptSurfacePatches( envToVisit, surfacePatchVisitor, nullMonitor );
      ProgressUtilities.worked( monitor, 1 );
    }
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#getMaxElevation()
   * @returns a valid Maximum Elevation value or Double.NaN and not the default -Double.MAX_VALUE
   */
  @Override
  public double getMaxElevation( )
  {
    return maxElevation == -Double.MAX_VALUE ? Double.NaN : maxElevation;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#getMinElevation()
   * @returns either a valid Minimum Elevation value or Double.NaN and not the default Double.MAX_VALUE
   */
  @Override
  public double getMinElevation( )
  {
    return minElevation == Double.MAX_VALUE ? Double.NaN : minElevation;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#setCoordinateSystem(java.lang.String)
   */
  @Override
  public void setCoordinateSystem( final String coordinateSystem )
  {
    // TODO
  }
}
