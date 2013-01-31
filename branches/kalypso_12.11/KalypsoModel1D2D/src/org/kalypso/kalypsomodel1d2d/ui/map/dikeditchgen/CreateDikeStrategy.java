/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.kalypsomodel1d2d.ui.map.dikeditchgen;

import java.math.BigDecimal;
import java.math.RoundingMode;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.elevation.IElevationModel;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree.model.geometry.GM_PolygonPatch;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.densify.Densifier;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.CoordinateFilter;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.simplify.DouglasPeuckerSimplifier;

/**
 * @author kurzbach
 */
public class CreateDikeStrategy implements CreateStructuredNetworkStrategy
{
  private final double[] m_ringDistancesLeft;

  private final double[] m_ringDistancesRight;

  private final double m_outerRightWidth;

  private final double m_outerLeftWidth;

  final IElevationModel m_elevationModel;

  final double m_innerElevation;

  private final Geometry m_network;

  public CreateDikeStrategy( final Geometry network, final double outerLeftWidth, final double outerRightWidth, final double innerWidth, final double innerElevation, final IElevationModel elevationModel )
  {
    m_network = network;
    m_outerLeftWidth = outerLeftWidth;
    m_outerRightWidth = outerRightWidth;
    m_innerElevation = innerElevation;
    m_elevationModel = elevationModel;
    m_ringDistancesLeft = getRingDistances( innerWidth, outerLeftWidth );
    m_ringDistancesRight = getRingDistances( innerWidth, outerRightWidth );
  }

  private void addBoundary( final TriangulationBuilder builder ) throws GM_Exception
  {
    final String coordinateSystem = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
    final CoordinateFilter interpolateElevationFilter = new InterpolateElevationFilter( coordinateSystem, m_elevationModel );
    final Geometry outer = getRingPolygons( m_network, m_outerLeftWidth, m_outerRightWidth );
    final double outerDensifyTol = m_ringDistancesLeft[0] * 2 * Math.pow( 2, getRingCount() );
    final Geometry outerDensified = Densifier.densify( outer, outerDensifyTol );
    outerDensified.apply( interpolateElevationFilter );

    final GM_Object outerRing = JTSAdapter.wrap( outerDensified, coordinateSystem );
    if( outerRing instanceof GM_Polygon )
      builder.addBoundary( (GM_Polygon)outerRing );
    else
    {
      final GM_Polygon[] allSurfaces = ((GM_MultiSurface)outerRing).getAllSurfaces();
      for( final GM_Polygon polygon : allSurfaces )
        builder.addBoundary( polygon );
    }
  }

  private void addBreaklines( final TriangulationBuilder tinBuilder ) throws GM_Exception
  {
    final String coordinateSystem = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
    final CoordinateFilter setInnerElevationFilter = new CoordinateFilter()
    {
      @Override
      public void filter( final Coordinate coord )
      {
        coord.z = m_innerElevation;
      }
    };

    double innerDensifyTolerance = m_ringDistancesLeft[0] * 4;

    // add inner polygon rings
    for( int i = 0; i < getRingCount(); i++ )
    {
      // add all inner polygon rings as breaklines
      final Geometry inner = getRingPolygons( m_network, m_ringDistancesLeft[i], m_ringDistancesRight[i] );
      final Geometry innerDensified = Densifier.densify( inner, innerDensifyTolerance );
      if( i == 0 )
        innerDensified.apply( setInnerElevationFilter );

      for( int j = 0; j < innerDensified.getNumGeometries(); j++ )
      {
        final GM_Polygon innerRing = (GM_Polygon)JTSAdapter.wrap( innerDensified.getGeometryN( j ), coordinateSystem );
        final GM_PolygonPatch surfacePatch = innerRing.getSurfacePatch();
        final GM_Position[] exteriorRing = surfacePatch.getExteriorRing();
        final GM_Curve innerRingExteriorAsCurve = GeometryFactory.createGM_Curve( exteriorRing, coordinateSystem );
        tinBuilder.addBreakLine( innerRingExteriorAsCurve );
        final GM_Position[][] interiorRings = surfacePatch.getInteriorRings();
        if( interiorRings != null )
        {
          for( final GM_Position[] ring : interiorRings )
          {
            final GM_Curve innerRingHoleAsCurve = GeometryFactory.createGM_Curve( ring, coordinateSystem );
            tinBuilder.addBreakLine( innerRingHoleAsCurve );
          }
        }
      }

      // double densification distance with each inner ring
      innerDensifyTolerance = innerDensifyTolerance * 2;
    }
  }

  private int getRingCount( )
  {
    final int ringCountLeft = m_ringDistancesLeft.length;
    final int ringCountRight = m_ringDistancesRight.length;
    return ringCountLeft < ringCountRight ? ringCountLeft : ringCountRight;
  }

  private double[] getRingDistances( final double innerWidth, final double outerWidth )
  {
    final double dieoffLeft = new BigDecimal( Math.log( 1 + outerWidth / innerWidth ) ).divide( new BigDecimal( Math.log( 2 ) ), RoundingMode.HALF_UP ).doubleValue() - 1;
    final int ringCount = Math.max( (int)Math.floor( dieoffLeft ), 1 );
    final double[] ringDistancesLeft = new double[ringCount];
    ringDistancesLeft[0] = innerWidth;
    for( int j = 1; j < ringCount; j++ )
      ringDistancesLeft[j] = ringCount / dieoffLeft * innerWidth * (Math.pow( 2, j + 1 ) - 1);
    return ringDistancesLeft;
  }

  private static Geometry getRingPolygons( final Geometry network, final double leftWidth, final double rightWidth )
  {
    final Geometry buffer = LineStringBufferBuilder.buffer( network, leftWidth, rightWidth );
    return DouglasPeuckerSimplifier.simplify( buffer.getGeometryN( 0 ), Math.min( leftWidth, rightWidth ) / 6 );
  }

  @Override
  public Control createControl( final Composite body, final FormToolkit toolkit, final IMapModell mapModell )
  {
    return null;
  }

  @Override
  public IStatus createMesh( final TriangulationBuilder tinBuilder ) throws CoreException
  {
    try
    {
      addBoundary( tinBuilder );
      addBreaklines( tinBuilder );
      tinBuilder.finish();
    }
    catch( final GM_Exception e )
    {
      throw new CoreException( StatusUtilities.statusFromThrowable( e ) );
    }
    return Status.OK_STATUS;
  }
}
