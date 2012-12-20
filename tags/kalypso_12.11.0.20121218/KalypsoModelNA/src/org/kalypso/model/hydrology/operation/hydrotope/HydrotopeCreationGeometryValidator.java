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
package org.kalypso.model.hydrology.operation.hydrotope;

import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.model.hydrology.internal.ModelNA;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypsodeegree_impl.model.sort.SpatialIndexExt;

import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.operation.valid.IsValidOp;
import com.vividsolutions.jts.operation.valid.TopologyValidationError;

/**
 * @author Gernot Belger
 */
public class HydrotopeCreationGeometryValidator
{
  private final SpatialIndexExt m_index;

  private final List<Polygon> m_allElements;

  private final String m_label;

  public HydrotopeCreationGeometryValidator( final String label, final SpatialIndexExt index )
  {
    m_label = label;
    m_index = index;

    /* Fetch all elements */
    final Envelope boundingBox = index.getBoundingBox();
    m_allElements = index.query( boundingBox );
  }

  public IStatus checkGeometryCorrectness( final IProgressMonitor monitor )
  {
    final IStatusCollector log = new StatusCollector( ModelNA.PLUGIN_ID );

    final String subTaskFormat = String.format( Messages.getString("HydrotopeCreationGeometryValidator_0"), m_label ); //$NON-NLS-1$

    for( int i = 0; i < m_allElements.size(); i++ )
    {
      ProgressUtilities.workedModulo( monitor, i, m_allElements.size(), 11, subTaskFormat );

      final Polygon polygon = m_allElements.get( i );

      final IsValidOp isValidOp = new IsValidOp( polygon );

      if( polygon == null )
        log.add( IStatus.WARNING, Messages.getString("HydrotopeCreationGeometryValidator_1") ); //$NON-NLS-1$
      else if( polygon.getArea() < FeatureListGeometryIntersector.MIN_AREA )
        addWarning( log, String.format( Messages.getString("HydrotopeCreationGeometryValidator_2"), polygon.getArea() ), polygon ); //$NON-NLS-1$
      else if( !isValidOp.isValid() )
      {
        final TopologyValidationError error = isValidOp.getValidationError();
        addWarning( log, error.getMessage(), polygon );
      }
    }

    return log.asMultiStatus( Messages.getString("HydrotopeCreationGeometryValidator_3") ); //$NON-NLS-1$
  }

  public IStatus checkSelfIntersection( final IProgressMonitor monitor )
  {
    final IStatusCollector log = new StatusCollector( ModelNA.PLUGIN_ID );

    final String subTaskFormat = String.format( Messages.getString("HydrotopeCreationGeometryValidator_4"), m_label ); //$NON-NLS-1$

    double totalArea = 0.0;
    double totalIntersectionArea = 0.0;

    for( int i = 0; i < m_allElements.size(); i++ )
    {
      ProgressUtilities.workedModulo( monitor, i, m_allElements.size(), 11, subTaskFormat );

      final Polygon polygon = m_allElements.get( i );
      final HydrotopeUserData polygonData = (HydrotopeUserData) polygon.getUserData();
      final int polygonNumber = polygonData.getCount();

      totalArea += polygon.getArea();

      final List<Polygon> otherPolygons = m_index.query( polygon.getEnvelopeInternal() );
      for( final Polygon other : otherPolygons )
      {
        if( other == polygon )
          continue;

        final HydrotopeUserData otherData = (HydrotopeUserData) other.getUserData();
        final int otherNumber = otherData.getCount();

        if( polygonNumber == -1 || otherNumber == -1 )
        {
          System.out.println( "Should never happen" ); //$NON-NLS-1$
        }

        // REMARK: this prevents checking intersection twice
        if( polygonNumber >= otherNumber )
          continue;

        if( polygon.overlaps( other ) )
        {
          final Geometry intersection = FeatureListGeometryIntersector.forceIntersection( polygon, other, log );

          final double intersectionArea = intersection.getArea();
          totalIntersectionArea += intersectionArea;

          final double intersectionPercent = intersectionArea / polygon.getArea() * 100;
          if( intersectionPercent >= 0.01 )
          {

            final String format = String.format( Messages.getString("HydrotopeCreationGeometryValidator_5"), intersectionArea, intersectionPercent ); //$NON-NLS-1$
            addWarning( log, format, polygon );
          }
        }
      }
    }

    /* Show info if there is a small intersection, warning if intersection is > 1.0 % */
    final double totalIntersectionPercent = (totalIntersectionArea / totalArea) * 100;
    if( totalIntersectionArea >= 0.01 || totalIntersectionPercent >= 0.01 )
    {
      final int severity = totalIntersectionPercent >= 1.0 ? IStatus.WARNING : IStatus.INFO;

      final String format = String.format( Messages.getString("HydrotopeCreationGeometryValidator_6"), totalIntersectionArea, totalIntersectionPercent ); //$NON-NLS-1$
      log.add( severity, format );
    }

    return log.asMultiStatus( Messages.getString("HydrotopeCreationGeometryValidator_7") ); //$NON-NLS-1$
  }

  private static void addWarning( final IStatusCollector log, final String message, final Polygon polygon )
  {
    final HydrotopeUserData data = (HydrotopeUserData) polygon.getUserData();
    final String label = data.toErrorString();
    log.add( IStatus.WARNING, Messages.getString("HydrotopeCreationGeometryValidator_8"), null, label, message ); //$NON-NLS-1$
  }
}