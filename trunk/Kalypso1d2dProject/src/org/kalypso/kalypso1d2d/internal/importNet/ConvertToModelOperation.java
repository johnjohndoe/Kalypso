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
package org.kalypso.kalypso1d2d.internal.importNet;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.gml.processes.constDelaunay.ConstraintDelaunayHelper;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;
import org.kalypso.kalypsomodel1d2d.ui.map.util.Add2DElementsCommand;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree.model.geometry.GM_PolygonPatch;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.bce.gis.io.zweidm.IPolygonWithName;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Polygon;

/**
 * @author Gertno Belger
 */
class ConvertToModelOperation implements ICoreRunnableWithProgress
{
  private final CommandableWorkspace m_discWorkspace;

  private final IPolygonWithName[] m_elements;

  public ConvertToModelOperation( final CommandableWorkspace discWorkspace, final IPolygonWithName[] elements )
  {
    m_discWorkspace = discWorkspace;
    m_elements = elements;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    try
    {
      monitor.beginTask( Messages.getString( "ConvertToModelOperation.0" ), 100 ); //$NON-NLS-1$

      /* Create rings */
      final List<GM_PolygonPatch> rings = createRings( m_elements, new SubProgressMonitor( monitor, 50 ) );

      monitor.subTask( Messages.getString( "ConvertToModelOperation.1" ) ); //$NON-NLS-1$
      final Add2DElementsCommand command = new Add2DElementsCommand( m_discWorkspace, rings );
      m_discWorkspace.postCommand( command );

      monitor.done();

      return Status.OK_STATUS;
    }
    catch( final OperationCanceledException e )
    {
      return Status.CANCEL_STATUS;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return new Status( IStatus.ERROR, Kalypso1d2dProjectPlugin.PLUGIN_ID, Messages.getString( "ConvertToModelAction_2" ), e ); //$NON-NLS-1$
    }
  }

  private List<GM_PolygonPatch> createRings( final IPolygonWithName[] elements, final IProgressMonitor monitor ) throws GM_Exception
  {
    monitor.beginTask( "Create triangles", elements.length ); //$NON-NLS-1$

    final List<GM_PolygonPatch> rings = new ArrayList<>();

    for( final IPolygonWithName element : elements )
    {
      final Polygon polygon = element.getPolygon();

      final GM_PolygonPatch[] ringsFromPoly = createRings( polygon );

      for( final GM_PolygonPatch ring : ringsFromPoly )
      {
        final double area = ring.getArea();
        // FIXME: hot fix -> triangulation seems to produce bad elements sometimes, this prohibits this
        if( area > 1.0 )
          rings.add( ring );
        else
          System.out.println( "Removed small triangle" ); //$NON-NLS-1$
      }

      ProgressUtilities.worked( monitor, 1 );
    }

    monitor.done();

    return rings;
  }

  private GM_PolygonPatch[] createRings( final Polygon polygon ) throws GM_Exception
  {
    final LineString exteriorRing = polygon.getExteriorRing();

    final int numInteriorRing = polygon.getNumInteriorRing();
    final int numPoints = polygon.getNumPoints();

    if( numPoints > 4 || numInteriorRing > 0 )
      return triangulatePolygon( polygon );

    final String srsName = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

    final GM_Position[] positions = JTSAdapter.wrap( exteriorRing.getCoordinates() );
    final GM_PolygonPatch singlePatch = GeometryFactory.createGM_PolygonPatch( positions, null, srsName );

    return new GM_PolygonPatch[] { singlePatch };
  }

  private GM_PolygonPatch[] triangulatePolygon( final Polygon polygon ) throws GM_Exception
  {
    final String srsName = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

    final GM_Polygon poly = (GM_Polygon)JTSAdapter.wrap( polygon, srsName );

    return ConstraintDelaunayHelper.convertToTriangles( poly, srsName, true );
  }
}