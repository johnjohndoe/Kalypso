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

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.apache.commons.collections.CollectionUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.model.hydrology.binding.HydrotopeCollection;
import org.kalypso.model.hydrology.binding.IHydrotope;
import org.kalypso.model.hydrology.internal.ModelNA;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.MultiPolygon;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.util.PolygonExtracter;

/**
 * @author Gernot Belger
 */
public class HydrotopeBuilder implements ICoreRunnableWithProgress
{
  // FIXME: check! Probably this should be the one of the hydrotopes
  private static final String COORDINATE_SYSTEM = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

  private final String m_logMessage;

  private final Collection<HydrotopeUserData> m_hydrotopeBeans;

  private final HydrotopeCollection m_hydrotopes;

  public HydrotopeBuilder( final Collection<HydrotopeUserData> hydrotopeBeans, final HydrotopeCollection hydrotopes, final String logMessage )
  {
    m_hydrotopeBeans = hydrotopeBeans;
    m_hydrotopes = hydrotopes;
    m_logMessage = logMessage;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws InvocationTargetException
  {
    final IStatusCollector log = new StatusCollector( ModelNA.PLUGIN_ID );

    final int size = m_hydrotopeBeans.size();

    final SubMonitor progress = SubMonitor.convert( monitor, Messages.getString("HydrotopeBuilder_0"), size ); //$NON-NLS-1$

    removeOverlappingGeometriesFromExisting();

    final IFeatureBindingCollection<IHydrotope> hydrotopes = m_hydrotopes.getHydrotopes();

    int count = 0;
    for( final HydrotopeUserData bean : m_hydrotopeBeans )
    {
      if( count % 100 == 0 )
      {
        final String msg = String.format( Messages.getString("HydrotopeBuilder_1"), count + 1, size ); //$NON-NLS-1$
        progress.subTask( msg );
        ProgressUtilities.worked( monitor, 100 );
      }

      try
      {
        final IHydrotope hydrotop = hydrotopes.addNew( IHydrotope.FEATURE_HYDROTOPE );
        bean.configureHydrotope( hydrotop, count );
      }
      catch( final CoreException e )
      {
        log.add( e.getStatus() );
      }

      count++;
    }

    log.add( IStatus.OK, Messages.getString("HydrotopeBuilder_2"), null, count ); //$NON-NLS-1$

    return log.asMultiStatus( m_logMessage );
  }

  private void removeOverlappingGeometriesFromExisting( ) throws InvocationTargetException
  {
    try
    {
      final IFeatureBindingCollection<IHydrotope> hydrotopes = m_hydrotopes.getHydrotopes();

      if( CollectionUtils.isEmpty( hydrotopes ) )
        return;

      final Geometry intersectionArea = buildIntersectionArea();

      final GM_Envelope gmEnvelope = JTSAdapter.wrap( intersectionArea.getEnvelopeInternal(), COORDINATE_SYSTEM );
      final List<IHydrotope> list = hydrotopes.query( gmEnvelope );

      for( final IHydrotope hydrotop : list )
      {
        final Geometry hydrotopGeom = JTSAdapter.export( hydrotop.getGeometry() );
        if( hydrotopGeom.disjoint( intersectionArea ) || hydrotopGeom.touches( intersectionArea ) )
          continue;

        if( hydrotopGeom.coveredBy( intersectionArea ) )
          hydrotopes.remove( hydrotop );
        else
        {
          final Geometry difference = hydrotopGeom.difference( intersectionArea );
          final GM_MultiSurface newHydrotopGeometry = toMultiSurface( difference, COORDINATE_SYSTEM );
          if( newHydrotopGeometry.isEmpty() )
            hydrotopes.remove( hydrotop );
          else
            hydrotop.setGeometry( newHydrotopGeometry );
        }
      }
    }
    catch( final GM_Exception e )
    {
      throw new InvocationTargetException( e );
    }
  }

  private GM_MultiSurface toMultiSurface( final Geometry difference, final String coordinateSystem )
  {
    try
    {
      final List<Polygon> polygons = PolygonExtracter.getPolygons( difference );

      final GeometryFactory factory = difference.getFactory();

      final MultiPolygon multiPolygon = factory.createMultiPolygon( polygons.toArray( new Polygon[polygons.size()] ) );

      return (GM_MultiSurface) JTSAdapter.wrap( multiPolygon, coordinateSystem );
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
      return null;
    }
  }

  // FIXME: very heavy operation!
  private Geometry buildIntersectionArea( )
  {
    final Collection<Polygon> allPolygons = new ArrayList<>();

    for( final HydrotopeUserData hydrotopeBean : m_hydrotopeBeans )
      allPolygons.addAll( Arrays.asList( hydrotopeBean.getGeometry() ) );

    final Polygon[] polygonArray = allPolygons.toArray( new Polygon[allPolygons.size()] );

    final GeometryCollection combinedArea = JTSAdapter.jtsFactory.createGeometryCollection( polygonArray );
    return combinedArea.buffer( 0.0 );
  }
}