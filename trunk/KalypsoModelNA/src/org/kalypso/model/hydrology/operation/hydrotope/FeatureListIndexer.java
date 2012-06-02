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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.model.hydrology.internal.ModelNA;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.kalypsodeegree_impl.model.sort.SpatialIndexExt;
import org.kalypsodeegree_impl.model.sort.SplitSortSpatialIndex;

import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.util.PolygonExtracter;

/**
 * Holds and build indices for hydrotopes intersection
 * 
 * @author Gernot Belger
 */
class FeatureListIndexer implements ICoreRunnableWithProgress
{
  private final Map<FeatureList, String> m_sourceLayers = new LinkedHashMap<FeatureList, String>();

  private SpatialIndexExt[] m_indices;

  private final String m_logLabel;

  public FeatureListIndexer( final String logLabel )
  {
    m_logLabel = logLabel;
  }

  public void addFeatureList( final FeatureList list, final String label )
  {
    m_sourceLayers.put( list, label );
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    final IStatusCollector log = new StatusCollector( ModelNA.PLUGIN_ID );

    final String taskName = Messages.getString( "org.kalypso.convert.namodel.FeatureListGeometryIntersector.1" ); //$NON-NLS-1$

    final SubMonitor progress = SubMonitor.convert( monitor, taskName, 100 );

    final List<SpatialIndexExt> indices = new ArrayList<>();

    int count = 0;
    for( final Entry<FeatureList, String> entry : m_sourceLayers.entrySet() )
    {
      final String label = entry.getValue();

      progress.subTask( String.format( "%d of %d - %s", count, m_sourceLayers.size(), label ) );

      final FeatureList featureList = entry.getKey();

      final SpatialIndexExt index = buildIndex( featureList, label, log );
      indices.add( index );

      ProgressUtilities.worked( progress, 1 );

      count++;
    }

    for( int i = 0; i < m_sourceLayers.size(); i++ )
    {
    }

    // sort by number of polygons
    m_indices = indices.toArray( new SpatialIndexExt[indices.size()] );
    Arrays.sort( m_indices, new SpatialIndexSizeComparator() );
    ArrayUtils.reverse( m_indices );

    return log.asMultiStatus( m_logLabel );
  }

  static SpatialIndexExt buildIndex( final FeatureList featureList, final String label, final IStatusCollector log )
  {
    final Envelope boundingBox = JTSAdapter.export( featureList.getBoundingBox() );

    final SpatialIndexExt index = new SplitSortSpatialIndex( boundingBox );
    index.setUserData( label );

    for( final Object element : featureList )
    {
      final Feature feature = (Feature) element;
      final GM_Object gmObj = feature.getDefaultGeometryPropertyValue();

      try
      {
        final Geometry export = JTSAdapter.export( gmObj );

        @SuppressWarnings("unchecked")
        final List<Polygon> polygons = PolygonExtracter.getPolygons( export );
        for( final Polygon polygon : polygons )
        {
          polygon.setSRID( export.getSRID() );
          polygon.setUserData( new HydrotopeBean( polygon, feature ) );

          // FIXME: only add valid polygons:
          // - do not overlap with existing ones
          // - are topological correct

          index.insert( polygon.getEnvelopeInternal(), polygon );
        }
      }
      catch( final Exception e )
      {
        log.add( IStatus.WARNING, Messages.getString( "FeatureListGeometryIntersector.0", feature.getName() ), e ); //$NON-NLS-1$
      }
    }
    return index;
  }

  public SpatialIndexExt[] getIndices( )
  {
    return m_indices;
  }
}