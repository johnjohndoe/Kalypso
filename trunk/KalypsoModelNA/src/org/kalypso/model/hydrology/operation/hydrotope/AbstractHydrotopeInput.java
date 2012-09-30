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

import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.kalypsodeegree_impl.model.sort.JSISpatialIndex;
import org.kalypsodeegree_impl.model.sort.SpatialIndexExt;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.util.PolygonExtracter;

/**
 * @author Gernot Belger
 */
abstract class AbstractHydrotopeInput<T extends Feature> implements IHydrotopeInput
{
  protected static final String STR_ATTRIBUTES = "Attributes"; //$NON-NLS-1$

  private SpatialIndexExt m_index = null;

  private final IFeatureBindingCollection<T> m_features;

  public AbstractHydrotopeInput( final IFeatureBindingCollection<T> features )
  {
    m_features = features;
  }

  protected IFeatureBindingCollection<T> getFeatures( )
  {
    return m_features;
  }

  protected Polygon[] buildInverseMask( )
  {
    return new Polygon[0];
  }

  @Override
  public void buildIndex( final IStatusCollector log )
  {
    m_index = buildIndex( m_features, log );

    int count = m_index.size();

    /*
     * Special case (mainly used for overlay): allow index to insert an 'inverse' element that covers the rest of the
     * area.
     */
    final Polygon[] inverseMask = buildInverseMask();
    for( final Polygon mask : inverseMask )
    {
      // using 'null' feature here; inverse elements never have any attributes
      mask.setUserData( new HydrotopeUserData( count++, mask, null ) );
      m_index.insert( mask.getEnvelopeInternal(), mask );
    }
  }

  static <F extends Feature> SpatialIndexExt buildIndex( final IFeatureBindingCollection<F> features, final IStatusCollector log )
  {
// final Envelope boundingBox = JTSAdapter.export( features.getBoundingBox() );

    final SpatialIndexExt index = new JSISpatialIndex();
// new SplitSortSpatialIndex( boundingBox );

    int count = 0;

    for( final Feature feature : features )
    {
      final GM_Object gmObj = feature.getDefaultGeometryPropertyValue();

      try
      {
        final Geometry export = JTSAdapter.export( gmObj );

        final List<Polygon> polygons = PolygonExtracter.getPolygons( export );
        for( final Polygon polygon : polygons )
        {
          polygon.setSRID( export.getSRID() );
          polygon.setUserData( new HydrotopeUserData( count, polygon, feature ) );

          // FIXME: only add valid polygons:
          // - do not overlap with existing ones
          // - are topological correct

          index.insert( polygon.getEnvelopeInternal(), polygon );

          count++;
        }
      }
      catch( final Exception e )
      {
        log.add( IStatus.WARNING, Messages.getString( "FeatureListGeometryIntersector.0", feature.getName() ), e ); //$NON-NLS-1$
      }
    }

    return index;
  }

  @Override
  public SpatialIndexExt getIndex( )
  {
    return m_index;
  }

  protected String formatMessage( final String message, final Feature element )
  {
    return String.format( Messages.getString("AbstractHydrotopeInput.1"), element.getName(), message ); //$NON-NLS-1$
  }
}
