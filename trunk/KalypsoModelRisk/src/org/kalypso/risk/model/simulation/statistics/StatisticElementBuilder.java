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
package org.kalypso.risk.model.simulation.statistics;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.risk.model.schema.binding.ILanduseClass;
import org.kalypso.risk.model.schema.binding.ILandusePolygon;
import org.kalypso.risk.model.schema.binding.ILandusePolygonCollection;
import org.kalypso.risk.model.schema.binding.IRasterizationControlModel;
import org.kalypso.shape.ShapeFile;
import org.kalypso.shape.dbf.DBaseException;
import org.kalypso.shape.geometry.ISHPGeometry;
import org.kalypso.shape.tools.SHP2JTS;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Polygon;
import com.vividsolutions.jts.geom.util.PolygonExtracter;

/**
 * @author Gernot Belger
 */
public class StatisticElementBuilder
{
  private final Map<StatisticItemKey, RiskStatisticItem> m_items = new LinkedHashMap<>();

  private final IRasterizationControlModel m_controlModel;

  public StatisticElementBuilder( final IRasterizationControlModel controlModel )
  {
    m_controlModel = controlModel;
  }

  public void addElements( final ILandusePolygonCollection landusePolygons, final ShapeFile shape, final String shapeNameField, final String shapeSRS, final IProgressMonitor monitor ) throws GM_Exception, IOException, DBaseException
  {
    final SubMonitor progress = SubMonitor.convert( monitor );
    progress.beginTask( "Intersect statistic areas", 30 );

    /* Load shape if given */
    progress.subTask( "loading shape..." );
    final StatisticGroup[] groups = readShape( shape, shapeNameField, shapeSRS );
    ProgressUtilities.worked( progress, 50 );

    /* Intersect groups with landuses and build all areas */
    final IFeatureBindingCollection<ILandusePolygon> landusePolygonCollection = landusePolygons.getLandusePolygonCollection();
    progress.subTask( "adding evaluation areas..." );
    final int progressCount = landusePolygonCollection.size() * groups.length;
    progress.setWorkRemaining( progressCount );

    for( final ILandusePolygon landusePolygon : landusePolygonCollection )
    {
      final GM_Surface< ? > landuseSurface = landusePolygon.getGeometry();
      final Polygon landuseArea = (Polygon) JTSAdapter.export( landuseSurface );

      final ILanduseClass landuseClass = landusePolygon.getLanduseClass( m_controlModel );

      for( final StatisticGroup group : groups )
      {
        final StatisticItemKey key = new StatisticItemKey( landuseClass.getName(), group.getName() );
        final RiskStatisticItem item = getItem( key );

        final Polygon groupArea = group.getArea();

        final Polygon[] keyAreas = intersectLanduseWithGroup( landuseArea, groupArea );
        for( final Polygon polygon : keyAreas )
          item.add( polygon );
      }

      ProgressUtilities.worked( monitor, groups.length );
    }
  }

  private Polygon[] intersectLanduseWithGroup( final Polygon landuseArea, final Polygon groupArea )
  {
    if( groupArea == null )
      return new Polygon[] { landuseArea };

    final Geometry intersection = landuseArea.intersection( groupArea );
    final List< ? > polygons = PolygonExtracter.getPolygons( intersection );
    return polygons.toArray( new Polygon[polygons.size()] );
  }

  private StatisticGroup[] readShape( final ShapeFile shape, final String shapeNameField, final String shapeSRS ) throws IOException, DBaseException
  {
    final int shapeSRID = JTSAdapter.toSrid( shapeSRS );
    final SHP2JTS shp2jts = new SHP2JTS( new GeometryFactory() );

    final Collection<StatisticGroup> groups = new ArrayList<>();

    if( shape != null )
    {
      final int numRecords = shape.getNumRecords();
      for( int i = 0; i < numRecords; i++ )
      {
        final ISHPGeometry shapeArea = shape.getShape( i );

        final String name = (String) shape.getRowValue( i, shapeNameField );

        final Geometry transformedShapeArea = shp2jts.transform( shapeSRID, shapeArea );
        final List<Polygon> polygons = PolygonExtracter.getPolygons( transformedShapeArea );
        // TODO: transform to kalypso SRS
        for( final Polygon polygon : polygons )
        {
          final StatisticGroup group = new StatisticGroup( name, polygon );
          groups.add( group );
        }
      }
    }

    /* Always add null group, serves as total for landuses */
    groups.add( new StatisticGroup( StringUtils.EMPTY, null ) );

    return groups.toArray( new StatisticGroup[groups.size()] );
  }

  private RiskStatisticItem getItem( final StatisticItemKey key )
  {
    if( !m_items.containsKey( key ) )
      m_items.put( key, new RiskStatisticItem( key ) );

    return m_items.get( key );
  }

  public RiskStatisticItem[] getItems( )
  {
    return m_items.values().toArray( new RiskStatisticItem[m_items.size()] );
  }
}