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
package org.kalypso.model.wspm.tuhh.core.profile.importer.wprof;

import java.awt.geom.Point2D;
import java.math.BigDecimal;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.SortedSet;
import java.util.TreeSet;

import javax.xml.namespace.QName;

import org.kalypso.commons.math.geom.PolyLine;
import org.kalypso.commons.xml.NS;
import org.kalypso.contribs.java.awt.Point2DXComparator;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.IWspmPhenomenonConstants;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfilePointMarker;
import org.kalypso.model.wspm.core.profil.IProfilePointPropertyProvider;
import org.kalypso.model.wspm.core.profil.visitors.ProfileVisitors;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;
import org.kalypso.model.wspm.tuhh.core.wprof.IWProfPoint;
import org.kalypso.observation.phenomenon.DictionaryPhenomenon;
import org.kalypso.observation.phenomenon.IPhenomenon;
import org.kalypso.observation.result.Component;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Spezialcode für Steiermark: Wasserspiegel ins Querprofile einfügen.
 *
 * @author Gernot Belger
 */
public class Waterlevel2DCreator
{
  private static final double WPROF_WATERLEVEL_MINIMAL_NODATA = -999999.999;

  private final Map<String, SortedSet<Point2D>> m_waterlevels = new HashMap<>();

  public void findWaterlevel( final IWProfPoint[] soilPoints )
  {
    for( final IWProfPoint point : soilPoints )
    {
      final BigDecimal distance = point.getDistance();

      final Feature feature = point.getFeature();
      final IFeatureType featureType = feature.getFeatureType();
      final IPropertyType[] properties = featureType.getProperties();
      for( final IPropertyType pt : properties )
      {
        final QName qName = pt.getQName();
        final String propertyName = qName.getLocalPart();
        if( propertyName.startsWith( Messages.getString( "Waterlevel2DCreator_0" ) ) ) //$NON-NLS-1$
        {
          final Object waterLevel = feature.getProperty( qName );
          if( waterLevel instanceof Number )
          {
            addWaterLevel( propertyName, distance, (Number) waterLevel );
          }
        }
      }
    }
  }

  private void addWaterLevel( final String name, final BigDecimal distance, final Number waterLevel )
  {
    final Point2D point2d = new Point2D.Double( distance.doubleValue(), waterLevel.doubleValue() );
    final SortedSet<Point2D> points = getWaterLevel( name );
    points.add( point2d );
  }

  private SortedSet<Point2D> getWaterLevel( final String name )
  {
    if( !m_waterlevels.containsKey( name ) )
    {
      m_waterlevels.put( name, new TreeSet<>( new Point2DXComparator() ) );
    }

    return m_waterlevels.get( name );
  }

  public void insertWaterlevel( final IProfile profile )
  {
    for( final Entry<String, SortedSet<Point2D>> entry : m_waterlevels.entrySet() )
    {
      final String name = entry.getKey();
      final SortedSet<Point2D> points = entry.getValue();
      final Point2D[] polyPoints = points.toArray( new Point2D[points.size()] );
      final PolyLine polyLine = new PolyLine( polyPoints, 0.001 );
      insertWaterlevel( profile, name, polyLine );
    }
  }

  private void insertWaterlevel( final IProfile profile, final String name, final PolyLine polyLine )
  {
    final int widthIndex = profile.indexOfProperty( IWspmConstants.POINT_PROPERTY_BREITE );

    final IComponent waterlevelComponent = createWaterlevelComponent( name );
    profile.addPointProperty( waterlevelComponent );
    final int waterlevelIndex = profile.indexOfProperty( waterlevelComponent );

    final IRecord[] points = profile.getPoints();
    for( final IRecord point : points )
    {
      final Object width = point.getValue( widthIndex );
      final BigDecimal waterlevel = getWaterlevel( width, polyLine );

      if( waterlevel != null && waterlevel.doubleValue() > WPROF_WATERLEVEL_MINIMAL_NODATA )
      {
        point.setValue( waterlevelIndex, waterlevel );
      }
    }
  }

  private IComponent createWaterlevelComponent( final String name )
  {
    final String id = "steiermark.waterlevel." + name; //$NON-NLS-1$
    final String description = Messages.getString( "Waterlevel2DCreator_1" ); //$NON-NLS-1$
    final String unit = "m"; //$NON-NLS-1$

    final IPhenomenon phenomenon = new DictionaryPhenomenon( IWspmPhenomenonConstants.PHENOMENON_WATERLEVEL_2D );
    final QName valueTypeName = new QName( NS.XSD_SCHEMA, "decimal" ); //$NON-NLS-1$
    final String frame = null;
    final Object defaultValue = null;

    return new Component( id, name, description, unit, frame, valueTypeName, defaultValue, phenomenon );
  }

  private BigDecimal getWaterlevel( final Object breite, final PolyLine polyLine )
  {
    if( breite instanceof Number )
    {
      final double distance = ((Number) breite).doubleValue();
      final double waterlevel = polyLine.getYFor( distance, false );
      return new BigDecimal( waterlevel ).setScale( 2, BigDecimal.ROUND_HALF_UP );
    }

    return null;
  }

  public void moveDurchstroemteBereiche( final IProfile profile )
  {
    final int widthIndex = profile.indexOfProperty( IWspmConstants.POINT_PROPERTY_BREITE );

    final IProfilePointMarker[] dbMarker = profile.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );

    double dbMin = Double.MAX_VALUE;
    double dbMax = -Double.MAX_VALUE;

    /* Minimal position is the old one */
    for( final IProfilePointMarker existingMarker : dbMarker )
    {
      profile.removePointMarker( existingMarker );

      final IRecord point = existingMarker.getPoint();
      final Object value = point.getValue( widthIndex );
      if( value instanceof Number )
      {
        final double width = ((Number) value).doubleValue();
        dbMin = Math.min( dbMin, width );
        dbMax = Math.max( dbMax, width );
      }
    }

    /* Enlarge by all points that have a waterlevel */
    for( final SortedSet<Point2D> waterlevel : m_waterlevels.values() )
    {
      for( final Point2D point2d : waterlevel )
      {
        if( point2d.getY() > WPROF_WATERLEVEL_MINIMAL_NODATA )
        {
          final double width = point2d.getX();
          dbMin = Math.min( dbMin, width );
          dbMax = Math.max( dbMax, width );
        }
      }
    }

    /* Enlarge by 20m or at least 2 profile points */
    dbMin -= 20.0;
    dbMax += 20.0;

    createMarker( profile, dbMin, dbMax );
  }

  private void createMarker( final IProfile profile, final double... widths )
  {
    final IProfilePointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( profile.getType() );

    for( final double width : widths )
    {
      final IProfileRecord point = ProfileVisitors.findNearestPoint( profile, width );

      if( point != null )
      {
        final IProfilePointMarker marker = profile.createPointMarker( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE, point );
        final Object defaultValue = provider.getDefaultValue( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );
        marker.setValue( defaultValue );
      }
    }
  }
}
