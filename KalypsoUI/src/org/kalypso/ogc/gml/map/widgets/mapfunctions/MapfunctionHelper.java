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
package org.kalypso.ogc.gml.map.widgets.mapfunctions;

import java.util.LinkedList;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.command.JMSelector;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapperEnvelopeProvider;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.sort.SplitSort;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * This class consists of functions which are helpfull when handling with features and the map view.
 * 
 * @author Holger Albert
 */
public class MapfunctionHelper
{
  /**
   * Find one feature on a map within a rectangle.
   */
  public static EasyFeatureWrapper[] findFeatureToSelect( final MapPanel mapPanel, final Rectangle rectangle, final EasyFeatureWrapper[] featureToSelectFrom, final int radius )
  {
    if( mapPanel == null )
      return new EasyFeatureWrapper[] {};

    final IMapModell mapModell = mapPanel.getMapModell();
    if( mapModell == null )
      return new EasyFeatureWrapper[] {};

    final GeoTransform transform = mapPanel.getProjection();
    final CS_CoordinateSystem coordinatesSystem = mapModell.getCoordinatesSystem();

    final SplitSort geoIndex = new SplitSort( null, null, null, new EasyFeatureWrapperEnvelopeProvider() );
    for( final EasyFeatureWrapper wrapper : featureToSelectFrom )
      geoIndex.add( wrapper );

    if( (rectangle.width < radius) && (rectangle.height < radius) )
    {
      final double g1x = transform.getSourceX( rectangle.x );
      final double g1y = transform.getSourceY( rectangle.y );

      final double gisRadius = Math.abs( transform.getSourceX( rectangle.x + radius ) - g1x );

      final JMSelector selector = new JMSelector();
      final GM_Point pointSelect = GeometryFactory.createGM_Point( g1x, g1y, coordinatesSystem );
      final EasyFeatureWrapper efw = (EasyFeatureWrapper) selector.selectNearest( pointSelect, gisRadius, geoIndex, false );

      if( efw == null )
        return new EasyFeatureWrapper[] {};
      else
        return new EasyFeatureWrapper[] { efw };
    }
    else
    {
      final GM_Envelope envelope = MapfunctionHelper.rectangleToEnvelope( transform, rectangle );

      final JMSelector selector = new JMSelector();
      final List<Object> features = selector.select( envelope, geoIndex, false );

      return features.toArray( new EasyFeatureWrapper[features.size()] );
    }
  }

  /**
   * Find one feature on a map within a rectangle.<br>
   * <br>
   * REMARK: This function is use for processing shape file workspace (see new QName("namespace", "type"))
   */
  public static Feature[] findFeatureToSelect( final MapPanel mapPanel, final Rectangle rectangle, final Feature[] features, final int radius )
  {
    if( mapPanel == null )
      return new Feature[] {};

    final IMapModell mapModell = mapPanel.getMapModell();
    if( mapModell == null )
      return new Feature[] {};

    final GeoTransform transform = mapPanel.getProjection();
    final CS_CoordinateSystem coordinatesSystem = mapModell.getCoordinatesSystem();

    if( features.length <= 0 )
      return new Feature[] {};

    final Feature oldRootFeature = features[0].getWorkspace().getRootFeature();
    final IFeatureType featureType = oldRootFeature.getFeatureType();
    final Object propertyType = oldRootFeature.getProperty( new QName( "namespace", "type" ) );

    final Feature root = ShapeSerializer.createWorkspaceRootFeature( featureType, propertyType );
    final IRelationType parentRelation = features[0].getParentRelation();

    final FeatureList geoIndex = FeatureFactory.createFeatureList( root, parentRelation );
    for( final Feature feature : features )
      geoIndex.add( feature );

    if( (rectangle.width < radius) && (rectangle.height < radius) )
    {
      final double g1x = transform.getSourceX( rectangle.x );
      final double g1y = transform.getSourceY( rectangle.y );

      final double gisRadius = Math.abs( transform.getSourceX( rectangle.x + radius ) - g1x );

      final JMSelector selector = new JMSelector();
      final GM_Point pointSelect = GeometryFactory.createGM_Point( g1x, g1y, coordinatesSystem );

      final Feature f = (Feature) selector.selectNearest( pointSelect, gisRadius, geoIndex, false );

      if( f == null )
        return new Feature[] {};
      else
        return new Feature[] { f };
    }
    else
    {
      final GM_Envelope envelope = MapfunctionHelper.rectangleToEnvelope( transform, rectangle );

      final JMSelector selector = new JMSelector();
      final List<Object> list = selector.select( envelope, geoIndex, false );
      final List<Feature> myList = new LinkedList<Feature>();
      for( final Object object : list )
      {
        if( !(object instanceof EasyFeatureWrapper) )
          continue;

        myList.add( ((EasyFeatureWrapper) object).getFeature() );
      }

      return myList.toArray( new Feature[] {} );
    }
  }

  public static GM_Envelope rectangleToEnvelope( final GeoTransform transform, final Rectangle rectangle )
  {
    final double g1x = transform.getSourceX( rectangle.x );
    final double g1y = transform.getSourceY( rectangle.y );
    final double g2x = transform.getSourceX( rectangle.x + rectangle.width );
    final double g2y = transform.getSourceY( rectangle.y + rectangle.height );

    final double minX = g1x < g2x ? g1x : g2x;
    final double maxX = g1x > g2x ? g1x : g2x;
    final double minY = g1y < g2y ? g1y : g2y;
    final double maxY = g1y > g2y ? g1y : g2y;

    return GeometryFactory.createGM_Envelope( minX, minY, maxX, maxY );
  }

}
