/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always.
 * 
 * If you intend to use this software in other ways than in kalypso
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree,
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree_impl.graphics.sld;

import java.awt.Color;
import java.util.ArrayList;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypsodeegree.graphics.sld.FeatureTypeStyle;
import org.kalypsodeegree.graphics.sld.Layer;
import org.kalypsodeegree.graphics.sld.Style;
import org.kalypsodeegree.graphics.sld.StyledLayerDescriptor;
import org.kalypsodeegree.graphics.sld.Symbolizer;
import org.kalypsodeegree.graphics.sld.UserStyle;
import org.kalypsodeegree_impl.filterencoding.PropertyName;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridCoverage;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * This Factory is intended to supply default styles as SLD (Styled Layer Descriptor). When the factory is created it is
 * Initialised with the physical location to save the generated SLDs to. The Factory creates default styles for a
 * specific feature type.The styles are registered as key value pair. The key is namespace plus feature type name.
 * 
 * @author Christoph Küpferle, Technische Universität Hamburg-Harburg
 */
public class DefaultStyleFactory
{
  /** Create a default user style for a given type name. */
  public static UserStyle createUserStyle( final IFeatureType featureType, final String styleName ) throws StyleNotDefinedException
  {
    final ArrayList<Symbolizer> symbolizer = new ArrayList<Symbolizer>();

    if( featureType.getQName().equals( RectifiedGridCoverage.QNAME ) )
      symbolizer.add( StyleFactory.createRasterSymbolizer() );

    final IPropertyType[] properties = featureType.getProperties();
    createSymbolizersForProperties( symbolizer, properties );

    final FeatureTypeStyle featureTypeStyle = StyleFactory.createFeatureTypeStyle( styleName, symbolizer.toArray( new Symbolizer[symbolizer.size()] ) );

    return (UserStyle) StyleFactory.createStyle( styleName, styleName, "empty Abstract", featureTypeStyle );
  }

  private static void createSymbolizersForProperties( final ArrayList<Symbolizer> symbolizer, final IPropertyType[] properties ) throws StyleNotDefinedException
  {
    for( final IPropertyType pt : properties )
    {
      if( pt instanceof IValuePropertyType && GeometryUtilities.isUndefinedGeometry( (IValuePropertyType) pt ) )
      {
        symbolizer.add( StyleFactory.createPointSymbolizer() );
        symbolizer.add( StyleFactory.createLineSymbolizer() );
        symbolizer.add( StyleFactory.createPolygonSymbolizer( StyleFactory.createStroke(), StyleFactory.createFill( Color.GRAY, 0.5d ), new PropertyName( pt.getQName() ) ) );
      }
      else if( GeometryUtilities.isGeometry( pt ) )
      {
        symbolizer.add( createGeometrySymbolizer( (IValuePropertyType) pt ) );
      }
    }
  }

  public static StyledLayerDescriptor createDefaultStyle( final String styleName, final Symbolizer[] symbolizer )
  {
    // called from flood-risk-job
    final FeatureTypeStyle featureTypeStyle = StyleFactory.createFeatureTypeStyle( styleName, symbolizer );
    final Style[] styles = new Style[] { (UserStyle_Impl) StyleFactory.createStyle( styleName, styleName, "no Abstract", featureTypeStyle ) };
    final Layer[] layers = new Layer[] { SLDFactory.createNamedLayer( "deegree style definition", null, styles ) };
    return SLDFactory.createStyledLayerDescriptor( layers, "1.0.0" );
  }

  private static Symbolizer createGeometrySymbolizer( final IValuePropertyType ftp ) throws StyleNotDefinedException
  {
    if( GeometryUtilities.isPointGeometry( ftp ) || GeometryUtilities.isMultiPointGeometry( ftp ) )
      return StyleFactory.createPointSymbolizer( new PropertyName( ftp.getQName() ) );

    if( GeometryUtilities.isLineStringGeometry( ftp ) || GeometryUtilities.isMultiLineStringGeometry( ftp ) )
      return StyleFactory.createLineSymbolizer();

    if( GeometryUtilities.isPolygonGeometry( ftp ) )
      return StyleFactory.createPolygonSymbolizer( StyleFactory.createStroke(), StyleFactory.createFill( Color.GRAY, 0.4d ), new PropertyName( ftp.getQName() ) );

    if( GeometryUtilities.isMultiPolygonGeometry( ftp ) )
      return StyleFactory.createPolygonSymbolizer( StyleFactory.createStroke(), StyleFactory.createFill( Color.GRAY, 0.4d ), new PropertyName( ftp.getQName() ) );

    throw new StyleNotDefinedException( "This geometry type: " + ftp.getQName() + " has no default style available" );
  }
}