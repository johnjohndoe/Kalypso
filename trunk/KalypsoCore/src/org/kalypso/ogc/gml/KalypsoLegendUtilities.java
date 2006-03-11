/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.ogc.gml;

import org.kalypso.gmlschema.adapter.IAnnotation;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * @author doemming
 */
public class KalypsoLegendUtilities
{
  private static GM_Object DEFAULT_POINT = GeometryFactory.createGM_Point( 0.5, 0.5, null );

  private static GM_Position[] DEFAULT_LINEPOSITIONS = new GM_Position[] { GeometryFactory.createGM_Position( 0.00, 0.3 ), GeometryFactory.createGM_Position( 0.33, 0.7 ),
      GeometryFactory.createGM_Position( 0.66, 0.3 ), GeometryFactory.createGM_Position( 1.00, 0.7 ), };

  private static GM_Envelope DEFAULT_ENVELOPE = GeometryFactory.createGM_Envelope( 0, 0, 1, 1 );

  private static GM_Object DEFAULT_LINESTRING = null;

  private static GM_Object DEFAULT_POLYGONE = null;
  static
  {
    try
    {
      DEFAULT_LINESTRING = GeometryFactory.createGM_Curve( DEFAULT_LINEPOSITIONS, null );
    }
    catch( GM_Exception e )
    {
      DEFAULT_LINESTRING = null;
      e.printStackTrace();
    }
  }
  static
  {
    try
    {
      DEFAULT_POLYGONE = GeometryFactory.createGM_Surface( DEFAULT_ENVELOPE, null );
    }
    catch( GM_Exception e )
    {
      e.printStackTrace();
    }
  }

  /**
   * update feature with legend properties that can be displayed by a SLD <br>
   * <b>the new featureproperties may violate the schema definition, <br>
   * e.g. all numeric properties will turn to a string (with annotation) </b>
   * 
   * @param legendFeature
   *          feature to update with legend properties
   */
  public static void updatePropertiesForLegend( final Feature legendFeature )
  {
    final IFeatureType featureType = legendFeature.getFeatureType();
    final IPropertyType[] properties = featureType.getProperties();
    for( int i = 0; i < properties.length; i++ )
    {
      final Object legendValue = getLegendValue( properties[i] );
      if( legendValue != null )
        legendFeature.setProperty( properties[i], legendValue );
    }
  }

  private static Object getLegendValue( final IPropertyType ftp )
  {
    if( ftp instanceof IValuePropertyType )
    {
      IValuePropertyType vpt = (IValuePropertyType) ftp;

      if( GeometryUtilities.isPointGeometry( vpt ) || GeometryUtilities.isMultiPointGeometry( vpt ) )
        return DEFAULT_POINT;
      if( GeometryUtilities.isLineStringGeometry( vpt ) || GeometryUtilities.isMultiLineStringGeometry( vpt ) )
        return DEFAULT_LINESTRING;
      if( GeometryUtilities.isPolygonGeometry( vpt ) || GeometryUtilities.isMultiPolygonGeometry( vpt ) )
        return DEFAULT_POLYGONE;
      if( GeometryUtilities.isUndefinedGeometry( vpt ) )
        return DEFAULT_POINT;
      final String type = vpt.getValueClass().getName();
      if( type.startsWith( "java.lang." ) )
      {
        final IAnnotation annotation = AnnotationUtilities.getAnnotation( ftp );
        if( annotation != null )
          return annotation.getLabel();
        return ftp.getQName().getLocalPart();
      }
    }
    return null;
  }
}
