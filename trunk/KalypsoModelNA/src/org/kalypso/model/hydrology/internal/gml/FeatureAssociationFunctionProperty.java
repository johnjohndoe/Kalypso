/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraï¿½e 22
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
package org.kalypso.model.hydrology.internal.gml;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * This function property creates 'arrows' from links beetween features.
 *
 * @author Andreas von Dömming
 * @author Gernot Belger
 */
public class FeatureAssociationFunctionProperty extends FeaturePropertyFunction
{
  private QName m_linkName;

  private QName m_link2Name;

  @Override
  public void init( final Map<String, String> properties )
  {
    final String linkNameString = properties.get( "relation" ); //$NON-NLS-1$
    m_linkName = QName.valueOf( linkNameString );

    final String link2NameString = properties.get( "relation2" ); //$NON-NLS-1$
    m_link2Name = link2NameString == null ? null : QName.valueOf( link2NameString );
  }

  @Override
  public Object getValue( final Feature feature, final IPropertyType pt, final Object currentValue )
  {
    final GMLWorkspace workspace = feature.getWorkspace();
    if( workspace == null )
      return null;

    try
    {
      final GM_Object srcGeo = feature.getDefaultGeometryPropertyValue();
      if( srcGeo == null )
        return null;

      final Feature[] relatedFeaturs = findRelatedFeatures( feature );

      final List<GM_Curve> curves = new ArrayList<>();
      for( final Feature relatedFeature : relatedFeaturs )
      {
        final GM_Object targetGeom = relatedFeature.getDefaultGeometryPropertyValue();
        if( targetGeom != null )
        {
          final GM_Point targetCenter = targetGeom.getCentroid();
          curves.add( createArrowLineString( srcGeo.getCentroid(), targetCenter ) );
        }
      }
      return GeometryFactory.createGM_MultiCurve( curves.toArray( new GM_Curve[curves.size()] ) );
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
      return null;
    }
  }

  private Feature[] findRelatedFeatures( final Feature feature )
  {
    final GMLWorkspace workspace = feature.getWorkspace();
    final IRelationType link1 = (IRelationType) feature.getFeatureType().getProperty( m_linkName );
    final Feature[] targetFeatures = workspace.resolveLinks( feature, link1 );
    if( m_link2Name == null )
      return targetFeatures;

    /* Heavy relation indirect into 2nd feature */
    final Collection<Feature> result = new ArrayList<>();
    for( final Feature bodyFeature : targetFeatures )
    {
      /* Could be another workspace */
      final GMLWorkspace targetWorkspace = feature.getWorkspace();
      final IRelationType link2 = (IRelationType) bodyFeature.getFeatureType().getProperty( m_link2Name );
      if( link2 != null )
      {
        final Feature[] target2Features = targetWorkspace.resolveLinks( bodyFeature, link2 );
        result.addAll( Arrays.asList( target2Features ) );
      }
    }
    return result.toArray( new Feature[result.size()] );
  }

  private static GM_Curve createArrowLineString( final GM_Point srcP, final GM_Point targetP ) throws GM_Exception
  {
    final GM_Position p1 = srcP.getPosition();
    final GM_Position p4 = targetP.getPosition();
    final GM_Position[] pos = new GM_Position[] { p1, p4 };
    return GeometryFactory.createGM_Curve( pos, srcP.getCoordinateSystem() );
  }

  @Override
  public Object setValue( final Feature feature, final IPropertyType pt, final Object valueToSet )
  {
    // Not supported, ignore
    return valueToSet;
  }
}