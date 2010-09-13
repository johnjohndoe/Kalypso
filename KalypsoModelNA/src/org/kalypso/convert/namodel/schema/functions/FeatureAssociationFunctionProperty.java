/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.convert.namodel.schema.functions;

import java.util.ArrayList;
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

  /**
   * @see org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction#init(java.util.Map)
   */
  @Override
  public void init( final Map<String, String> properties )
  {
    final String linkNameString = properties.get( "relation" ); //$NON-NLS-1$
    m_linkName = QName.valueOf( linkNameString );
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#getValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  @Override
  public Object getValue( final Feature feature, final IPropertyType pt, final Object currentValue )
  {
    final GMLWorkspace workspace = feature.getWorkspace();
    if( workspace == null )
      return null;

    try
    {
      final GM_Object srcGeo = feature.getDefaultGeometryProperty();
      if( srcGeo == null )
        return null;

      final IRelationType rt = (IRelationType) feature.getFeatureType().getProperty( m_linkName );

      final GetGeomDestinationFeatureVisitor visitor = new GetGeomDestinationFeatureVisitor( workspace, rt, 2 );
      visitor.visit( feature );

      final GM_Object[] destGeo = visitor.getGeometryDestinations();
      final List<GM_Curve> curves = new ArrayList<GM_Curve>();
      for( final GM_Object element : destGeo )
        curves.add( createArrowLineString( srcGeo.getCentroid(), element.getCentroid(), 0.8, 0.01 ) );
      return GeometryFactory.createGM_MultiCurve( curves.toArray( new GM_Curve[curves.size()] ) );
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
      return null;
    }
  }

  private static GM_Curve createArrowLineString( final GM_Point srcP, final GM_Point targetP, final double weightLength, final double weightWidth ) throws GM_Exception
  {
    final double dx = targetP.getX() - srcP.getX();
    final double dy = targetP.getY() - srcP.getY();

    final GM_Position p1 = srcP.getPosition();
    final GM_Position p4 = targetP.getPosition();
    final GM_Position p2 = GeometryFactory.createGM_Position( p1.getX() + weightLength * dx, p1.getY() + weightLength * dy );
    final GM_Position p3 = GeometryFactory.createGM_Position( p2.getX() + weightWidth * dy, p2.getY() - weightWidth * dx );
    final GM_Position p5 = GeometryFactory.createGM_Position( p2.getX() - weightWidth * dy, p2.getY() + weightWidth * dx );

    final GM_Position[] pos = new GM_Position[] { p1, p2, p3, p4, p5, p2 };
    return GeometryFactory.createGM_Curve( pos, srcP.getCoordinateSystem() );
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#setValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  @Override
  public Object setValue( final Feature feature, final IPropertyType pt, final Object valueToSet )
  {
    // Not supported, ignore
    return valueToSet;
  }
}