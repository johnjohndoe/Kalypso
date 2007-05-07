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
package org.kalypso.ui.wizards.imports.roughness;

import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.kalypsosimulationmodel.schema.UrlCatalogRoughness;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class RougnessValuesPropertyFunction extends FeaturePropertyFunction
{
  private final static QName m_groundClsMember = new QName( UrlCatalogRoughness.NS_ROUGHNESS_MODEL, "roughness_GroundLink" );

  private final static QName m_vegetationClsMember = new QName( UrlCatalogRoughness.NS_ROUGHNESS_MODEL, "roughness_VegetationLink" );

  private final static QName m_name = new QName( NS.GML3, "name" );

  private final static QName m_groundTypeName = new QName( UrlCatalogRoughness.NS_ROUGHNESS_MODEL, "groundTypeName" );

  private final static QName m_vegetationTypeName = new QName( UrlCatalogRoughness.NS_ROUGHNESS_MODEL, "vegetationTypeName" );

//  private final static QName m_colorStyle = new QName( UrlCatalogRoughness.NS_ROUGHNESS_MODEL, "colorStyle" );

  /**
   * @see org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction#init(java.util.Map)
   */
  @Override
  public void init( Map<String, String> properties )
  {
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#getValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  @SuppressWarnings("unchecked")
  public Object getValue( Feature feature, IPropertyType pt, Object currentValue )
  {
    QName ptQName = pt.getQName();
    Feature member = null;
//    if( ptQName.equals( m_colorStyle ) )
//    {
//      return currentValue;
//    }
//    else 
      if( ptQName.equals( m_groundTypeName ) )
    {
      member = (Feature) feature.getProperty( m_groundClsMember );
      if( member == null )
        return "";
      else
        return getValue( member.getProperty( m_name ) );
    }
    else if( ptQName.equals( m_vegetationTypeName ) )
    {
      member = (Feature) feature.getProperty( m_vegetationClsMember );
      if( member == null )
        return "";
      else
        return getValue( member.getProperty( m_name ) );
    }
    member = (Feature) feature.getProperty( m_vegetationClsMember );
    try
    {
     // TODO: here very often NPE fly... which is slow
      return getValue( member.getProperty( ptQName ) );
    }
    catch( Exception e1 )
    {
      member = (Feature) feature.getProperty( m_groundClsMember );
      try
      {
        return getValue( member.getProperty( ptQName ) );
      }
      catch( Exception e2 )
      {
        return null;
      }
    }
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#setValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  public Object setValue( Feature feature, IPropertyType pt, Object valueToSet )
  {
//    QName ptQName = pt.getQName();
//    if( !ptQName.equals( m_colorStyle ) )
//      return null;
//    Object existingValue = feature.getProperty( m_colorStyle );
//    if( !valueToSet.equals( existingValue ) )
//    {
//      // TODO: modify roughness sld!!!
//      IDocumentReference[] documentReferences = feature.getParentRelation().getDocumentReferences();
//      String reference = documentReferences[0].getReference();
//      
////      final URL styleURL = feature.getWorkspace().getContext();
////      System.out.println(styleURL);
//      System.out.println( "Feature: " + feature.getProperty( m_name ) );
//      System.out.println( "Old color: " + existingValue );
//      System.out.println( "New color: " + valueToSet.toString() );
//    }
//    return valueToSet;
    return null;
  }

  @SuppressWarnings("unchecked")
  private Object getValue( Object object )
  {
    if( object == null )
      return "";
    if( object instanceof List )
      return ((List<Object>) object).get( 0 );
    else
      return object;
  }
}
