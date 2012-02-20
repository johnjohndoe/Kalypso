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

import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.virtual.VirtualFunctionValuePropertyType;
import org.kalypso.kalypsosimulationmodel.schema.UrlCatalogRoughness;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class RougnessValuesPropertyFunction extends FeaturePropertyFunction
{
  private final static QName m_groundClsMember = new QName( UrlCatalogRoughness.NS_ROUGHNESS_MODEL, "roughness_GroundLink" ); //$NON-NLS-1$

  private final static QName m_vegetationClsMember = new QName( UrlCatalogRoughness.NS_ROUGHNESS_MODEL, "roughness_VegetationLink" ); //$NON-NLS-1$

  private final static QName m_eddyViscosityClsMember = new QName( UrlCatalogRoughness.NS_ROUGHNESS_MODEL, "roughness_EddyViscosityLink" ); //$NON-NLS-1$

  private final static QName m_groundTypeName = new QName( UrlCatalogRoughness.NS_ROUGHNESS_MODEL, "groundTypeName" ); //$NON-NLS-1$

  private final static QName m_vegetationTypeName = new QName( UrlCatalogRoughness.NS_ROUGHNESS_MODEL, "vegetationTypeName" ); //$NON-NLS-1$

  private final static QName m_eddyViscosityTypeName = new QName( UrlCatalogRoughness.NS_ROUGHNESS_MODEL, "eddyViscosityTypeName" ); //$NON-NLS-1$

  // private final static QName m_colorStyle = new QName( UrlCatalogRoughness.NS_ROUGHNESS_MODEL, "colorStyle" );

  /**
   * @see org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction#init(java.util.Map)
   */
  @Override
  public void init( final Map<String, String> properties )
  {
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#getValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  @Override
  public Object getValue( final Feature feature, final IPropertyType pt, final Object currentValue )
  {
    final QName ptQName = pt.getQName();

    final IPropertyType property = feature.getFeatureType().getProperty( ptQName );
    if( property != null && !(property instanceof VirtualFunctionValuePropertyType) )
      return getValue( feature.getProperty( ptQName ) );

    Feature member = null;

    if( ptQName.equals( m_groundTypeName ) )
    {
      member = FeatureHelper.resolveLink( feature, m_groundClsMember, true );
      if( member == null )
        return ""; //$NON-NLS-1$
      else
        return getValue( member.getProperty( Feature.QN_NAME ) );
    }
    else if( ptQName.equals( m_vegetationTypeName ) )
    {
      member = FeatureHelper.resolveLink( feature, m_vegetationClsMember );
      if( member == null )
        return ""; //$NON-NLS-1$
      else
        return getValue( member.getProperty( Feature.QN_NAME ) );
    }
    else if( ptQName.equals( m_eddyViscosityTypeName ) )
    {
      member = FeatureHelper.resolveLink( feature, m_eddyViscosityClsMember );
      if( member == null )
        return ""; //$NON-NLS-1$
      else
        return getValue( member.getProperty( Feature.QN_NAME ) );
    }

    return getRoughnessFeatureValue( feature, ptQName );
  }

  private Object getRoughnessFeatureValue( final Feature feature, final QName ptQName )
  {
    Object object = feature.getProperty( m_vegetationClsMember );

    if( object != null )
    {
      final Feature member = FeatureHelper.resolveLinkedFeature( feature.getWorkspace(), object );

      if( member != null && member.getFeatureType().getProperty( ptQName ) != null )
        return getValue( member.getProperty( ptQName ) );
    }

    object = feature.getProperty( m_groundClsMember );

    if( object != null )
    {
      final Feature member = FeatureHelper.resolveLinkedFeature( feature.getWorkspace(), object );

      if( member != null && member.getFeatureType().getProperty( ptQName ) != null )
        return getValue( member.getProperty( ptQName ) );
    }

    object = feature.getProperty( m_eddyViscosityClsMember );

    if( object != null )
    {
      final Feature member = FeatureHelper.resolveLinkedFeature( feature.getWorkspace(), object );

      if( member != null && member.getFeatureType().getProperty( ptQName ) != null )
        return getValue( member.getProperty( ptQName ) );
    }

    return null;
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#setValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  @Override
  public Object setValue( final Feature feature, final IPropertyType pt, final Object valueToSet )
  {
    // QName ptQName = pt.getQName();
    // if( !ptQName.equals( m_colorStyle ) )
    // return null;
    // Object existingValue = feature.getProperty( m_colorStyle );
    // if( !valueToSet.equals( existingValue ) )
    // {
    // // TODO: modify roughness sld!!!
    // IDocumentReference[] documentReferences = feature.getParentRelation().getDocumentReferences();
    // String reference = documentReferences[0].getReference();
    //
    // // final URL styleURL = feature.getWorkspace().getContext();
    // // System.out.println(styleURL);
    // System.out.println( "Feature: " + feature.getProperty( m_name ) );
    // System.out.println( "Old color: " + existingValue );
    // System.out.println( "New color: " + valueToSet.toString() );
    // }
    // return valueToSet;
    return null;
  }

  @SuppressWarnings("unchecked")
  private Object getValue( final Object object )
  {
    if( object == null )
      return null;
    if( object instanceof List )
      return ((List<Object>) object).get( 0 );
    else
      return object;
  }
}
