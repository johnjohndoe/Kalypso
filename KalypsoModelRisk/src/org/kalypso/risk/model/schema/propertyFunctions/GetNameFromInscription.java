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
package org.kalypso.risk.model.schema.propertyFunctions;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;

/**
 * This function property returns a string from an inscription according to the active language (defaults to en).
 * <p>
 * Parameters:
 * <ul>
 * <li>shallOverwrite (Boolean): shall the string be translated?</li>
 * </ul>
 * </p>
 * 
 * @author thuel2
 */
public class GetNameFromInscription extends FeaturePropertyFunction
{
  private String m_langShort = "en"; // language of application //$NON-NLS-1$

  private String m_langLong = "en"; // language of application //$NON-NLS-1$

  private boolean m_shallOverwrite = false;

  private static final String F_STATE_INSCRIPTION_MEMBER = "inscriptionMember"; //$NON-NLS-1$

  private static final String P_STATE_INSCRIPTION_LABEL = "label"; //$NON-NLS-1$

  private static final String P_STATE_INSCRIPTION_LANGUAGE = "language"; //$NON-NLS-1$

  private static final String NS_COMMON = "http://www.tu-harburg.de/wb/kalypso/risk/schemata/common"; //$NON-NLS-1$

  private static final QName QN_STATE_INSCRIPTION_MEMBER = new QName( NS_COMMON, F_STATE_INSCRIPTION_MEMBER );

  private static final QName QN_STATE_INSCRIPTION_LANGUAGE = new QName( NS_COMMON, P_STATE_INSCRIPTION_LANGUAGE );

  private static final QName QN_STATE_INSCRIPTION_LABEL = new QName( NS_COMMON, P_STATE_INSCRIPTION_LABEL );

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#getValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  @Override
  public Object getValue( final Feature feature, final IPropertyType pt, final Object currentValue )
  {

    final IFeatureType featureType = feature.getFeatureType();
    final IPropertyType inscriptionProperty = featureType.getProperty( QN_STATE_INSCRIPTION_MEMBER );
    if( inscriptionProperty == null )
      return currentValue;

    final List< ? > inscriptionList = (List< ? >) feature.getProperty( inscriptionProperty );
    if( inscriptionList == null )
      return currentValue;

    if( inscriptionList.size() < 1 )
      return currentValue;

    Feature f = null;
    final HashMap<String, String> intNames = new HashMap<String, String>();
    for( final Object object : inscriptionList )
    {
      if( object instanceof Feature )
        f = (Feature) object;
      else
        // this branch should never be reached according to the schema file
        f = feature.getWorkspace().getFeature( (String) object );

      if( f != null )
      {
        final IFeatureType inscriptionFtp = f.getFeatureType();
        final IPropertyType langProp = inscriptionFtp.getProperty( QN_STATE_INSCRIPTION_LANGUAGE );
        final Object langFeat = f.getProperty( langProp );
        if( langFeat != null )
        {
          final String languageUri = ((XLinkedFeature_Impl) langFeat).getUri();
          final String languageHref = ((XLinkedFeature_Impl) langFeat).getHref();
          final String lang = languageHref.replaceAll( languageUri + "#", "" ); //$NON-NLS-1$ //$NON-NLS-2$
          final IPropertyType labelProp = inscriptionFtp.getProperty( QN_STATE_INSCRIPTION_LABEL );
          final Object label = f.getProperty( labelProp );
          if( label != null )
          {
            final String newMapName = label.toString();
            intNames.put( lang, newMapName );
          }
        }
      }
    }
    if( currentValue == null || (intNames.containsValue( currentValue ) && m_shallOverwrite) )
    {
      if( intNames.containsKey( m_langLong ) )
        return intNames.get( m_langLong );
      else if (intNames.containsKey( m_langShort ))
        return intNames.get( m_langShort );
      else
        return null;
    }

    return currentValue;
  }

  /**
   * @see org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction#init(java.util.Map)
   */
  @Override
  public void init( final Map<String, String> properties )
  {

    m_shallOverwrite = Boolean.valueOf( properties.get( "shallOverwrite" ) ); //$NON-NLS-1$
    // get language from application
    // m_language = System.getProperty( "osgi.nl.user", "en" );
    m_langLong = System.getProperty( "org.osgi.framework.language", "en" ); //$NON-NLS-1$ //$NON-NLS-2$

    final String[] langArray = m_langLong.split( "_" );//$NON-NLS-1$ 
    m_langShort = langArray[0];
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#setValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  @Override
  public Object setValue( final Feature feature, final IPropertyType pt, final Object valueToSet )
  {

    return valueToSet;
  }

}
