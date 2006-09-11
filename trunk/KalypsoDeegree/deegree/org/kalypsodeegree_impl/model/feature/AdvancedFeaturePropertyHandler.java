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
package org.kalypsodeegree_impl.model.feature;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

import javax.xml.namespace.QName;

import org.apache.xmlbeans.XmlCursor;
import org.apache.xmlbeans.XmlObject;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.xml.NS;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.feature.FeatureContentType;
import org.kalypso.gmlschema.feature.IDetailedFeatureType;
import org.kalypso.gmlschema.feature.IFeatureContentType;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypsodeegree.KalypsoDeegreeExtensions;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeaturePropertyHandler;

/**
 * This implementation does the following to handle property sets/gets:
 * <ul>
 * <li>check if the value fits to the property type</li>
 * <li>delegate to a application-info-handler if present</li>
 * <li>else, use {@link org.kalypsodeegree_impl.model.feature.DefaultFeaturePropertyHandler}</li>
 * </ul>
 * 
 * @author Gernot Belger
 */
public class AdvancedFeaturePropertyHandler implements IFeaturePropertyHandler
{
  private final static IFeaturePropertyHandler DEFAULT_HANDLER = new DefaultFeaturePropertyHandler();

  private final static IFeaturePropertyHandler CHECK_HANDLER = new CheckFeaturePropertyHandler();

  private final Map<IPropertyType, IFeaturePropertyHandler> m_handlers = new HashMap<IPropertyType, IFeaturePropertyHandler>();

  public static final QName QNAME_FUNCTION = new QName( NS.KALYPSO_APPINFO, "functionId" );

  public static final QName QNAME_PROPERTY = new QName( NS.KALYPSO_APPINFO, "property" );

  public static final QName QNAME_NAME = new QName( NS.KALYPSO_APPINFO, "name" );

  public static final QName QNAME_VALUE = new QName( NS.KALYPSO_APPINFO, "value" );

  public AdvancedFeaturePropertyHandler( final IFeatureType featureType )
  {
    if( featureType instanceof IDetailedFeatureType )
    {
      final IDetailedFeatureType dft = (IDetailedFeatureType) featureType;

      final IFeatureContentType featureContentType = (FeatureContentType) dft.getFeatureContentType();
      final XmlObject[] funcProps = featureContentType.collectFunctionProperties();
      for( final XmlObject funcProp : funcProps )
      {
        try
        {
          final XmlCursor funcCursor = funcProp.newCursor();

          final String functionId = funcCursor.getAttributeText( QNAME_FUNCTION );
          final String property = funcCursor.getAttributeText( QNAME_PROPERTY );

          final String[] qnameParts = property.split( ":" );
          final String propertyNamespace = funcCursor.namespaceForPrefix( qnameParts[0] );
          final String propertyLocalPart = qnameParts[1];

          final QName propertyQName = new QName( propertyNamespace, propertyLocalPart );

          final IPropertyType pt = featureType.getProperty( propertyQName );

          final XmlObject[] parameters = funcProp.selectPath( "declare namespace xs='" + NS.XSD_SCHEMA + "' " + "declare namespace kapp" + "='" + NS.KALYPSO_APPINFO + "' ./kapp:parameter" );
          final Map<String, String> properties = parseParameters( parameters );

          final FeaturePropertyFunction propertyFunction = KalypsoDeegreeExtensions.createPropertyFunction( functionId, properties );
          m_handlers.put( pt, propertyFunction );
        }
        catch( final Exception e )
        {
          final IStatus status = StatusUtilities.statusFromThrowable( e );
          KalypsoDeegreePlugin.getDefault().getLog().log( status );
        }

      }
    }
  }

  private Map<String, String> parseParameters( final XmlObject[] parameters )
  {
    /* IMPORTENT: Use linked hash map in order to preserve parameter order. */
    final Map<String, String> properties = new LinkedHashMap<String, String>();

    for( final XmlObject parameter : parameters )
    {
      final XmlObject[] names = parameter.selectChildren( QNAME_NAME );
      final XmlObject[] values = parameter.selectChildren( QNAME_VALUE );

      final String name = names.length == 0 ? null : names[0].newCursor().getTextValue();
      final String value = values.length == 0 ? null : values[0].newCursor().getTextValue();

      if( name != null && value != null )
        properties.put( name, value );
    }

    return properties;
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#setValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  public Object setValue( final Feature feature, final IPropertyType pt, final Object valueToSet )
  {
    final Object checkedValue = CHECK_HANDLER.setValue( feature, pt, valueToSet );

    final IFeaturePropertyHandler handler = m_handlers.get( pt );
    if( handler != null )
      return handler.setValue( feature, pt, checkedValue );

    return DEFAULT_HANDLER.setValue( feature, pt, checkedValue );
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#getValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  public Object getValue( final Feature feature, final IPropertyType pt, final Object currentValue )
  {
    final Object checkedValue = CHECK_HANDLER.getValue( feature, pt, currentValue );

    final IFeaturePropertyHandler handler = m_handlers.get( pt );
    if( handler != null )
      return handler.getValue( feature, pt, checkedValue );

    return DEFAULT_HANDLER.getValue( feature, pt, checkedValue );
  }

}
