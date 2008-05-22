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
package org.kalypsodeegree_impl.model.feature;

import java.util.HashMap;
import java.util.Map;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.virtual.IFunctionPropertyType;
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

  private final Map<QName, IFeaturePropertyHandler> m_handlers = new HashMap<QName, IFeaturePropertyHandler>();

  public AdvancedFeaturePropertyHandler( final IFeatureType featureType )
  {
    // We simply check, if any property is a virtual property with attached functionId
    // For each of those we create a special handler
    final IPropertyType[] properties = featureType.getProperties();
    for( final IPropertyType propertyType : properties )
    {
      if( propertyType instanceof IFunctionPropertyType )
      {
        final QName qname = propertyType.getQName();
        final IFunctionPropertyType virtualPt = (IFunctionPropertyType) propertyType;
        final String functionId = virtualPt.getFunctionId();
        final Map<String, String> functionProperties = virtualPt.getFunctionProperties();

        try
        {
          final FeaturePropertyFunction propertyFunction = KalypsoDeegreeExtensions.createPropertyFunction( functionId, functionProperties );
          m_handlers.put( qname, propertyFunction );
        }
        catch( final CoreException e )
        {
          final String msg = String.format( "Failed to create property function with id '%s' for property '%s'", functionId, qname );
          final IStatus status = StatusUtilities.createStatus( IStatus.ERROR, msg, e );

          // Simply log errors to the error log, but continue
          KalypsoDeegreePlugin.getDefault().getLog().log( status );
        }
      }
    }
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#setValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  public Object setValue( final Feature feature, final IPropertyType pt, final Object valueToSet )
  {
    final Object checkedValue = CHECK_HANDLER.setValue( feature, pt, valueToSet );

    final IFeaturePropertyHandler handler = m_handlers.get( pt.getQName() );
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

    final IFeaturePropertyHandler handler = m_handlers.get( pt.getQName() );
    if( handler != null )
      return handler.getValue( feature, pt, checkedValue );

    return DEFAULT_HANDLER.getValue( feature, pt, checkedValue );
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#invalidateEnvelope(org.kalypso.gmlschema.property.IPropertyType)
   */
  public boolean invalidateEnvelope( final IPropertyType pt )
  {
    final IFeaturePropertyHandler handler = m_handlers.get( pt.getQName() );
    if( handler != null )
      return handler.invalidateEnvelope( pt );

    return DEFAULT_HANDLER.invalidateEnvelope( pt );
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#isFunctionProperty(org.kalypso.gmlschema.property.IPropertyType)
   */
  public boolean isFunctionProperty( final IPropertyType pt )
  {
    final IFeaturePropertyHandler handler = m_handlers.get( pt.getQName() );
    if( handler != null )
      return handler.isFunctionProperty( pt );

    return DEFAULT_HANDLER.isFunctionProperty( pt );
  }

}
