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

import java.util.Map;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExecutableExtension;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypsodeegree.model.feature.IFeaturePropertyHandler;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * Represents a function-property.
 * 
 * @author Gernot Belger
 */
public abstract class FeaturePropertyFunction implements IFeaturePropertyHandler, IExecutableExtension
{
  /**
   * @see org.eclipse.core.runtime.IExecutableExtension#setInitializationData(org.eclipse.core.runtime.IConfigurationElement,
   *      java.lang.String, java.lang.Object)
   */
  public void setInitializationData( final IConfigurationElement config, final String propertyName, final Object data )
  {
    // read signature from config
  }

  /**
   * Parses a schema fragment of this kind:
   * 
   * <pre>
   *    &lt;kapp:functionProperty kapp:functionId=&quot;org.kalypso.gmlschema.functionProperty.const&quot; kapp:property=&quot;wspm:cacheDate&quot;&gt;
   *       &lt;kapp:parameter&gt;
   *          &lt;kapp:name&gt;-name&lt;/kapp:name&gt;
   *          &lt;kapp:value&gt;-value-&lt;/kapp:value&gt;
   *       &lt;/kapp:parameter&gt;
   *    &lt;/kapp:functionProperty&gt;
   * </pre>
   * 
   * @param properties
   *            the properties of the appinfo TODO: allow init to throw an exception in order to produce better error
   *            messages, if function could not be correctly initialized
   */
  public abstract void init( final Map<String, String> properties );

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#invalidateEnvelope(org.kalypso.gmlschema.property.IPropertyType)
   */
  public boolean invalidateEnvelope( final IPropertyType pt )
  {
    return GeometryUtilities.isGeometry( pt );
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#isFunctionProperty(org.kalypso.gmlschema.property.IPropertyType)
   */
  public boolean isFunctionProperty( final IPropertyType pt )
  {
    return true;
  }
}
