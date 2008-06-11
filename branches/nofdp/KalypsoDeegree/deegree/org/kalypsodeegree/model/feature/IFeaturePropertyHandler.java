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
package org.kalypsodeegree.model.feature;

import org.kalypso.gmlschema.property.IPropertyType;

/**
 * A handler to tweak the getting/setting of properties on features.
 * <p>
 * Used to implement 'function-style' properties such as 'boundedBy'.
 * </p>
 * 
 * @author Gernot Belger
 */
public interface IFeaturePropertyHandler
{
  /**
   * Called before the value is really set into a feature.
   * <p>
   * Although the feature is an argument to this function, no 'set' method should be called in order to prevent
   * recursion.
   * </p>
   * 
   * @param valueToSet
   *            The value with which {@link Feature#setProperty(IPropertyType, Object)} was called.
   * @return The value which should really be stored in the feature.
   */
  public Object setValue( final Feature feature, final IPropertyType pt, final Object valueToSet );

  /**
   * Called before the value is really returned from a feature.
   * <p>
   * Although the feature is an argument to this function, no 'get' method should be called in order to prevent
   * recursion.
   * </p>
   * 
   * @param currentValue
   *            The value which is currently stored in the feature.
   * @return The value which should really be returned from {@link Feature#getProperty(QName)}.
   */
  public Object getValue( final Feature feature, final IPropertyType pt, final Object currentValue );

  /** Return if the envelope of the feature shall be invalidated after settings this property. */
  public boolean invalidateEnvelope( final IPropertyType pt );

  /**
   * REMARK: only for internal use. Is used to determine if a property is a function property. Function properties do
   * not get transformed during load.<br/> This is needed in order to prohibit loading of xlinked-workspaces during
   * gml-loading, in order to avoid dead-locks.
   */
  public boolean isFunctionProperty( IPropertyType pt );
}
