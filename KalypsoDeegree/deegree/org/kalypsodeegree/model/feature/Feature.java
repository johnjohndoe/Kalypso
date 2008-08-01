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

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IAdaptable;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;

/**
 * A GML Feature represents a general object.
 * <p>
 * A Feature is adaptable, thus allowing Adapter Factories and/or Subclasses to provide another "view" over a feature
 * object. For instance, an observation-feature can be directly represented as an observation.
 * 
 * @author doemming this class extends the deegree feature interface and implements methods to handle properties that
 *         have maxOccurs > 1
 */
public interface Feature extends DeegreeFeature, IAdaptable
{
  public GMLWorkspace getWorkspace( );

  /**
   * Return the parent of this feature, that is, the feature wich contains this feature as inline feature.
   * 
   * @see #getParentRelation()
   */
  public Feature getParent( );

  /**
   * Returns the {@link IRelationType} where this feature resides inside its parent feature.
   * 
   * @see #getParent()
   */
  public IRelationType getParentRelation( );

  public void setProperty( final IPropertyType propertyType, final Object value );

  public void setProperty( final QName propQName, final Object value );

  /**
   * @deprecated use getPropery(PropertyType)
   */
  @Deprecated
  public Object getProperty( final String propLocalName );

  /**
   * @deprecated
   */
  @Deprecated
  public void setProperty( final String propLocalName, final Object value );

  public Object getProperty( final QName propQName );

  /**
   * intended to be called from GMLWorkspace when root feature is set.
   */
  public void setWorkspace( final GMLWorkspace workspace );

  public void invalidEnvelope( );
}