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
package org.kalypsodeegree_impl.model.feature.event;

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.event.IGmlWorkspaceDelta;
import org.kalypsodeegree.model.feature.event.IGmlWorkspaceDeltaVisitor;

/**
 * Implementation of {@link org.kalypsodeegree.model.feature.event.IGmlWorkspaceDelta}.
 * <p>
 * This class is not intended to be instantiated or subclassed by clients.
 * 
 * @author Gernot Belger
 */
class GmlWorkspaceDelta implements IGmlWorkspaceDelta
{
  private final Feature m_feature;

  private final IPropertyType m_property;

  private final int m_kind;

  private List<IGmlWorkspaceDelta> m_children = new ArrayList<IGmlWorkspaceDelta>();

  public GmlWorkspaceDelta( final Feature feature, final IPropertyType property, final int kind )
  {
    m_feature = feature;
    m_property = property;
    m_kind = kind;
  }

  void addChild( final IGmlWorkspaceDelta child )
  {
    m_children.add( child );
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.IGmlWorkspaceDelta#accept(org.kalypsodeegree.model.feature.event.IGmlWorkspaceDeltaVisitor)
   */
  public void accept( final IGmlWorkspaceDeltaVisitor visitor ) throws CoreException
  {
    if( !visitor.visit( this ) )
      return;

    for( final IGmlWorkspaceDelta delta : m_children )
      delta.accept( visitor );
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.IGmlWorkspaceDelta#findMember(javax.xml.namespace.QName)
   */
  public IGmlWorkspaceDelta[] findMember( final QName qname )
  {
    final QNameDeltaVisitor visitor = new QNameDeltaVisitor( qname );
    try
    {
      accept( visitor );
    }
    catch( final CoreException e )
    {
      KalypsoDeegreePlugin.getDefault().getLog().log( e.getStatus() );
      return null;
    }
    return visitor.getResult();
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.IGmlWorkspaceDelta#getAffectedChildren()
   */
  public IGmlWorkspaceDelta[] getAffectedChildren( )
  {
    return m_children.toArray( new IGmlWorkspaceDelta[m_children.size()] );
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.IGmlWorkspaceDelta#getFeature()
   */
  public Feature getFeature( )
  {
    return m_feature;
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.IGmlWorkspaceDelta#getKind()
   */
  public int getKind( )
  {
    return m_kind;
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.IGmlWorkspaceDelta#getProperty()
   */
  public IPropertyType getProperty( )
  {
    return m_property;
  }
}
