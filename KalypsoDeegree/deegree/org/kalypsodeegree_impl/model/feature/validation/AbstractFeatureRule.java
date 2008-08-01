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
package org.kalypsodeegree_impl.model.feature.validation;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IConfigurationElement;
import org.kalypsodeegree.model.feature.validation.IFeatureRule;

/**
 * Abstract implementation of {@link IFeatureRule}, handles the {@link org.eclipse.core.runtime.IExecutableExtension}
 * stuff.
 * 
 * @author Gernot Belger
 */
public abstract class AbstractFeatureRule implements IFeatureRule
{
  private String m_id;

  private String m_name;

  private String m_description;

  private QName[] m_qnames;

  public void setInitializationData( final IConfigurationElement config, final String propertyName, final Object data )
  {
    m_id = config.getAttribute( "id" );
    m_name = config.getAttribute( "name" );
    m_description = config.getAttribute( "description" );

    final IConfigurationElement[] children = config.getChildren( "qname" );
    // leave qnames null if no children are available as hint that this rule applies to all features.
    if( children.length > 0 )
    {
      m_qnames = new QName[children.length];
      for( int i = 0; i < children.length; i++ )
      {
        final IConfigurationElement child = children[i];
        final String namespace = child.getAttribute( "namespace" );
        final String localPart = child.getAttribute( "localPart" );
        m_qnames[i] = new QName( namespace, localPart );
      }
    }
  }

  public String getID( )
  {
    return m_id;
  }

  public String getName( )
  {
    return m_name;
  }

  public String getDescription( )
  {
    return m_description;
  }

  public QName[] getQNames( )
  {
    return m_qnames;
  }

}