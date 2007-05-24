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

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExecutableExtension;
import org.kalypsodeegree.model.feature.IGmlWorkspaceListener;

/**
 * Abstract implementation of {@link IGmlWorkspaceListener} encapsulating the common code.
 * <p>
 * Always derive from this class in order to implement a listener.
 * 
 * @author Gernot Belger
 */
public abstract class GmlWorkspaceListener implements IGmlWorkspaceListener, IExecutableExtension
{
  private QName[] m_qnames;

  /**
   * @see org.eclipse.core.runtime.IExecutableExtension#setInitializationData(org.eclipse.core.runtime.IConfigurationElement,
   *      java.lang.String, java.lang.Object)
   */
  public void setInitializationData( final IConfigurationElement config, final String propertyName, final Object data )
  {
    final IConfigurationElement[] children = config.getChildren( "qname" );

    final List<QName> qnames = new ArrayList<QName>( children.length );
    for( final IConfigurationElement child : children )
    {
      final String namespace = child.getAttribute( "namespace" );
      final String localPart = child.getAttribute( "localPart" );

      qnames.add( new QName( namespace, localPart ) );
    }

    m_qnames = qnames.toArray( new QName[qnames.size()] );
  }

  /**
   * @see org.kalypsodeegree.model.feature.IGmlWorkspaceListener#getQNames()
   */
  public final QName[] getQNames( )
  {
    return m_qnames;
  }
}
