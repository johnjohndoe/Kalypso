/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.ogc.gml;

import org.eclipse.core.runtime.IAdapterFactory;
import org.eclipse.ui.IActionFilter;
import org.kalypso.contribs.eclipse.ui.actionfilters.ActionFilterRegistry;

/**
 * @author schlienger
 */
public class FeatureThemeActionFilterAdapterFactory implements IAdapterFactory
{
  private final ActionFilterRegistry m_featureActionFilter;

  public FeatureThemeActionFilterAdapterFactory( )
  {
    m_featureActionFilter = new ActionFilterRegistry( IKalypsoFeatureTheme.class.getName() );
  }

  /**
   * @see org.eclipse.core.runtime.IAdapterFactory#getAdapter(java.lang.Object, java.lang.Class)
   */
  public Object getAdapter( Object adaptableObject, Class adapterType )
  {
    if( adapterType == IActionFilter.class )
      return m_featureActionFilter;
    
    return null;
  }

  /**
   * @see org.eclipse.core.runtime.IAdapterFactory#getAdapterList()
   */
  public Class[] getAdapterList( )
  {
    final Class[] classes = { IActionFilter.class };
    return classes;
  }
}
