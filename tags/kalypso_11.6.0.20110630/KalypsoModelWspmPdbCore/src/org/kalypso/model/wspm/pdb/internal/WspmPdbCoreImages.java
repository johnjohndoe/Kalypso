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
package org.kalypso.model.wspm.pdb.internal;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.plugin.AbstractUIPlugin;

/**
 * @author Gernot Belger
 */
public class WspmPdbCoreImages
{
  public static final ImageDescriptor id( final String pluginID, final String location )
  {
    return AbstractUIPlugin.imageDescriptorFromPlugin( pluginID, location );
  }

  public static final ImageDescriptor id( final String location )
  {
    return id( WspmPdbCorePlugin.PLUGIN_ID, location );
  }

  public static final ImageDescriptor IMAGE_POSTGIS_32x32 = id( "icons/PostGIS_32x32.gif" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_POSTGIS_64x64 = id( "icons/PostGIS_64x64.jpg" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_ORACLE_32x32 = id( "icons/Oracle_32x32.jpg" ); //$NON-NLS-1$

  public static final ImageDescriptor IMAGE_ORACLE_48x48 = id( "icons/Oracle_48x48.png" ); //$NON-NLS-1$
}
