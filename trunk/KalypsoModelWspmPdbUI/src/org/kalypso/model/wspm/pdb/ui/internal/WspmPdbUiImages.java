/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.model.wspm.pdb.ui.internal;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.internal.IWorkbenchGraphicConstants;
import org.eclipse.ui.internal.WorkbenchImages;
import org.kalypso.commons.eclipse.core.runtime.PluginImageProvider;
import org.kalypso.commons.eclipse.core.runtime.PluginImageProvider.ImageKey;

/**
 * Convenience class for storing references to image descriptors used by the readme tool.
 */
@SuppressWarnings( "restriction" )
public class WspmPdbUiImages
{
  public static enum IMAGE implements ImageKey
  {
    CONNECT_TO_PDB( "icons/led24/connect.png" ), //$NON-NLS-1$
    DISCONNECT_FROM_PDB( "icons/led24/disconnect.png" ), //$NON-NLS-1$
    SHOW_INFO( "icons/showInfo.gif" ), //$NON-NLS-1$

    PDB_CONNECTED( "icons/led24/connect.png" ), //$NON-NLS-1$
    PDB_DISCONNECTED( "icons/led24/disconnect.png" ), //$NON-NLS-1$

    STATE( "icons/state.gif" ), //$NON-NLS-1$
    EVENT_MEASURED( "icons/eventMeasured.gif" ), //$NON-NLS-1$
    EVENT_SIMULATION( "icons/eventSimulated.gif" ), //$NON-NLS-1$
    WATER_BODY( "icons/waterBody.png" ), //$NON-NLS-1$
    CROSS_SECTION( "icons/crossSection.gif" ), //$NON-NLS-1$

    REFRESH_CONTENT_VIEWER( "icons/refresh.gif" ), //$NON-NLS-1$
    PENDING( "icons/pending.gif" ), //$NON-NLS-1$ 

    IMPORT( "icons/import.gif" ), //$NON-NLS-1$
    EXPORT( "icons/export.gif" ), //$NON-NLS-1$

    WARNING_DECORATION( "icons/warning_small.gif" ), //$NON-NLS-1$

    ADD_FROM_EXTERNAL_LOCATION( "icons/addFromExternalLocation.gif" ); //$NON-NLS-1$

    private final String m_imagePath;

    private IMAGE( final String imagePath )
    {
      m_imagePath = imagePath;
    }

    /**
     * @see org.kalypso.informdss.KalypsoInformDSSImages.ImageKey#getImagePath()
     */
    @Override
    public String getImagePath( )
    {
      return m_imagePath;
    }
  }

  public static ImageDescriptor getImageDescriptor( final IMAGE image )
  {
    final PluginImageProvider imageProvider = WspmPdbUiPlugin.getDefault().getImageProvider();
    return imageProvider.getImageDescriptor( image );
  }

  public static Image getImage( final IMAGE image )
  {
    final PluginImageProvider imageProvider = WspmPdbUiPlugin.getDefault().getImageProvider();
    return imageProvider.getImage( image );
  }

  public static final ImageDescriptor IMG_WIZBAN_IMPORT_WIZ = WorkbenchImages.getImageDescriptor( IWorkbenchGraphicConstants.IMG_WIZBAN_IMPORT_WIZ );
}