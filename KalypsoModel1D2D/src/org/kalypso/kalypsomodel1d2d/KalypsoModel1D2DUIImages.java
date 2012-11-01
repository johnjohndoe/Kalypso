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
package org.kalypso.kalypsomodel1d2d;

import org.kalypso.commons.eclipse.core.runtime.PluginImageProvider.ImageKey;

/**
 * @author Thomas Jung
 */
public class KalypsoModel1D2DUIImages
{
  public enum IMGKEY implements ImageKey
  {
    OK( "icons/elcl16/ok.gif" ), //$NON-NLS-1$
    EDIT( "icons/elcl16/edit.gif" ), //$NON-NLS-1$
    SELECT( "icons/elcl16/boxSel.gif" ), //$NON-NLS-1$
    DELETE( "icons/elcl16/remove.gif" ), //$NON-NLS-1$
    CHANGE( "icons/elcl16/change.gif" ), //$NON-NLS-1$
    PLAY( "icons/elcl16/nav_go.gif" ), //$NON-NLS-1$
    GO( "icons/elcl16/blue_arrow_right.gif" ), //$NON-NLS-1$
    ADD( "icons/elcl16/addrepo_rep.gif" ), //$NON-NLS-1$
    RUN_SIM( "icons/startCalculation.gif" ), //$NON-NLS-1$
    ZOOM_WITH_RECT( "icons/elcl16/zoomin.gif" ), //$NON-NLS-1$
    PAN( "icons/elcl16/pan.gif" ), //$NON-NLS-1$
    HYDROGRAPH_ADD( "icons/elcl16/hydrograph_create.gif" ), //$NON-NLS-1$
    HYDROGRAPH_EDIT( "icons/elcl16/hydrograph_edit.gif" ), //$NON-NLS-1$
    HYDROGRAPH_SELECT( "icons/elcl16/hydrograph_select.gif" ), //$NON-NLS-1$
    HYDROGRAPH_REMOVE( "icons/elcl16/hydrograph_delete.gif" ), //$NON-NLS-1$
    HYDROGRAPH_JUMP_TO( "icons/elcl16/nav_go.gif" ), //$NON-NLS-1$
    HYDROGRAPH_EXPORT( "icons/elcl16/hydrograph_export.gif" ), //$NON-NLS-1$
    HYDROGRAPH_IMPORT( "icons/elcl16/hydrograph_import.gif" ), //$NON-NLS-1$
    HYDROGRAPH_COLLECTION_ADD( "icons/elcl16/add.gif" ), //$NON-NLS-1$
    HYDROGRAPH_COLLECTION_REMOVE( "icons/elcl16/remove.gif" ), //$NON-NLS-1$
    HYDROGRAPH_COLLECTION_PROCESS( "icons/elcl16/hydro_process.gif" ), //$NON-NLS-1$
    ELEVATION_SHOW( "icons/elcl16/23_show_elevationmodel.gif" ), //$NON-NLS-1$
    ELEVATION_DELETE( "icons/elcl16/25_cut_elevationmodel.gif" ), //$NON-NLS-1$
    ELEVATION_MOVE_UP( "icons/elcl16/list_up.gif" ), //$NON-NLS-1$
    ELEVATION_MOVE_DOWN( "icons/elcl16/list_down.gif" ), //$NON-NLS-1$
    IMPORT_EXTERNAL_RESULT( "icons/obj16/importExternalResult.gif" ), //$NON-NLS-1$
    CHANNEL_EDIT_INFO( "icons/obj16/createChannelInfo.png" ); //$NON-NLS-1$

    private final String m_imagePath;

    private IMGKEY( final String imagePath )
    {
      m_imagePath = imagePath;
    }

    /**
     * @see org.kalypso.commons.eclipse.core.runtime.PluginImageProvider.ImageKey#getImagePath()
     */
    @Override
    public String getImagePath( )
    {
      return m_imagePath;
    }

  }

  private KalypsoModel1D2DUIImages( )
  {
    // wird nicht instantiiert
  }

}