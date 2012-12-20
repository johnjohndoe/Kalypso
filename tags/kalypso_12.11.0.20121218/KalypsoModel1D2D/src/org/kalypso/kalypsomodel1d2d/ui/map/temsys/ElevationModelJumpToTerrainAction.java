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
package org.kalypso.kalypsomodel1d2d.ui.map.temsys;

import org.eclipse.jface.action.Action;
import org.kalypso.commons.eclipse.core.runtime.PluginImageProvider;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DUIImages;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypsodeegree.model.elevation.ElevationException;
import org.kalypsodeegree.model.elevation.IElevationModel;

/**
 * @author Gernot Belger
 */
class ElevationModelJumpToTerrainAction extends Action
{
  private final ApplyElevationWidgetDataModel m_dataModel;

  ElevationModelJumpToTerrainAction( final ApplyElevationWidgetDataModel dataModel )
  {
    m_dataModel = dataModel;

    setToolTipText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ElevationModelSystemEditorComponent.10" ) ); //$NON-NLS-1$

    final PluginImageProvider imageProvider = KalypsoModel1D2DPlugin.getImageProvider();
    setImageDescriptor( imageProvider.getImageDescriptor( KalypsoModel1D2DUIImages.IMGKEY.ELEVATION_SHOW ) );
  }

  @Override
  public void run( )
  {
    try
    {
      final IElevationModel elevationModel = m_dataModel.getElevationProvider();
      if( elevationModel != null )
        m_dataModel.getMapPanel().setBoundingBox( elevationModel.getBoundingBox() );
    }
    catch( final ElevationException e )
    {
      e.printStackTrace();
    }
  }
}