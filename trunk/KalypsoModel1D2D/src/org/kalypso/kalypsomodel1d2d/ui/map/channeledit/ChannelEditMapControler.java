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
package org.kalypso.kalypsomodel1d2d.ui.map.channeledit;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.editdata.IProfileData;
import org.kalypso.ogc.gml.IKalypsoLayerModell;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * Repaint the map if the data object changes.<br/>
 * Also handles the 'auto zoom' functionality.<br/>
 * If the 'autoZoom' button is on, always jump to active profile.
 *
 * @author Gernot Belger
 */
class ChannelEditMapControler implements PropertyChangeListener
{
  private final ChannelEditWidget m_widget;

  public ChannelEditMapControler( final ChannelEditWidget widget )
  {
    m_widget = widget;
  }

  @Override
  public void propertyChange( final PropertyChangeEvent evt )
  {
    final ChannelEditData data = (ChannelEditData)evt.getSource();

    final String propertyName = evt.getPropertyName();

    switch( propertyName )
    {
    /* auto zoom, if active profile changes (or auto zoom changes to true ) */
      case ChannelEditData.PROPERTY_ACTIVE_PROFILE:
      case ChannelEditData.PROPERTY_PROFILE_AUTO_ZOOM:
        jumpToActiveProfile( data );
        break;

      default:
        break;
    }

    // repaint in all cases
    m_widget.repaintMap();
  }

  private void jumpToActiveProfile( final ChannelEditData data )
  {
    if( !data.getProfileAutoZoom() )
      return;

    final IProfileData activeProfile = data.getActiveProfile();
    if( activeProfile == null )
      return;

    final IMapPanel panel = m_widget.getMapPanel();
    if( panel == null )
      return;

    final IKalypsoLayerModell modell = panel.getMapModell();
    if( modell == null )
      return;

    try
    {
      final GM_Envelope mapExtent = activeProfile.getMapExtent( modell.getCoordinatesSystem() );
      if( mapExtent == null )
        return;

      final GM_Envelope scaledEnvelope = GeometryUtilities.scaleEnvelope( mapExtent, 1.20 );
      panel.setBoundingBox( scaledEnvelope );
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
    }
  }
}