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
package org.kalypso.ui.rrm.internal.gml.feature.view;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.model.hydrology.binding.timeseries.IStationCollection;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ui.rrm.internal.gml.feature.view.dialogs.ChooseTimeseriesDialog;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.zml.obslink.TimeseriesLinkType;

/**
 * @author Dirk Kuch
 * @author Gernot Belger
 */
public class ChooseZmlLinkAction extends Action
{
  private final ChooseZmlLinkFeatureViewControl m_featureControl;

  private String m_parameterType;

  public ChooseZmlLinkAction( final ChooseZmlLinkFeatureViewControl featureControl, final String text )
  {
    super( text );

    m_featureControl = featureControl;
  }

  @Override
  public void runWithEvent( final Event event )
  {
    final Shell shell = event.widget.getDisplay().getActiveShell();

    final CommandableWorkspace workspace = FindTimeseriesLinkRunnable.getStationsWorkspace();
    final IStationCollection collection = FindTimeseriesLinkRunnable.getStationCollection();

    if( Objects.isNull( workspace, collection ) )
    {
      MessageDialog.openError( shell, Messages.getString( "ChooseZmlLinkFeatureViewControl_0" ), Messages.getString( "ChooseZmlLinkFeatureViewControl_1" ) ); //$NON-NLS-1$ //$NON-NLS-2$
      return;
    }

    final ITimeseries timeseries = m_featureControl.getTimeseries();

    final ChooseTimeseriesDialog dialog = new ChooseTimeseriesDialog( shell, workspace, collection, m_parameterType );
    dialog.setSelection( timeseries );
    if( dialog.open() == Window.OK )
    {
      final ITimeseries selection = dialog.getSelection();
      final ZmlLink link = selection == null ? null : selection.getDataLink();
      final TimeseriesLinkType linkType = link == null ? null : link.getTimeseriesLink();

      m_featureControl.changeLink( linkType );
    }
  }
}