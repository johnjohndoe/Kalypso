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
package org.kalypso.ui.rrm.internal.timeseries.view.actions;

import org.apache.commons.lang3.ObjectUtils;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.model.hydrology.binding.timeseries.IStation;
import org.kalypso.ui.rrm.internal.UIRrmImages;
import org.kalypso.ui.rrm.internal.UIRrmImages.DESCRIPTORS;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.timeseries.view.EditStationWizard;
import org.kalypso.ui.rrm.internal.timeseries.view.StationComposite;
import org.kalypso.ui.rrm.internal.utils.featureTree.ITreeNodeModel;

/**
 * @author Gernot Belger
 */
public class EditStationAction extends Action
{
  private final IStation m_station;

  private final StationComposite m_stationControl;

  private final ITreeNodeModel m_context;

  public EditStationAction( final ITreeNodeModel context, final IStation station, final StationComposite stationControl )
  {
    m_context = context;
    m_station = station;
    m_stationControl = stationControl;

    setText( Messages.getString( "EditStationAction_0" ) ); //$NON-NLS-1$
    setToolTipText( Messages.getString( "EditStationAction_1" ) ); //$NON-NLS-1$

    setImageDescriptor( UIRrmImages.id( DESCRIPTORS.EDIT_STATION ) );
  }

  @Override
  public void runWithEvent( final Event event )
  {
    final Shell shell = event.widget.getDisplay().getActiveShell();

    final String oldGroup = m_station.getGroup();

    final Wizard wizard = new EditStationWizard( m_station );
    wizard.setWindowTitle( Messages.getString( "EditStationAction_2" ) ); //$NON-NLS-1$

    final WizardDialog dialog = new WizardDialog( shell, wizard );
    if( dialog.open() != Window.OK )
      return;

    m_stationControl.refresh();

    final String newGroup = m_station.getGroup();
    if( !ObjectUtils.equals( oldGroup, newGroup ) )
    {
      m_context.clear();
      m_context.refreshTree( m_station );
    }
  }
}