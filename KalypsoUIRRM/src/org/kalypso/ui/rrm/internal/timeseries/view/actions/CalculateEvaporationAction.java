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

import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.contribs.eclipse.jface.dialog.DialogSettingsUtils;
import org.kalypso.model.hydrology.timeseries.binding.IStation;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.UIRrmImages;
import org.kalypso.ui.rrm.internal.UIRrmImages.DESCRIPTORS;
import org.kalypso.ui.rrm.internal.timeseries.view.evaporation.CalculateEvaporationWizard;
import org.kalypso.ui.rrm.internal.utils.featureTree.ITreeNodeModel;

/**
 * @author Dirk Kuch
 */
public class CalculateEvaporationAction extends Action
{
  private final IStation m_station;

  private final ITreeNodeModel m_model;

  public CalculateEvaporationAction( final ITreeNodeModel model, final IStation station )
  {
    m_station = station;
    m_model = model;

    setText( "Berechne Seeverdunstung" ); //$NON-NLS-1$
    setToolTipText( "Berechne Seeverdunstungszeitreihe auf Grundlage vorhandenen mittlere Eingangszeitreihen (Luftfeuchte, Temperatur, Sonnenscheindauer, Windgeschwindigkeit" ); //$NON-NLS-1$

    setImageDescriptor( UIRrmImages.id( DESCRIPTORS.PARAMETER_TYPE_EVAPORATION ) );
  }

  @Override
  public void runWithEvent( final Event event )
  {
    final Shell shell = event.widget.getDisplay().getActiveShell();

    final CommandableWorkspace workspace = m_model.getWorkspace();

    final CalculateEvaporationData data = new CalculateEvaporationData();
    final IDialogSettings settings = DialogSettingsUtils.getDialogSettings( KalypsoUIRRMPlugin.getDefault(), CalculateEvaporationWizard.class.getName() );
    data.init( settings );

    final CalculateEvaporationWizard wizard = new CalculateEvaporationWizard( workspace, m_station, data );
    wizard.setDialogSettings( settings );
    wizard.setWindowTitle( getText() );

    final WizardDialog dialog = new WizardDialog( shell, wizard );
    if( dialog.open() == org.eclipse.jface.window.Window.OK )
    {

    }

    /* Prepare data */
// final ImportObservationData data = prepareData();
// final ITimeseries timeseries = showWizard( shell, data );
//
// m_model.refreshTree( timeseries );
  }

}