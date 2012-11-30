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
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.contribs.eclipse.jface.dialog.DialogSettingsUtils;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.timeseries.operations.ImportTimeseriesOperation;
import org.kalypso.ui.rrm.internal.timeseries.view.imports.IMergeTimeseriesOperation;
import org.kalypso.ui.rrm.internal.timeseries.view.imports.TimeseriesImportWizard;
import org.kalypso.ui.rrm.internal.timeseries.view.imports.TimeseriesUpdateWizard;
import org.kalypso.ui.rrm.internal.utils.featureTree.ITreeNodeModel;
import org.kalypso.zml.ui.imports.ImportObservationData;

/**
 * @author Dirk Kuch
 */
public abstract class AbstractOverwriteTimeseriesAction extends Action
{
  private final ITreeNodeModel m_model;

  private final ITimeseries m_timeseries;

  public AbstractOverwriteTimeseriesAction( final ITreeNodeModel model, final ITimeseries timeseries )
  {
    m_model = model;
    m_timeseries = timeseries;
  }

  protected ITimeseries getTimeseries( )
  {
    return m_timeseries;
  }

  @Override
  public void runWithEvent( final Event event )
  {
    final Shell shell = event.widget.getDisplay().getActiveShell();

    /* Prepare data */
    final String parameterType = m_timeseries.getParameterType();
    final ImportObservationData data = new ImportObservationData( parameterType );
    final ITimeseries timeseries = showWizard( shell, data );
    if( timeseries != null )
    {
      /* HACK: Reset the selection and set it again. */
      /* Like this the timeseries composite will be refreshed. */
      m_model.refreshTree( (ITimeseries) null );
      m_model.refreshTree( timeseries );
    }
  }

  private ITimeseries showWizard( final Shell shell, final ImportObservationData data )
  {
    final ImportTimeseriesOperation importOperation = new ImportTimeseriesOperation( data, null );

    final IDialogSettings settings = DialogSettingsUtils.getDialogSettings( KalypsoUIRRMPlugin.getDefault(), TimeseriesImportWizard.class.getName() );
    data.init( settings );

    data.setParameterType( m_timeseries.getParameterType() );
    importOperation.setDescription( m_timeseries.getDescription() );
    importOperation.setQuality( m_timeseries.getQuality() );

    final TimeseriesUpdateWizard wizard = new TimeseriesUpdateWizard( m_timeseries, importOperation, getMergeOperation(), data );
    wizard.setDialogSettings( settings );
    wizard.setWindowTitle( getText() );

    final WizardDialog dialog = new WizardDialog( shell, wizard );
    if( dialog.open() == Window.OK )
      return m_timeseries;

    return null;
  }

  protected abstract IMergeTimeseriesOperation getMergeOperation( );
}