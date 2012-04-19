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
package org.kalypso.ui.rrm.internal.timeseries.view.imports;

import java.io.File;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.hydrology.timeseries.binding.IStation;
import org.kalypso.model.hydrology.timeseries.binding.ITimeseries;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.timeseries.operations.ImportTimeseriesOperation;
import org.kalypso.ui.rrm.internal.timeseries.operations.StoreTimeseriesOperation;
import org.kalypso.ui.rrm.internal.timeseries.view.TimeseriesBean;
import org.kalypso.ui.rrm.internal.timeseries.view.TimeseriesPropertiesComposite;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBeanWizardPage;
import org.kalypso.zml.ui.imports.IImportObservationSourceChangedListener;
import org.kalypso.zml.ui.imports.ImportObservationData;
import org.kalypso.zml.ui.imports.ImportObservationSourcePage;

/**
 * @author Gernot Belger
 */
public class TimeseriesImportWizard extends Wizard
{
  private final ImportTimeseriesOperation m_importOperation;

  private final CommandableWorkspace m_workspace;

  protected final IStation m_station;

  private final TimeseriesBean m_bean;

  private ITimeseries m_timeseries;

  public TimeseriesImportWizard( final ImportTimeseriesOperation importOperation, final ImportObservationData data, final TimeseriesBean bean, final CommandableWorkspace workspace, final IStation station )
  {
    m_importOperation = importOperation;
    m_bean = bean;
    m_workspace = workspace;
    m_station = station;

    final ImportObservationSourcePage importPage = new ImportObservationSourcePage( "sourcePage", data );
    importPage.addListener( new IImportObservationSourceChangedListener()
    {
      @Override
      public void sourceFileChanged( final File file )
      {
        String description = "";
        if( Objects.isNotNull( file ) )
        {
          description = String.format( "Quelle: %s", file.getAbsolutePath() );
        }

        bean.setProperty( ITimeseries.QN_DESCRIPTION, description );
      }
    } );

    addPage( importPage ); //$NON-NLS-1$

    addPage( new FeatureBeanWizardPage( "beanPage" ) //$NON-NLS-1$
    {
      @Override
      protected Control createFeatureBeanControl( final Composite parent, final IDataBinding binding )
      {
        return new TimeseriesPropertiesComposite( m_station, parent, bean, binding, false );
      }
    } );

  }

  @Override
  public boolean performCancel( )
  {
    saveSettings();

    return super.performCancel();
  }

  @Override
  public boolean performFinish( )
  {
    final StatusCollector stati = new StatusCollector( KalypsoUIRRMPlugin.getID() );
    saveSettings();

    stati.add( RunnableContextHelper.execute( getContainer(), true, false, m_importOperation ) );
    if( stati.matches( IStatus.ERROR ) )
    {
      doShowStatusDialog( stati );
      return false;
    }

    final StoreTimeseriesOperation storeOperation = new StoreTimeseriesOperation( m_bean, m_workspace, m_station, m_importOperation );
    storeOperation.updateDataAfterFinish();

    stati.add( RunnableContextHelper.execute( getContainer(), true, false, storeOperation ) );
    m_timeseries = storeOperation.getTimeseries();

    doShowStatusDialog( stati );

    return !stati.matches( IStatus.ERROR );
  }

  private void doShowStatusDialog( final StatusCollector stati )
  {
    final IStatus status = stati.asMultiStatus( "Zeitreihen-Import" );

    final StatusDialog dialog = new StatusDialog( getShell(), status, "Zeitreihen-Import" );
    dialog.open();
  }

  private void saveSettings( )
  {
    m_importOperation.getData().storeSettings( getDialogSettings() );
  }

  public ITimeseries getTimeseries( )
  {
    return m_timeseries;
  }
}