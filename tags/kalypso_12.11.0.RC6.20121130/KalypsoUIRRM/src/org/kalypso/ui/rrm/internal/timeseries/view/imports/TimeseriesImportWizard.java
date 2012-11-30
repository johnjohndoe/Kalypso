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

import org.eclipse.core.databinding.validation.IValidator;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.hydrology.binding.timeseries.IParameterTypeProvider;
import org.kalypso.model.hydrology.binding.timeseries.IStation;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.ui.rrm.internal.IUiRrmWorkflowConstants;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.timeseries.QualityUniqueValidator;
import org.kalypso.ui.rrm.internal.timeseries.operations.ImportTimeseriesOperation;
import org.kalypso.ui.rrm.internal.timeseries.operations.StoreTimeseriesOperation;
import org.kalypso.ui.rrm.internal.timeseries.operations.StoreTimeseriesStatusOperation;
import org.kalypso.ui.rrm.internal.timeseries.view.TimeseriesBean;
import org.kalypso.ui.rrm.internal.timeseries.view.TimeseriesPropertiesComposite;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBeanWizardPage;
import org.kalypso.zml.ui.imports.IImportObservationSourceChangedListener;
import org.kalypso.zml.ui.imports.ImportObservationData;
import org.kalypso.zml.ui.imports.ImportObservationSourcePage;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Gernot Belger
 */
public class TimeseriesImportWizard extends Wizard
{
  private final ImportTimeseriesOperation m_importOperation;

  protected final IStation m_station;

  private final TimeseriesBean m_bean;

  private ITimeseries m_timeseries;

  public TimeseriesImportWizard( final ImportTimeseriesOperation importOperation, final TimeseriesBean bean, final IStation station )
  {
    m_importOperation = importOperation;
    m_bean = bean;
    m_station = station;

    setNeedsProgressMonitor( true );

    final ImportObservationData data = m_importOperation.getData();

    final IParameterTypeProvider parameterTypeProvider = new IParameterTypeProvider()
    {
      @Override
      public String getParameterType( )
      {
        return data.getParameterType();
      }
    };

    final ImportObservationSourcePage importPage = new ImportObservationSourcePage( "sourcePage", data ); //$NON-NLS-1$
    importPage.addListener( new IImportObservationSourceChangedListener()
    {
      @Override
      public void sourceFileChanged( final File file )
      {
        String description = ""; //$NON-NLS-1$
        if( Objects.isNotNull( file ) )
        {
          description = String.format( Messages.getString( "TimeseriesImportWizard_2" ), file.getAbsolutePath() ); //$NON-NLS-1$
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
        // REMARK: do not check current quality, we are creating a new timeseries
        final String currentQuality = null;

        final IValidator qualityValidator = new QualityUniqueValidator( station, currentQuality, parameterTypeProvider );

        return new TimeseriesPropertiesComposite( m_station, parent, bean, binding, false, qualityValidator );
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
    /* Save the settings. */
    saveSettings();

    /* The status collector. */
    final StatusCollector stati = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    /* Import the timeseries. */
    final IStatus importTimeseriesStatus = RunnableContextHelper.execute( getContainer(), true, false, m_importOperation );
    stati.add( importTimeseriesStatus );
    if( stati.matches( IStatus.ERROR ) )
    {
      doShowStatusDialog( stati.asMultiStatus( Messages.getString( "TimeseriesImportWizard_3" ) ) ); //$NON-NLS-1$
      return false;
    }

    /* Store the timeseries. */
    m_importOperation.setQuality( (String) m_bean.getProperty( ITimeseries.PROPERTY_QUALITY ) );
    m_importOperation.setDescription( (String) m_bean.getProperty( ITimeseries.QN_DESCRIPTION ) );

    final StoreTimeseriesOperation storeOperation = new StoreTimeseriesOperation( m_station, m_importOperation );
    final IStatus storeTimeseriesStatus = RunnableContextHelper.execute( getContainer(), true, false, storeOperation );
    stati.add( storeTimeseriesStatus );

    /* Get the timeseries. */
    m_timeseries = storeOperation.getTimeseries();

    /* Store the timeseries status. */
    final StoreTimeseriesStatusOperation storeStatusOperation = new StoreTimeseriesStatusOperation( m_timeseries, stati.asMultiStatus( Messages.getString( "TimeseriesImportWizard_3" ) ) ); //$NON-NLS-1$
    final IStatus storeStatusStatus = RunnableContextHelper.execute( getContainer(), true, false, storeStatusOperation );
    stati.add( storeStatusStatus );

    /* Save the workspace of the stations. */
    final IStatus saveStationsStatus = saveStations();
    stati.add( saveStationsStatus );

    /* Show the status dialog. */
    doShowStatusDialog( stati.asMultiStatus( Messages.getString( "TimeseriesImportWizard_3" ) ) ); //$NON-NLS-1$

    return !stati.matches( IStatus.ERROR );
  }

  private void doShowStatusDialog( final IStatus status )
  {
    StatusDialog.open( getShell(), status, getWindowTitle() );
  }

  private void saveSettings( )
  {
    m_importOperation.getData().storeSettings( getDialogSettings() );
  }

  private IStatus saveStations( )
  {
    try
    {
      final IScenarioDataProvider dataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
      dataProvider.saveModel( IUiRrmWorkflowConstants.SCENARIO_DATA_STATIONS, new NullProgressMonitor() );
      return new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), Messages.getString("TimeseriesImportWizard.2") ); //$NON-NLS-1$
    }
    catch( final CoreException ex )
    {
      ex.printStackTrace();
      return ex.getStatus();
    }
  }

  public ITimeseries getTimeseries( )
  {
    return m_timeseries;
  }
}