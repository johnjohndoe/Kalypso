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

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.hydrology.binding.timeseries.IStation;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ui.rrm.internal.IUiRrmWorkflowConstants;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.timeseries.operations.ImportTimeseriesOperation;
import org.kalypso.ui.rrm.internal.timeseries.view.TimeseriesPropertiesComposite;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBean;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBeanWizardPage;
import org.kalypso.zml.ui.imports.ImportObservationData;
import org.kalypso.zml.ui.imports.ImportObservationSourcePage;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Dirk Kuch
 */
public class TimeseriesUpdateWizard extends Wizard
{
  private final ImportTimeseriesOperation m_importOperation;

  private final IMergeTimeseriesOperation m_mergeOperation;

  private FeatureBean<ITimeseries> m_bean;

  public TimeseriesUpdateWizard( final ITimeseries timeseries, final ImportTimeseriesOperation importOperation, final IMergeTimeseriesOperation mergeOperation, final ImportObservationData data )
  {
    m_importOperation = importOperation;
    m_mergeOperation = mergeOperation;
    m_bean = new FeatureBean<>( timeseries );

    addPage( new ImportObservationSourcePage( "sourcePage", data, true ) ); //$NON-NLS-1$

    final FeatureBean<ITimeseries> bean = m_bean;
    addPage( new FeatureBeanWizardPage( "beanPage" ) //$NON-NLS-1$
    {
      @Override
      protected Control createFeatureBeanControl( final Composite parent, final IDataBinding binding )
      {
        final IStation station = timeseries.getStation();

        return new TimeseriesPropertiesComposite( station, parent, bean, binding, true, null );
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

    /* Import the timeseries. */
    final IStatus status = RunnableContextHelper.execute( getContainer(), true, false, m_importOperation );
    if( !status.isOK() )
    {
      StatusDialog.open( getShell(), status, getWindowTitle() );
      return false;
    }

    /* Get the imported timeseries. */

    // FIXME: wrong place -> interpolation of missing values etc should take place after the merge, else we get holes in
    // the new timeseries

    final IObservation observation = m_importOperation.getObservation();
    m_mergeOperation.setObservation( observation );

    /* Merge the imported and existing timeseries. */
    final IStatus status2 = RunnableContextHelper.execute( getContainer(), true, false, m_mergeOperation );
    if( !status2.isOK() )
    {
      StatusDialog.open( getShell(), status2, getWindowTitle() );
      return false;
    }

    /* Change timeseries and save workspace  */
    final IStatus status3 = changeTimeseries(  );
    if( !status3.isOK() )
    {
      StatusDialog.open( getShell(), status3, getWindowTitle() );
      return false;
    }

    StatusDialog.open( getShell(), status, getWindowTitle() );
    return true;
  }

  private void saveSettings( )
  {
    m_importOperation.getData().storeSettings( getDialogSettings() );
  }

  private IStatus changeTimeseries( )
  {
    try
    {
      final IScenarioDataProvider dataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();

      final ICommand changes = m_bean.applyChanges();
      dataProvider.postCommand( IUiRrmWorkflowConstants.SCENARIO_DATA_STATIONS, changes );

      dataProvider.saveModel( IUiRrmWorkflowConstants.SCENARIO_DATA_STATIONS, new NullProgressMonitor() );
      return new Status( IStatus.OK, KalypsoUIRRMPlugin.getID(), Messages.getString("TimeseriesUpdateWizard.0") ); //$NON-NLS-1$
    }
    catch( final CoreException ex )
    {
      ex.printStackTrace();
      return ex.getStatus();
    }
    catch( final InvocationTargetException e )
    {
      final Throwable targetException = e.getTargetException();
      targetException.printStackTrace();

      return new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), "Failed to update timeseries properties", e ); //$NON-NLS-1$
    }
  }
}