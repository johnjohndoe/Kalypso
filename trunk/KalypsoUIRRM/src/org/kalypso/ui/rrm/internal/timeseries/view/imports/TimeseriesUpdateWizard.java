/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.hydrology.timeseries.binding.IStation;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ui.rrm.internal.timeseries.operations.ImportTimeseriesOperation;
import org.kalypso.ui.rrm.internal.timeseries.view.TimeseriesBean;
import org.kalypso.ui.rrm.internal.timeseries.view.TimeseriesPropertiesComposite;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBeanWizardPage;
import org.kalypso.zml.ui.imports.ImportObservationData;
import org.kalypso.zml.ui.imports.ImportObservationSourcePage;

/**
 * @author Dirk Kuch
 */
public class TimeseriesUpdateWizard extends Wizard
{
  private final ImportTimeseriesOperation m_importOperation;

  private final IMergeTimeseriesOperation m_mergeOperation;

  public TimeseriesUpdateWizard( final IStation station, final ImportTimeseriesOperation importOperation, final IMergeTimeseriesOperation mergeOperation, final ImportObservationData data, final TimeseriesBean bean )
  {
    m_importOperation = importOperation;
    m_mergeOperation = mergeOperation;

    addPage( new ImportObservationSourcePage( "sourcePage", data, true ) ); //$NON-NLS-1$

    addPage( new FeatureBeanWizardPage( "beanPage" ) //$NON-NLS-1$
    {
      @Override
      protected Control createFeatureBeanControl( final Composite parent, final IDataBinding binding )
      {
        return new TimeseriesPropertiesComposite( station, parent, bean, binding, true );
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
    saveSettings();

    final IStatus status = RunnableContextHelper.execute( getContainer(), true, false, m_importOperation );
    if( !status.isOK() )
    {
      StatusDialog.open( getShell(), status, getWindowTitle() );
      return true;
    }

    final IObservation observation = m_importOperation.getObservation();
    m_mergeOperation.setObservation( observation );

    final IStatus status2 = RunnableContextHelper.execute( getContainer(), true, false, m_mergeOperation );
    if( !status2.isOK() )
      StatusDialog.open( getShell(), status2, getWindowTitle() );
    else
      StatusDialog.open( getShell(), status, getWindowTitle() );

    return true;
  }

  private void saveSettings( )
  {
    m_importOperation.getData().storeSettings( getDialogSettings() );
  }
}