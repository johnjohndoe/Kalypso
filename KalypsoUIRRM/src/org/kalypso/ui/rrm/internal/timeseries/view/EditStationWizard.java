/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.ui.rrm.internal.timeseries.view;

import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.ui.rrm.internal.timeseries.binding.Station;
import org.kalypso.ui.rrm.internal.timeseries.view.featureBinding.FeatureBean;
import org.kalypso.ui.rrm.internal.timeseries.view.featureBinding.FeatureBeanWizardPage;

/**
 * @author Gernot Belger
 */
public class EditStationWizard extends Wizard
{
  private final FeatureBean<Station> m_stationBean;

  private final ITimeseriesTreeModel m_model;

  public EditStationWizard( final ITimeseriesTreeModel model, final Station station )
  {
    m_model = model;
    m_stationBean = new FeatureBean<>( station );
  }

  @Override
  public void addPages( )
  {
    final FeatureBean<Station> stationBean = m_stationBean;

    final WizardPage page = new FeatureBeanWizardPage( "feature" ) //$NON-NLS-1$
    {
      @Override
      protected Control createFeatureBeanControl( final Composite parent, final IDataBinding binding )
      {
        return new StationComposite( parent, stationBean, binding, true );
      }
    };

    addPage( page );
  }

  @Override
  public boolean performFinish( )
  {
    try
    {
      final ICommand command = m_stationBean.applyChanges();
      m_model.postCommand( command );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    return true;
  }
}