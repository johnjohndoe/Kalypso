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
package org.kalypso.ui.rrm.internal.timeseries.view;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.model.hydrology.timeseries.binding.IHydrologicalStation;
import org.kalypso.model.hydrology.timeseries.binding.IStation;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.rrm.internal.UIRrmImages;
import org.kalypso.ui.rrm.internal.UIRrmImages.DESCRIPTORS;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBean;

/**
 * @author Gernot Belger
 */
public class NewHydrologicalStationAction extends Action
{
  private final ITimeseriesTreeModel m_model;

  private final String m_group;

  public NewHydrologicalStationAction( final ITimeseriesTreeModel model, final String group )
  {
    m_model = model;
    m_group = group;

    setText( "New hydrological station" );
    setToolTipText( "Creates a new hydrologcial station" );
    setImageDescriptor( UIRrmImages.id( DESCRIPTORS.STATION_NEW_HYDROLOGICAL ) );
  }

  @Override
  public void runWithEvent( final Event event )
  {
    final Shell shell = event.widget.getDisplay().getActiveShell();

    final CommandableWorkspace workspace = m_model.getWorkspace();

    final FeatureBean<IStation> bean = new FeatureBean<>( IHydrologicalStation.FEATURE_HYDROLOGICAL_STATION );
    bean.setProperty( IStation.PROPERTY_GROUP, m_group );

    final NewStationWizard wizard = new NewStationWizard( workspace, bean );
    wizard.setWindowTitle( getText() );

    final WizardDialog dialog = new WizardDialog( shell, wizard );
    if( dialog.open() != Window.OK )
      return;

    /* Refresh tree */
    final IStation newStation = wizard.getNewStation();
    m_model.refreshTree( newStation );
  }
}