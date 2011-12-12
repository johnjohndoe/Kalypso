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

import java.util.HashMap;
import java.util.Map;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.timeseries.binding.IStation;
import org.kalypso.model.hydrology.timeseries.binding.IStationCollection;
import org.kalypso.model.hydrology.timeseries.binding.ITimeseries;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.editor.gmleditor.util.command.AddFeatureCommand;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBean;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBeanWizardPage;

/**
 * @author Gernot Belger
 */
public class NewStationWizard extends Wizard
{
  private final FeatureBean<IStation> m_bean;

  private final CommandableWorkspace m_workspace;

  private IStation m_newStation;

  public NewStationWizard( final CommandableWorkspace workspace, final FeatureBean<IStation> bean )
  {
    m_workspace = workspace;
    m_bean = bean;
  }

  @Override
  public void addPages( )
  {
    final FeatureBean<IStation> bean = m_bean;

    addPage( new FeatureBeanWizardPage( "beanPage" ) //$NON-NLS-1$
    {
      @Override
      protected Control createFeatureBeanControl( final Composite parent, final IDataBinding binding )
      {
        return new StationComposite( parent, bean, binding, true );
      }
    } );
  }

  @Override
  public boolean performFinish( )
  {
    try
    {
      final Map<QName, Object> properties = new HashMap<>( m_bean.getProperties() );

      final IStationCollection collection = (IStationCollection) m_workspace.getRootFeature();
      final IRelationType parentRelation = (IRelationType) collection.getFeatureType().getProperty( IStationCollection.MEMBER_STATION );

      final AddFeatureCommand command = new AddFeatureCommand( m_workspace, ITimeseries.FEATURE_TIMESERIES, collection, parentRelation, -1, properties, null, -1 );

      m_workspace.postCommand( command );

      m_newStation = (IStation) command.getNewFeature();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), "Failed to create new station", e );
      StatusDialog.open( getShell(), status, getWindowTitle() );
    }

    return false;
  }

  public IStation getNewStation( )
  {
    return m_newStation;
  }
}