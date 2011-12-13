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
package org.kalypso.ui.rrm.internal.cm.view;

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
import org.kalypso.model.hydrology.cm.binding.ICatchmentModel;
import org.kalypso.model.rcm.binding.ILinearSumGenerator;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ui.editor.gmleditor.util.command.AddFeatureCommand;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBean;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBeanWizardPage;

/**
 * @author Gernot Belger
 */
// FIXME: delete
public class NewGeneratorLinearSumWizard extends Wizard
{
  private final CommandableWorkspace m_workspace;

  private final FeatureBean<ILinearSumGenerator> m_bean;

  public NewGeneratorLinearSumWizard( final CommandableWorkspace workspace, final FeatureBean<ILinearSumGenerator> bean )
  {
    m_workspace = workspace;
    m_bean = bean;
  }

  @Override
  public void addPages( )
  {
    final FeatureBean<ILinearSumGenerator> bean = m_bean;

    final String[] allowedParameterTypes = new String[] { ITimeseriesConstants.TYPE_RAINFALL, ITimeseriesConstants.TYPE_TEMPERATURE, ITimeseriesConstants.TYPE_EVAPORATION };

    addPage( new FeatureBeanWizardPage( "beanPage" ) //$NON-NLS-1$
    {
      @Override
      protected Control createFeatureBeanControl( final Composite parent, final IDataBinding binding )
      {
        return new LinearSumNewComposite( parent, bean, binding, allowedParameterTypes );
      }
    } );
  }

  @Override
  public boolean performFinish( )
  {
    try
    {

      // FIXME: copy catchments from model.gml

      final Map<QName, Object> properties = new HashMap<>( m_bean.getProperties() );

      final ICatchmentModel collection = (ICatchmentModel) m_workspace.getRootFeature();
      final IRelationType parentRelation = (IRelationType) collection.getFeatureType().getProperty( ICatchmentModel.MEMBER_CATCHMENT_GENERATOR );

      final QName type = m_bean.getFeatureType().getQName();

      final AddFeatureCommand command = new AddFeatureCommand( m_workspace, type, collection, parentRelation, -1, properties, null, -1 );

      m_workspace.postCommand( command );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), "Failed to create new generator", e ); //$NON-NLS-1$
      StatusDialog.open( getShell(), status, getWindowTitle() );
      return false;
    }

    return true;
  }
}
