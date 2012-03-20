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
package org.kalypso.ui.rrm.internal.timeseries.view;

import java.net.URL;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypso.model.hydrology.timeseries.binding.IStation;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBean;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBeanWizardPage;
import org.kalypso.ui.rrm.internal.utils.featureTree.ITreeNodeModel;

/**
 * @author Gernot Belger
 */
public class EditStationWizard extends Wizard
{
  private final FeatureBean<IStation> m_stationBean;

  private final ITreeNodeModel m_model;

  private String m_oldFolder;

  public EditStationWizard( final ITreeNodeModel model, final IStation station )
  {
    m_model = model;
    m_stationBean = new FeatureBean<>( station );

    init( station );
  }

  private void init( final IStation station )
  {
    m_oldFolder = station.getTimeseriesFoldername();

  }

  @Override
  public void addPages( )
  {
    final FeatureBean<IStation> stationBean = m_stationBean;

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

    final IStation station = m_stationBean.getFeature();
    final String folder = station.getTimeseriesFoldername();
    if( !StringUtils.equals( m_oldFolder, folder ) )
      doMoveStationFolder( station, folder );

    return true;
  }

  private void doMoveStationFolder( final IStation station, final String folder )
  {
    try
    {
      final URL context = station.getWorkspace().getContext();

      final URL urlSource = UrlResolverSingleton.resolveUrl( context, m_oldFolder );
      final URL urlTarget = UrlResolverSingleton.resolveUrl( context, folder );

      final IFolder source = ResourceUtilities.findFolderFromURL( urlSource );
      final IFolder target = ResourceUtilities.findFolderFromURL( urlTarget );

      if( source.exists() )
        source.move( target.getFullPath(), true, new NullProgressMonitor() );

      target.refreshLocal( IResource.DEPTH_INFINITE, new NullProgressMonitor() );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }
}