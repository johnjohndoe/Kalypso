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
package org.kalypso.ui.rrm.internal.hydrotops;

import java.net.URL;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.core.util.pool.IPoolableObjectType;
import org.kalypso.core.util.pool.PoolableObjectType;
import org.kalypso.core.util.pool.ResourcePool;
import org.kalypso.model.hydrology.binding.parameter.Parameter;
import org.kalypso.model.hydrology.project.RrmScenario;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.handlers.MapHandlerUtils;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.FeatureSelectionHelper;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * Common code in association with Hydrotope stuff.
 * 
 * @author Gernot Belger
 */
public final class HydrotopesHelper
{
  private HydrotopesHelper( )
  {
    throw new UnsupportedOperationException();
  }

  public static CommandableWorkspace findWorkspaceForImport( final IStructuredSelection selection )
  {
    final IKalypsoTheme[] themes = MapHandlerUtils.getSelectedThemes( selection );
    for( final IKalypsoTheme theme : themes )
    {
      if( theme instanceof IKalypsoFeatureTheme )
      {
        final IKalypsoFeatureTheme featureTheme = (IKalypsoFeatureTheme) theme;
        return featureTheme.getWorkspace();
      }
    }

    if( selection instanceof IFeatureSelection )
    {
      final IFeatureSelection fs = (IFeatureSelection) selection;
      final Feature firstFeature = FeatureSelectionHelper.getFirstFeature( fs );
      return fs.getWorkspace( firstFeature );
    }

    throw new IllegalArgumentException();
  }

  public static Parameter findParameterModel( final Shell shell, final String windowTitle, final GMLWorkspace contextWorkspace )
  {
    final URL contextLocation = contextWorkspace.getContext();

    final IPoolableObjectType poolKey = new PoolableObjectType( "gml", RrmScenario.FILE_PARAMETER_GML, contextLocation ); //$NON-NLS-1$

    try
    {
      final ResourcePool pool = KalypsoCorePlugin.getDefault().getPool();
      final CommandableWorkspace parameterWorkspace = (CommandableWorkspace) pool.getObject( poolKey );
      return (Parameter) parameterWorkspace.getRootFeature();
    }
    catch( final CoreException e )
    {
      final String message = Messages.getString( "org.kalypso.ui.rrm.internal.hydrotops.ImportPedologyWizard.3" ); //$NON-NLS-1$
      final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), message, e );

      KalypsoUIRRMPlugin.getDefault().getLog().log( status );

      StatusDialog.open( shell, status, windowTitle );

      return null;
    }
  }
}