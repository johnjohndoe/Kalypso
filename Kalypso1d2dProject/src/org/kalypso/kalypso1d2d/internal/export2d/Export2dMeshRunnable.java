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
package org.kalypso.kalypso1d2d.internal.export2d;

import java.io.File;
import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.conv.I2DMeshConverter;
import org.kalypso.kalypsomodel1d2d.conv.MeshConverterFactory;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.geolog.GeoLog;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCollection;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Thomas Jung
 */
final class Export2dMeshRunnable implements ICoreRunnableWithProgress
{
  private final File m_exportFile;

  private final String m_selectedExtension;

  private final boolean m_exportRoughness;

  private final boolean m_exportMiddleNodes;

  public Export2dMeshRunnable( final File exportFile, final String selectedExtension, final boolean exportRoughness, final boolean exportMiddleNodes )
  {
    m_exportFile = exportFile;
    m_selectedExtension = selectedExtension;
    m_exportRoughness = exportRoughness;
    m_exportMiddleNodes = exportMiddleNodes;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException, InvocationTargetException
  {
    monitor.beginTask( Messages.getString( "org.kalypso.wizards.export2d.Export2dMeshRunnable.0" ), IProgressMonitor.UNKNOWN ); //$NON-NLS-1$

    final IScenarioDataProvider dataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
    final IFEDiscretisationModel1d2d discretisationModel = dataProvider.getModel( IFEDiscretisationModel1d2d.class.getName() );
    final IFlowRelationshipModel flowRelationshipModel = dataProvider.getModel( IFlowRelationshipModel.class.getName() );
    final IRoughnessClsCollection roughnessModel = m_exportRoughness ? (IRoughnessClsCollection)dataProvider.getModel( IRoughnessClsCollection.class.getName() ) : null;

    try
    {
      final I2DMeshConverter converter = MeshConverterFactory.getConverter( discretisationModel, flowRelationshipModel, null, roughnessModel, null, true, m_exportMiddleNodes, new GeoLog( null ), m_selectedExtension );
      converter.writeMesh( m_exportFile );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new InvocationTargetException( e );
    }

    monitor.done();

    return Status.OK_STATUS;
  }
}
