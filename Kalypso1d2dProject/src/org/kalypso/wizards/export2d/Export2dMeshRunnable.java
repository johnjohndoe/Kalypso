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
package org.kalypso.wizards.export2d;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.kalypsomodel1d2d.conv.Gml2RMA10SConv;
import org.kalypso.kalypsomodel1d2d.conv.Gml2SMSConv;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.geolog.GeoLog;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCollection;
import org.kalypso.wizards.i18n.Messages;

/**
 * @author Thomas Jung
 * 
 */
public final class Export2dMeshRunnable implements ICoreRunnableWithProgress
{
  private final File m_exportFile;

  private final String m_selectedExtension;

  private final boolean m_exportRoughness;

  private final boolean m_exportMiddleNodes;

  public Export2dMeshRunnable( File exportFile, String selectedExtension, boolean exportRoughness, boolean exportMiddleNodes )
  {
    m_exportFile = exportFile;
    m_selectedExtension = selectedExtension;
    m_exportRoughness = exportRoughness;
    m_exportMiddleNodes = exportMiddleNodes;
  }

  public IStatus execute( IProgressMonitor monitor ) throws CoreException, InvocationTargetException
  {
    monitor.beginTask( Messages.getString( "org.kalypso.wizards.export2d.Export2dMeshRunnable.0" ), IProgressMonitor.UNKNOWN ); //$NON-NLS-1$

    final SzenarioDataProvider dataProvider = KalypsoAFGUIFrameworkPlugin.getDefault().getDataProvider();
    final IFEDiscretisationModel1d2d discretisationModel = dataProvider.getModel( IFEDiscretisationModel1d2d.class );
    final IFlowRelationshipModel flowRelationshipModel = dataProvider.getModel( IFlowRelationshipModel.class );
    final IRoughnessClsCollection roughnessModel = m_exportRoughness ? dataProvider.getModel( IRoughnessClsCollection.class ) : null;

    // TODO: check which format has been chosen.

    if( m_selectedExtension == "*.2d" ) //$NON-NLS-1$
    {
      final Gml2RMA10SConv converter = new Gml2RMA10SConv( discretisationModel, flowRelationshipModel, null, roughnessModel, null, true, m_exportMiddleNodes, new GeoLog( null ) );

      try
      {
        final OutputStream outputStream = new FileOutputStream( m_exportFile );
        converter.writeRMA10sModel( outputStream );
      }
      catch( IOException e )
      {
        e.printStackTrace();
        throw new InvocationTargetException( e );
      }
    }
    else if( m_selectedExtension == "*.2dm" ) //$NON-NLS-1$
    {
      final Gml2SMSConv converter = new Gml2SMSConv( discretisationModel, roughnessModel );

      try
      {
        converter.writeRMA10sModel( m_exportFile );
      }
      catch( IOException e )
      {
        e.printStackTrace();
        throw new InvocationTargetException( e );
      }

    }

    return Status.OK_STATUS;
  }
}
