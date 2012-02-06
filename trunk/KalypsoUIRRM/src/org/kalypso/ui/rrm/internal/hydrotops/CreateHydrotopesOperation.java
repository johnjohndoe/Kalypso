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

import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.model.hydrology.binding.IHydrotope;
import org.kalypso.model.hydrology.operation.hydrotope.HydrotopeCreationOperation;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author "Gernot Belger"
 */
public final class CreateHydrotopesOperation implements ICoreRunnableWithProgress
{
  private final GMLWorkspace m_workspace;

  private final IFile m_outputFile;

  private final FeatureList m_fflCatchment;

  private final IFeatureBindingCollection<IHydrotope> m_fflHydrotops;

  private final FeatureList m_fflPedology;

  private final FeatureList m_fflGeology;

  private final FeatureList m_fflLanduse;

  public CreateHydrotopesOperation( final GMLWorkspace workspace, final IFile outputFile, final FeatureList fflCatchment, final IFeatureBindingCollection<IHydrotope> fflHydrotops, final FeatureList fflPedology, final FeatureList fflGeology, final FeatureList fflLanduse )
  {
    m_workspace = workspace;
    m_outputFile = outputFile;
    m_fflCatchment = fflCatchment;
    m_fflHydrotops = fflHydrotops;
    m_fflPedology = fflPedology;
    m_fflGeology = fflGeology;
    m_fflLanduse = fflLanduse;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    try
    {
      final HydrotopeCreationOperation op = new HydrotopeCreationOperation( m_fflLanduse, m_fflPedology, m_fflGeology, m_fflCatchment, m_fflHydrotops, m_workspace, null );
      op.setDissolveMode( true );

      final SubMonitor progress = SubMonitor.convert( monitor, Messages.getString( "org.kalypso.ui.rrm.internal.hydrotops.RRMCreateHydrotopsHandler.7" ), 1000 ); //$NON-NLS-1$

      op.run( progress.newChild( 900, SubMonitor.SUPPRESS_BEGINTASK ) );

      if( monitor.isCanceled() )
        return Status.OK_STATUS;

      /* Save hydrotope file */
      monitor.subTask( Messages.getString( "org.kalypso.ui.rrm.internal.hydrotops.RRMCreateHydrotopsHandler.8" ) ); //$NON-NLS-1$
      final File file = m_outputFile.getLocation().toFile();
      GmlSerializer.serializeWorkspace( file, m_workspace, "UTF-8" ); //$NON-NLS-1$
      ProgressUtilities.worked( progress, 90 );
      m_outputFile.refreshLocal( IResource.DEPTH_ZERO, progress.newChild( 10 ) );
    }
    catch( final InvocationTargetException ite )
    {
      final Throwable targetException = ite.getTargetException();
      if( targetException instanceof CoreException )
        return ((CoreException) targetException).getStatus();

      return StatusUtilities.createStatus( IStatus.ERROR, targetException.getLocalizedMessage(), targetException );
    }
    catch( final IOException e )
    {
      return StatusUtilities.createStatus( IStatus.ERROR, Messages.getString( "org.kalypso.ui.rrm.internal.hydrotops.RRMCreateHydrotopsHandler.10" ), e ); //$NON-NLS-1$
    }
    catch( final GmlSerializeException e )
    {
      return StatusUtilities.createStatus( IStatus.ERROR, Messages.getString( "org.kalypso.ui.rrm.internal.hydrotops.RRMCreateHydrotopsHandler.10" ), e ); //$NON-NLS-1$
    }
    return Status.OK_STATUS;
  }
}