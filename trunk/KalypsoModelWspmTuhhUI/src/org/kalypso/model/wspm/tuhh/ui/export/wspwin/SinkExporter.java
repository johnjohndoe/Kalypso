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
package org.kalypso.model.wspm.tuhh.ui.export.wspwin;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.model.wspm.core.KalypsoModelWspmCorePlugin;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.serializer.IProfileSink;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class SinkExporter
{
  private final IProfileSink m_sink;

  public SinkExporter( final IProfileSink sink )
  {
    m_sink = sink;
  }

  public void export( final IProfileFeature[] profiles, final File file, final IProgressMonitor monitor ) throws CoreException
  {
    try
    {
      writeProfiles( profiles, file, monitor );
    }
    catch( final IOException e )
    {
      final String message = String.format( Messages.getString( "SinkExporter_0" ) ); //$NON-NLS-1$
      final IStatus status = new Status( IStatus.ERROR, KalypsoModelWspmCorePlugin.getID(), message, e );
      throw new CoreException( status );
    }
    finally
    {
      monitor.done();
    }
  }

  private void writeProfiles( final IProfileFeature[] profiles, final File file, final IProgressMonitor monitor ) throws IOException
  {
    OutputStream os = null;
    try
    {
      os = new BufferedOutputStream( new FileOutputStream( file ) );
      writeProfiles( profiles, os, monitor );
      os.close();
    }
    finally
    {
      IOUtils.closeQuietly( os );
    }
  }

  private void writeProfiles( final IProfileFeature[] profileFeatures, final OutputStream os, final IProgressMonitor monitor ) throws IOException
  {
    monitor.beginTask( Messages.getString( "SinkExporter_1" ), profileFeatures.length ); //$NON-NLS-1$

    final IProfile[] profiles = ProfileFeatureSorter.extractProfiles( profileFeatures, monitor );
// new IProfil[profileFeatures.length];
// for( int i = 0; i < profiles.length; i++ )
// {
// profiles[i] = profileFeatures[i].getProfil();
// ProgressUtilities.worked( monitor, 1 );
// }

    // FIXME: what encoding?
    final OutputStreamWriter writer = new OutputStreamWriter( os );
    m_sink.write( profiles, writer );
    writer.flush();

    monitor.done();
  }

}
