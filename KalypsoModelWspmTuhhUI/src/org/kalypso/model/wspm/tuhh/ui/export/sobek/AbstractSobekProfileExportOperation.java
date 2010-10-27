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
package org.kalypso.model.wspm.tuhh.ui.export.sobek;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Formatter;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.model.wspm.core.KalypsoModelWspmCorePlugin;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;

/**
 * @author Gernot Belger
 */
public abstract class AbstractSobekProfileExportOperation implements ISobekProfileExportOperation
{
  private final File m_targetFile;

  private final IProfileFeature[] m_profiles;

  private Formatter m_formatter;

  private final Collection<IStatus> m_stati = new ArrayList<IStatus>();

  public AbstractSobekProfileExportOperation( final File targetFile, final IProfileFeature[] profilesToExport )
  {
    m_targetFile = targetFile;
    m_profiles = profilesToExport;
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress#execute(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException
  {
    try
    {
      monitor.beginTask( "Profile schreiben", m_profiles.length );

      m_formatter = new Formatter( m_targetFile );

      for( final IProfileFeature profil : m_profiles )
      {
        monitor.subTask( String.format( "%s (%s)", profil.getName(), profil.getBigStation() ) );

        writeProfile( m_formatter, profil.getProfil() );
        ProgressUtilities.worked( monitor, 1 );
      }
      close();

      return getStatus();
    }
    catch( final IOException e )
    {
      final String message = String.format( "Failed to write profiles" );
      final IStatus status = new Status( IStatus.ERROR, KalypsoModelWspmCorePlugin.getID(), message, e );
      throw new CoreException( status );
    }
    finally
    {
      if( m_formatter != null )
        m_formatter.close();
      monitor.done();
    }
  }

  protected abstract void writeProfile( Formatter formatter, IProfil profil );

  protected void add( final IStatus status )
  {
    m_stati.add( status );
  }

  protected void close( ) throws IOException
  {
    m_formatter.flush();
    checkIO();
    m_formatter.close();
    checkIO();
  }

  private void checkIO( ) throws IOException
  {
    final IOException ioException = m_formatter.ioException();
    if( ioException != null )
      throw ioException;
  }

  private IStatus getStatus( )
  {
    final IStatus[] children = m_stati.toArray( new IStatus[m_stati.size()] );
    if( children.length > 0 )
      return new MultiStatus( KalypsoModelWspmTuhhUIPlugin.getID(), -1, children, "Es sind Probleme beim SOBEK-Export aufgetreten", null );
    return Status.OK_STATUS;
  }

}
