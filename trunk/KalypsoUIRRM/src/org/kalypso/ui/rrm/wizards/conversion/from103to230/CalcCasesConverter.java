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
package org.kalypso.ui.rrm.wizards.conversion.from103to230;

import java.io.File;
import java.io.IOException;

import org.apache.commons.io.FileUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.model.hydrology.project.INaProjectConstants;
import org.kalypso.module.conversion.AbstractLoggingOperation;

/**
 * @author Gernot Belger
 */
public class CalcCasesConverter extends AbstractLoggingOperation
{
  private final File m_sourceDir;

  private final File m_targetDir;

  public CalcCasesConverter( final File sourceDir, final File targetDir )
  {
    super( "Rechenvarianten" );
    m_sourceDir = sourceDir;
    m_targetDir = targetDir;
  }

  /**
   * @see org.kalypso.ui.rrm.wizards.conversion.AbstractLoggingOperation#doExecute(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  protected void doExecute( final IProgressMonitor monitor ) throws CoreException, InterruptedException
  {
    try
    {
      final SubMonitor progress = SubMonitor.convert( monitor, "Konvertiere Rechenvarianten", 101 );

      final CalcCaseConvertWalker walker = new CalcCaseConvertWalker( m_sourceDir );
      final File[] calcCases = walker.execute();
      ProgressUtilities.worked( progress, 1 );

      progress.setWorkRemaining( calcCases.length );

      for( int i = 0; i < calcCases.length; i++ )
      {
        final File sourceDir = calcCases[i];
        final String calcCaseName = sourceDir.getName();

        progress.subTask( String.format( "Rechenvariante %s (%d/%d)...", calcCaseName, i + 1, calcCases.length ) );
        final File targetDir = determineTargetDir( sourceDir );

        try
        {
          prepareCalcCase( targetDir );

          final CalcCaseConverter calcCaseConverter = new CalcCaseConverter( sourceDir, targetDir );
          calcCaseConverter.doExecute( progress.newChild( 1 ) );
        }
        catch( final CoreException ce )
        {
          final IStatus status = ce.getStatus();
          if( status.matches( IStatus.CANCEL ) )
            throw new InterruptedException( "Abbruch durch Benutzer" );

          getLog().add( status );
        }
        catch( final Throwable e )
        {
          getLog().addError( "Unerwarteter Fehler beim Konvertieren von Rechenvariante '%s'", e, calcCaseName );
        }
      }
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      getLog().addError( "Fehler beim Zugriff auf die Rechenvarianten", e );
    }
  }

  /** Copy calc-case template of target */
  private void prepareCalcCase( final File calcCaseDir ) throws IOException
  {
    final File calcCaseTemplateDir = new File( m_targetDir, INaProjectConstants.CALC_CASE_TEMPLATE_DIR );
    FileUtils.copyDirectory( calcCaseTemplateDir, calcCaseDir );
  }

  private File determineTargetDir( final File directory )
  {
    final IPath basePath = Path.fromOSString( m_sourceDir.getAbsolutePath() );
    final IPath calcCaseSourcePath = basePath.append( INaProjectConstants.FOLDER_RECHENVARIANTEN );
    final IPath currentPath = Path.fromOSString( directory.getAbsolutePath() );

    // If we are inside the 'Rechenvarianten', keep this relative order.
    // If we are outside, we move it into 'rechenvariante/Andere' in the target.
    IPath targetRelativePath;
    if( calcCaseSourcePath.isPrefixOf( currentPath ) )
      targetRelativePath = currentPath.makeRelativeTo( calcCaseSourcePath );
    else
      targetRelativePath = new Path( "Andere" ).append( calcCaseSourcePath.makeRelativeTo( basePath ) );

    final IPath calcCaseTargetPath = new Path( m_targetDir.getAbsolutePath() ).append( INaProjectConstants.FOLDER_RECHENVARIANTEN );
    return calcCaseTargetPath.append( targetRelativePath ).toFile();
  }

}
