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
package org.kalypso.model.wspm.tuhh.schema.simulation;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.filefilter.FileFilterUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.model.wspm.tuhh.schema.KalypsoModelWspmTuhhSchemaPlugin;

/**
 * Processes the .km result files<br/>
 * Copies all .km files into a subfolder<br>
 * Reads the .km files into a gml format.
 * 
 * @author Gernot Belger
 */
public class KMProcessor
{
  private final File m_inputDir;

  private final File m_kmOutputDir;

  private final File m_outputDir;

  public KMProcessor( final File inputDir, final File kmOutputDir, final File outputDir )
  {
    m_inputDir = inputDir;
    m_kmOutputDir = kmOutputDir;
    m_outputDir = outputDir;
  }

  public void execute( final IProgressMonitor monitor ) throws CoreException
  {
    try
    {
      monitor.beginTask( "Processing .km files", 100 );

      monitor.subTask( "Converting .km files to gml" );
      convertKmToGml();
      ProgressUtilities.worked( monitor, 80 );

      monitor.subTask( "Moving .km files to result folder" );
      moveKmFiles();
      ProgressUtilities.worked( monitor, 20 );
    }
    finally
    {
      ProgressUtilities.done( monitor );
    }
  }

  private void convertKmToGml( )
  {
    /* Read results */
//    log.log( true, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeHelper.13" ) ); //$NON-NLS-1$
    final File targetGmlFile = new File( m_outputDir, "km.gml" ); //$NON-NLS-1$
    try
    {
      // FIXME
// readResults( m_inputDir, targetGmlFile, calculation, log, resultEater );

// final File gmvResultFile = new File( tmpDir, ERGEBNISSE_GMV );
//      final URL ergebnisseGmbLocation = PolynomeHelper.class.getResource( "resources/" + ERGEBNISSE_GMV ); //$NON-NLS-1$
// FileUtils.copyURLToFile( ergebnisseGmbLocation, gmvResultFile );
// resultEater.addResult( WspmTuhhCalcJob.OUTPUT_QINTERVALL_RESULT_GMV, gmvResultFile );
    }
    catch( final Throwable e )
    {
//      log.log( e, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeHelper.14" ), e.getLocalizedMessage() ); //$NON-NLS-1$
//      monitor.setFinishInfo( IStatus.ERROR, Messages.getString( "org.kalypso.model.wspm.tuhh.schema.simulation.PolynomeHelper.15" ) + e.getLocalizedMessage() ); //$NON-NLS-1$
    }
  }

  private void moveKmFiles( ) throws CoreException
  {
    try
    {
      // TODO: we copy also the *.km files (ok, this is not the best target dir), in order
      // to use them for Klinin-Miljukov calculation later.
      // However it would be nice, if the KM import code of KalypsoHydrology could directly read the polynom results (we
      // probably need to add more data to the polnome files as well).
      final FileFilter kmFilter = FileFilterUtils.suffixFileFilter( ".km" ); //$NON-NLS-1$
      final File[] kmFiles = m_inputDir.listFiles( kmFilter );
      for( final File kmFile : kmFiles )
        FileUtils.moveFileToDirectory( kmFile, m_kmOutputDir, true );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, KalypsoModelWspmTuhhSchemaPlugin.getID(), "Failed to move .km files to result folder" );
      throw new CoreException( status );
    }
  }
}