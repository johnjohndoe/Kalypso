/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.ui.rrm.wizards.conversion.to10_10;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.util.regex.Pattern;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOCase;
import org.apache.commons.io.filefilter.SuffixFileFilter;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.java.io.visitor.FileFilterVisitor;
import org.kalypso.model.hydrology.project.INaProjectConstants;
import org.kalypso.module.conversion.AbstractLoggingOperation;
import org.kalypso.ui.rrm.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.i18n.Messages;
import org.kalypsodeegree.xml.XMLTools;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;

/**
 * @author Gernot Belger
 */
public class BasicModelConverter extends AbstractLoggingOperation
{
  private static final Pattern PATTERN_LINK_MODEL_ZEITREIHEN = Pattern.compile( "project:/.model/(?i)Zeitreihen/" );

  private final File m_sourceDir;

  private final File m_targetDir;

  private final IStatusCollector m_log = new StatusCollector( KalypsoUIRRMPlugin.getID() );

  public BasicModelConverter( final File sourceDir, final File targetDir )
  {
    super( Messages.getString("BasicModelConverter_1") ); //$NON-NLS-1$

    m_sourceDir = sourceDir;
    m_targetDir = targetDir;
  }

  /**
   * @see org.kalypso.ui.rrm.wizards.conversion.AbstractLoggingOperation#doExecute(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  protected void doExecute( final IProgressMonitor monitor ) throws IOException, CoreException
  {
    try
    {
      /* Copy basic .gml files */
      copyFile( INaProjectConstants.GML_MODELL_PATH );
      copyFile( INaProjectConstants.GML_HYDROTOP_PATH );
      copyFile( INaProjectConstants.GML_PARAMETER_PATH );
      copySynthFile();

      copyBasicTimeseries();

      copyObservationConf();
    }
    finally
    {
      monitor.done();
    }
  }

  /**
   * Copies the synth.gml file. Different kalypso version had different names for that file or even ignored it
   * completely.
   */
  private void copySynthFile( ) throws IOException
  {
    final File synthSourceFile = new File( m_sourceDir, "synthN.gml" ); //$NON-NLS-1$
    if( synthSourceFile.exists() )
    {
      copyFile( "synthN.gml", INaProjectConstants.GML_SYNTH_N_PATH ); //$NON-NLS-1$
      return;
    }

    final File calcSynthSourceFile = new File( m_sourceDir, INaProjectConstants.GML_SYNTH_N_PATH );
    if( calcSynthSourceFile.exists() )
    {
      copyFile( INaProjectConstants.GML_SYNTH_N_PATH );
      return;
    }

    /* Some versions/project did not have any synth-file: just show warning */
    getLog().add( IStatus.WARNING, Messages.getString("BasicModelConverter_2") ); //$NON-NLS-1$
  }

  /**
   * Copy observationConf and changes the timeseries links inside.
   */
  private void copyObservationConf( ) throws IOException
  {
    /* copy observationConfig */
    final File sourceDir = new File( m_sourceDir, INaProjectConstants.FOLDER_OBSERVATION_CONF );
    final File targetDir = new File( m_targetDir, INaProjectConstants.FOLDER_OBSERVATION_CONF );
    FileUtils.copyDirectory( sourceDir, targetDir, true );

    /* Find all gml files here */
    final FileFilter gmlFileFilter = new SuffixFileFilter( ".gml", IOCase.INSENSITIVE ); //$NON-NLS-1$
    final FileFilterVisitor gmlFileFinder = new FileFilterVisitor( gmlFileFilter, true, true );
    FileUtilities.accept( targetDir, gmlFileFinder, true );

    /* Change zml link to new location */
    final File[] result = gmlFileFinder.getResult();
    for( final File file : result )
    {
      try
      {
        fixObservationConfTimeseriesLinks( file );
      }
      catch( final SAXException e )
      {
        m_log.add( IStatus.ERROR, Messages.getString( "BasicModelConverter_4" ), e, file.getName() ); //$NON-NLS-1$
      }
    }
  }

  private void fixObservationConfTimeseriesLinks( final File observationConfFile ) throws SAXException, IOException
  {
    // FIXME: we read the whole document only to find the encoding; is there no other way?
    final Document document = XMLTools.parse( observationConfFile );
    final String encoding = document.getInputEncoding();
    final String fileContents = FileUtils.readFileToString( observationConfFile, encoding );

    final String newFileContents = PATTERN_LINK_MODEL_ZEITREIHEN.matcher( fileContents ).replaceAll( "project:/Zeitreihen/" );

    FileUtils.writeStringToFile( observationConfFile, newFileContents, encoding );
  }

  private void copyBasicTimeseries( ) throws CoreException, IOException
  {
    final File sourceTimeseriesDir = findSourceTimeseriesDir();
    final File targetTimeseriesDir = new File( m_targetDir, INaProjectConstants.FOLDER_ZEITREIHEN );
    if( !sourceTimeseriesDir.isDirectory() )
    {
      final String relativePath = FileUtilities.getRelativePathTo( m_sourceDir, sourceTimeseriesDir );
      final String msg = String.format( Messages.getString("BasicModelConverter_4"), relativePath ); //$NON-NLS-1$
      final IStatus error = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), msg );
      throw new CoreException( error );
      // final File sourceOtherTimeseriesDir = new File( m_sourceDir, INaProjectConstants.FOLDER_ZEITREIHEN );
      // TODO: vielleicht stattdessen nur log-warnung und noch mal prüfen, ob es der User nicht selbst verschoben hat.
    }

    FileUtils.copyDirectory( sourceTimeseriesDir, targetTimeseriesDir, true );
  }

  /**
   * Different model version have this directory hat different places.
   */
  private File findSourceTimeseriesDir( )
  {
    final File sourceModelDir = new File( m_sourceDir, ".model" ); //$NON-NLS-1$
    final File sourceTimeseriesDir = new File( sourceModelDir, "Zeitreihen" );
    if( sourceTimeseriesDir.isDirectory() )
      return sourceTimeseriesDir;

    return new File( m_sourceDir, INaProjectConstants.FOLDER_ZEITREIHEN );
  }

  private void copyFile( final String path ) throws IOException
  {
    copyFile( path, path );
  }

  private void copyFile( final String sourcePath, final String targetPath ) throws IOException
  {
    final File modelSourceFile = new File( m_sourceDir, sourcePath );
    final File modelTargetFile = new File( m_targetDir, targetPath );

    FileUtils.copyFile( modelSourceFile, modelTargetFile, true );
  }

}
