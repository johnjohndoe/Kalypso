/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.tuhh.ui.imports.ctripple;

import java.io.File;

import org.apache.commons.io.FilenameUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.wspm.tuhh.core.ctripple.CodedTripple;
import org.kalypso.model.wspm.tuhh.core.ctripple.CodedTrippleHorizonMapper;
import org.kalypso.model.wspm.tuhh.core.ctripple.CodedTrippleReader;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;

/**
 * @author Holger Albert
 */
public class CodedTrippleCreateProfilesOperation implements ICoreRunnableWithProgress
{
  private final CodedTrippleImportData m_data;

  public CodedTrippleCreateProfilesOperation( final CodedTrippleImportData data )
  {
    m_data = data;
  }

  @Override
  public IStatus execute( IProgressMonitor monitor )
  {
    /* Monitor. */
    if( monitor == null )
      monitor = new NullProgressMonitor();

    try
    {
      /* Monitor. */
      monitor.beginTask( Messages.getString( "CodedTrippleCreateProfilesOperation.0" ), 1000 ); //$NON-NLS-1$
      monitor.subTask( Messages.getString( "CodedTrippleCreateProfilesOperation.1" ) ); //$NON-NLS-1$

      /* Get the source file. */
      final File sourceFile = m_data.getSourceFile().getFile();

      /* Create the mapping files. */
      final String codeToHorizonIdFilename = String.format( "%s_CodeToHorizonId.properties", FilenameUtils.getBaseName( sourceFile.getName() ) ); //$NON-NLS-1$
      final String horizonIdToPartIdFilename = String.format( "%s_HorizonIdToPartId.properties", FilenameUtils.getBaseName( sourceFile.getName() ) ); //$NON-NLS-1$
      final String codeToCodeDescriptionFilename = String.format( "%s_codeToCodeDescription.properties", FilenameUtils.getBaseName( sourceFile.getName() ) ); //$NON-NLS-1$
      final String horizonIdToHorizonIdDescriptionFilename = String.format( "%s_horizonIdToHorizonIdDescription.properties", FilenameUtils.getBaseName( sourceFile.getName() ) ); //$NON-NLS-1$
      final File codeToHorizonIdFile = new File( sourceFile.getParentFile(), codeToHorizonIdFilename );
      final File horizonIdToPartIdFile = new File( sourceFile.getParentFile(), horizonIdToPartIdFilename );
      final File codeToCodeDescriptionFile = new File( sourceFile.getParentFile(), codeToCodeDescriptionFilename );
      final File horizonIdToHorizonIdDescriptionFile = new File( sourceFile.getParentFile(), horizonIdToHorizonIdDescriptionFilename );

      /* Create the coded tripple horizon mapper. */
      final CodedTrippleHorizonMapper mapper = new CodedTrippleHorizonMapper();
      mapper.loadIdMappings( codeToHorizonIdFile, horizonIdToPartIdFile );
      mapper.loadDescriptionMappings( codeToCodeDescriptionFile, horizonIdToHorizonIdDescriptionFile );

      /* Read the source file. */
      final CodedTrippleReader reader = new CodedTrippleReader();
      reader.read( sourceFile, mapper );

      /* Monitor. */
      monitor.worked( 500 );

      /* Get the coded tripple data object. */
      final CodedTripple data = reader.getCodedTripple();

      /* Create a status collector. */
      final IStatusCollector collector = new StatusCollector( KalypsoModelWspmTuhhUIPlugin.getID() );

      /* Get the bad codes. */
      final String[] badCodes = data.getBadCodes();
      if( badCodes.length > 0 )
      {
        for( final String badCode : badCodes )
          collector.add( new Status( IStatus.WARNING, KalypsoModelWspmTuhhUIPlugin.getID(), String.format( Messages.getString( "CodedTrippleCreateProfilesOperation.3" ), badCode ) ) ); //$NON-NLS-1$
      }

      /* Create the status. */
      final IStatus status = collector.asMultiStatus( Messages.getString( "CodedTrippleCreateProfilesOperation.2" ) ); //$NON-NLS-1$

      /* Store the coded tripple data object. */
      m_data.setCodedTrippleData( data );
      m_data.setCodedTrippleDataStatus( status );

      /* Monitor. */
      monitor.worked( 500 );

      return status;
    }
    catch( final Exception ex )
    {
      /* Create a error status. */
      final Status errorStatus = new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getID(), ex.getLocalizedMessage(), ex );

      /* Store the coded tripple data object. */
      m_data.setCodedTrippleData( null );
      m_data.setCodedTrippleDataStatus( errorStatus );

      return errorStatus;
    }
    finally
    {
      /* Monitor. */
      monitor.done();
    }
  }
}