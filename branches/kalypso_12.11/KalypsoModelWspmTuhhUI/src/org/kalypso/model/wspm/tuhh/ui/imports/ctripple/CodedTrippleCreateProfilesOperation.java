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

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.wspm.tuhh.core.ctripple.CodedTripple;
import org.kalypso.model.wspm.tuhh.core.ctripple.CodedTrippleReader;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;

/**
 * @author Holger Albert
 */
public class CodedTrippleCreateProfilesOperation implements ICoreRunnableWithProgress
{
  private final CodedTrippleImportData m_data;

  public CodedTrippleCreateProfilesOperation( CodedTrippleImportData data )
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

      /* Read the source file. */
      CodedTrippleReader reader = new CodedTrippleReader();
      File sourceFile = m_data.getSourceFile().getFile();
      reader.read( sourceFile );

      /* Monitor. */
      monitor.worked( 500 );

      /* Get the coded tripple data object. */
      CodedTripple data = reader.getCodedTrippleData();

      /* Create a ok status. */
      final IStatus okStatus = new Status( IStatus.OK, KalypsoModelWspmTuhhUIPlugin.getID(), Messages.getString( "CodedTrippleCreateProfilesOperation.2" ) ); //$NON-NLS-1$

      /* Store the coded tripple data object. */
      m_data.setCodedTrippleData( data );
      m_data.setCodedTrippleDataStatus( okStatus );

      /* Monitor. */
      monitor.worked( 500 );

      return okStatus;
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