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
package org.kalypso.model.wspm.tuhh.ui.imports.ewawi;

import java.io.File;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.wspm.ewawi.data.EwawiPlus;
import org.kalypso.model.wspm.ewawi.data.reader.EwawiProReader;
import org.kalypso.model.wspm.ewawi.data.reader.EwawiStaReader;
import org.kalypso.model.wspm.ewawi.utils.EwawiKey;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;

/**
 * @author Holger Albert
 */
public class EwawiCreateProfilesOperation implements ICoreRunnableWithProgress
{
  private final EwawiImportData m_data;

  public EwawiCreateProfilesOperation( final EwawiImportData data )
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
      monitor.beginTask( Messages.getString("EwawiCreateProfilesOperation.0"), 1000 ); //$NON-NLS-1$
      monitor.subTask( Messages.getString("EwawiCreateProfilesOperation.1") ); //$NON-NLS-1$

      /* Create the ewawi data object. */
      final EwawiKey key = new EwawiKey( "", "ALIAS", "", "" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
      final EwawiPlus data = new EwawiPlus( key );

      /* Read the .pro file. */
      final EwawiProReader proReader = new EwawiProReader( data );
      final File propFile = m_data.getProFile().getFile();
      proReader.read( propFile );

      /* Monitor. */
      monitor.worked( 500 );

      /* Read the .sta file. */
      final EwawiStaReader staReader = new EwawiStaReader( data );
      final File staFile = m_data.getStaFile().getFile();
      staReader.read( staFile );

      /* Create a ok status. */
      final Status okStatus = new Status( IStatus.OK, KalypsoModelWspmTuhhUIPlugin.getID(), Messages.getString("EwawiCreateProfilesOperation.2") ); //$NON-NLS-1$

      /* Store the ewawi data object. */
      m_data.setEwawiData( data );
      m_data.setEwawiDataStatus( okStatus );

      /* Monitor. */
      monitor.worked( 500 );

      return okStatus;
    }
    catch( final Exception ex )
    {
      /* Create a error status. */
      final Status errorStatus = new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getID(), ex.getLocalizedMessage(), ex );

      /* Store the ewawi data object. */
      m_data.setEwawiData( null );
      m_data.setEwawiDataStatus( errorStatus );

      return errorStatus;
    }
    finally
    {
      /* Monitor. */
      monitor.done();
    }
  }
}