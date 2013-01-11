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

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.tuhh.core.ctripple.CodedTripple;
import org.kalypso.model.wspm.tuhh.core.ctripple.CodedTrippleProfile;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;

/**
 * @author Holger Albert
 */
public class CodedTrippleImportOperation implements ICoreRunnableWithProgress
{
  private final AbstractCodedTrippleWorker m_worker;

  private final CodedTrippleImportData m_data;

  public CodedTrippleImportOperation( AbstractCodedTrippleWorker worker, CodedTrippleImportData data )
  {
    m_worker = worker;
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
      monitor.beginTask( Messages.getString( "CodedTrippleImportOperation.0" ), 700 ); //$NON-NLS-1$
      monitor.subTask( Messages.getString( "CodedTrippleImportOperation.1" ) ); //$NON-NLS-1$

      /* Update the classifications. */
      m_worker.updateClassifications( m_data );

      /* Monitor. */
      monitor.worked( 100 );
      monitor.subTask( Messages.getString( "CodedTrippleImportOperation.2" ) ); //$NON-NLS-1$

      /* Get the coded tripple data object. */
      CodedTripple data = m_data.getCodedTrippleData();

      /* Get the coded tripple profiles. */
      CodedTrippleProfile[] profiles = data.getProfiles();
      for( final CodedTrippleProfile profile : profiles )
      {
        /* Create the wspm profile. */
        final IProfileFeature newProfile = m_worker.createNewProfile( m_data, profile );
        m_worker.createMarkers( newProfile );

        /* Monitor. */
        monitor.worked( 500 / profiles.length );
      }

      /* Monitor. */
      monitor.subTask( Messages.getString( "CodedTrippleImportOperation.3" ) ); //$NON-NLS-1$

      /* Fire change events. */
      m_worker.fireChangeEvents();

      /* Monitor. */
      monitor.worked( 100 );

      return new Status( IStatus.OK, KalypsoModelWspmTuhhUIPlugin.getID(), Messages.getString( "CodedTrippleImportOperation.4" ) ); //$NON-NLS-1$
    }
    catch( final Exception ex )
    {
      return new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getID(), ex.getLocalizedMessage(), ex );
    }
    finally
    {
      /* Monitor. */
      monitor.done();
    }
  }
}