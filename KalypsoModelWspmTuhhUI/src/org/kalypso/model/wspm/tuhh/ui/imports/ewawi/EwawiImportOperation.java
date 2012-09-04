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
import org.kalypso.model.wspm.ewawi.utils.GewShape;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;

/**
 * @author Holger Albert
 */
public class EwawiImportOperation implements ICoreRunnableWithProgress
{
  private final CommandableWorkspace m_workspace;

  private final TuhhWspmProject m_targetProject;

  private final EwawiImportData m_data;

  public EwawiImportOperation( final CommandableWorkspace workspace, final TuhhWspmProject targetProject, final EwawiImportData data )
  {
    m_workspace = workspace;
    m_targetProject = targetProject;
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
      monitor.beginTask( "Importing EWAWI+ profiles", 1000 );
      monitor.subTask( "Reading input data..." );

      /* Create the ewawi data object. */
      final EwawiKey key = new EwawiKey( "", "ALIAS", "", "" );
      final EwawiPlus data = new EwawiPlus( key );

      /* Read the .pro file. */
      final EwawiProReader proReader = new EwawiProReader( data );
      final File propFile = m_data.getProFile().getFile();
      proReader.read( propFile );

      /* Read the .sta file. */
      final EwawiStaReader staReader = new EwawiStaReader( data );
      final File staFile = m_data.getStaFile().getFile();
      staReader.read( staFile );

      /* Monitor. */
      monitor.worked( 200 );
      monitor.subTask( "Reading river shape..." );

      /* The river shape. */
      GewShape gewShape = null;

      /* Read the river shape. */
      final File shpFile = m_data.getRiverShapeData().getShpFile().getFile();
      if( shpFile != null )
      {
        gewShape = new GewShape( shpFile );
        gewShape.init();
      }

      /* Monitor. */
      monitor.worked( 200 );
      monitor.subTask( "Creating profiles..." );

      // TODO

      /* Monitor. */
      monitor.worked( 600 );

      return new Status( IStatus.OK, KalypsoModelWspmTuhhUIPlugin.getID(), "OK" );
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