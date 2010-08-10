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
package org.kalypso.model.hydrology.internal.preprocessing;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.Date;
import java.util.logging.Logger;

import org.apache.commons.io.FileUtils;
import org.eclipse.core.runtime.OperationCanceledException;
import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.convert.namodel.NAModellConverter;
import org.kalypso.convert.namodel.NaSimulationData;
import org.kalypso.convert.namodel.manager.HydroHash;
import org.kalypso.convert.namodel.manager.IDManager;
import org.kalypso.convert.namodel.manager.LzsimManager;
import org.kalypso.model.hydrology.binding.NAControl;
import org.kalypso.model.hydrology.binding.NAModellControl;
import org.kalypso.model.hydrology.internal.NaAsciiDirs;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * Converts KalypsoHydrology gml files to Kalypso-NA ascii files.
 * 
 * @author Gernot Belger
 */
public class NAModelPreprocessor
{
  private File m_preprocessedAsciiDir;

  private final IDManager m_idManager;

  private final NaSimulationData m_simulationData;

  private final NAConfiguration m_conf;

  private final Logger m_logger;

  private final NaAsciiDirs m_asciiDirs;

  public NAModelPreprocessor( final NAConfiguration conf, final NaAsciiDirs asciiDirs, final IDManager idManager, final NaSimulationData simulationData, final Logger logger )
  {
    m_conf = conf;
    m_asciiDirs = asciiDirs;
    m_idManager = idManager;
    m_simulationData = simulationData;
    m_logger = logger;
  }

  /**
   * Sets the directory of preprocessed ASCII files.<br>
   * Optional: must be called before {@link #process(ISimulationMonitor)}.<br/>
   * If this directory is set, ASCII files in this directory will be used instead of recreating them from gml.<br/>
   * Original comment:<br>
   * While optimization, you can recycle files from a former run. implement here to copy the files to your tmp dir and
   * while generating files you should check if files already exist, and on your option do not generate them.<br/>
   * WARNING: never use result files or files that vary during optimization.<br/>
   */
  public void setPreprocessedFilesDir( final File preprocessedAsciiDir )
  {
    m_preprocessedAsciiDir = preprocessedAsciiDir;
  }

  public void process( final ISimulationMonitor monitor ) throws NAPreprocessorException, OperationCanceledException
  {
    try
    {
      doProcess( monitor );
    }
    catch( final Exception e )
    {
      // Handle only unexpected exceptions here. Everything else should be handling deeper down!
      e.printStackTrace();
      throw new NAPreprocessorException( "Unexpected error while generating Kalypso-NA ASCII files", e );
    }
  }

  // FIXME: improve error handling! Do not throw generic Exception!
  private void doProcess( final ISimulationMonitor monitor ) throws Exception
  {
    final NAModellControl naControl = m_simulationData.getNaControl();
    final GMLWorkspace modelWorkspace = m_simulationData.getModelWorkspace();
    final NAControl metaControl = m_simulationData.getMetaControl();
    final GMLWorkspace sudsWorkspace = m_simulationData.getSudsWorkspace();

    final String rootNodeID = naControl.getRootNodeID();
    final boolean useResults = naControl.isUseResults();

    monitor.setMessage( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.15" ) ); //$NON-NLS-1$
    checkCancel( monitor );

    copyPreprocessedDirs();
    checkCancel( monitor );

    monitor.setMessage( "Adding additional virtual channels" );
    tweakGmlModel( modelWorkspace, rootNodeID, useResults );
    checkCancel( monitor );

    monitor.setMessage( "Writing control files for Kalypso-NA" );
    final NAControlConverter naControlConverter = new NAControlConverter( metaControl, m_asciiDirs.startDir );
    naControlConverter.writeFalstart();
    naControlConverter.writeStartFile( naControl, modelWorkspace, sudsWorkspace, m_idManager );
    checkCancel( monitor );

    // write net and so on....
    monitor.setMessage( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.23" ) ); //$NON-NLS-1$
    new NAModellConverter( m_conf, rootNodeID, m_logger ).write();
    checkCancel( monitor );

    // Write start conditions, shouldn't this go into the general ascii writer?
    writeStartCondition();
    checkCancel( monitor );

    // Create "out_we.nat", else Kalypso-NA will not run
    m_asciiDirs.outWeNatDir.mkdirs();
  }

  /**
   * Changes the modell-workspace before it is really written into ascii files.
   */
  private void tweakGmlModel( final GMLWorkspace modelWorkspace, final String rootNodeID, final boolean useResults ) throws Exception
  {
    final URL zmlContext = m_conf.getZMLContext();

    final NaNodeResultProvider nodeResultProvider = new NaNodeResultProvider( modelWorkspace, useResults, rootNodeID, zmlContext );
    final NaModelTweaker naModelTweaker = new NaModelTweaker( modelWorkspace, nodeResultProvider );
    naModelTweaker.tweakModel();
  }

  private void checkCancel( final ISimulationMonitor monitor )
  {
    if( monitor.isCanceled() )
      throw new OperationCanceledException();
  }

  /* During optimization, use previously processed files to improve performance */
  // TODO: check if we can replace this by a more general caching mechanism based on the file-date of the input gml
  // file.
  private void copyPreprocessedDirs( ) throws NAPreprocessorException
  {
    try
    {
      if( m_preprocessedAsciiDir == null )
        return;

      final NaAsciiDirs inputAsciiDirs = new NaAsciiDirs( m_preprocessedAsciiDir );

      if( inputAsciiDirs.klimaDatDir.exists() )
        FileUtils.copyDirectory( inputAsciiDirs.klimaDatDir, m_asciiDirs.asciiDir );

      if( inputAsciiDirs.zuflussDir.exists() )
        FileUtils.copyDirectory( inputAsciiDirs.zuflussDir, m_asciiDirs.asciiDir );

      if( inputAsciiDirs.hydroTopDir.exists() )
        FileUtils.copyDirectory( inputAsciiDirs.hydroTopDir, m_asciiDirs.asciiDir );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      throw new NAPreprocessorException( "Failed to copy preprocessed ascii files", e );
    }
  }

  private void writeStartCondition( ) throws NAPreprocessorException
  {
    final GMLWorkspace lzsimWorkspace = m_simulationData.getLzsimWorkspace();
    if( lzsimWorkspace == null )
    {
      final Date simulationStart = m_simulationData.getMetaControl().getSimulationStart();
      final String msg = Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.26", simulationStart ); //$NON-NLS-1$
      m_logger.info( msg );
      return;
    }

    try
    {
      final HydroHash hydroHash = m_conf.getHydroHash();
      LzsimManager.writeLzsimFiles( m_idManager, hydroHash, m_asciiDirs.lzsimDir, lzsimWorkspace );
    }
    catch( final Exception e )
    {
      throw new NAPreprocessorException( "Failed to write start condition", e );
    }
  }
}
