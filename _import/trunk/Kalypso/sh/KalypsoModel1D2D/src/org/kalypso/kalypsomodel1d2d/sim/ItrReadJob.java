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
package org.kalypso.kalypsomodel1d2d.sim;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.math.BigDecimal;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author Gernot Belger
 */
public class ItrReadJob extends Job
{
  private final File m_file;

  /** The underlying workspace of the current observation */
  private GMLWorkspace m_workspace;

  /** The current observation of step m_stepNr */
  private IObservation<TupleResult> m_obs = null;

  private final Map<String, IComponent> m_components = new HashMap<String, IComponent>();

  private int m_stepNr = -1;

  private int m_lastLineNumber;

  private final IProgressMonitor m_monitor;

  private final int m_maxNumberOfSteps;

  private final File m_outputDir;

  public ItrReadJob( final String name, final File file, final int maxNumberOfSteps, final File outputDir, final IProgressMonitor monitor )
  {
    super( name );

    m_file = file;
    m_maxNumberOfSteps = maxNumberOfSteps;
    m_outputDir = outputDir;
    m_monitor = monitor;
  }

  public int getStepNr( )
  {
    return m_stepNr;
  }

  /**
   * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  protected IStatus run( final IProgressMonitor monitor )
  {
    monitor.beginTask( getName(), IProgressMonitor.UNKNOWN );

    try
    {
      while( true )
      {
        try
        {
          final int oldStepNr = m_stepNr;

          readFile();

          if( oldStepNr != m_stepNr )
          {
            final String msg = String.format( "RMA10s wird ausgeführt - Schritt %d (%d)", m_stepNr, m_maxNumberOfSteps );
            m_monitor.subTask( msg );
            m_monitor.worked( m_stepNr - oldStepNr );
          }

          Thread.sleep( 1000 );
          ProgressUtilities.worked( monitor, 1 );
        }
        catch( final InterruptedException e )
        {
          e.printStackTrace();
        }
      }
    }
    catch( final CoreException e )
    {
      return e.getStatus();
    }
  }

  public void readFile( )
  {
    if( !m_file.exists() )
      return;

    /* Read file and write outputs */
    LineNumberReader lnr = null;
    try
    {
      lnr = new LineNumberReader( new FileReader( m_file ) );
      while( lnr.ready() )
      {
        final String line = lnr.readLine();
        if( line == null )
          break;

        processLine( line, lnr.getLineNumber() );
      }
    }
    catch( final FileNotFoundException e )
    {
      // TODO Auto-generated catch block
      if( lnr == null )
        StatusUtilities.createStatus( IStatus.WARNING, ISimulation1D2DConstants.CODE_RMA10S, "Iterations-Log konnte nicht geöffnet werden.", e );

      final String msg = String.format( "Fehler in Zeile %d beim Lesen des Iterations-Log konnte nicht gelesen werden.", lnr.getLineNumber() );
      StatusUtilities.createStatus( IStatus.WARNING, ISimulation1D2DConstants.CODE_RMA10S, msg, e );
    }
    catch( final IOException e )
    {
      // TODO Auto-generated catch block
      if( lnr == null )
        StatusUtilities.createStatus( IStatus.WARNING, ISimulation1D2DConstants.CODE_RMA10S, "Iterations-Log konnte nicht geöffnet werden.", e );

      final String msg = String.format( "Fehler in Zeile %d beim Lesen des Iterations-Log konnte nicht gelesen werden.", lnr.getLineNumber() );
      StatusUtilities.createStatus( IStatus.WARNING, ISimulation1D2DConstants.CODE_RMA10S, msg, e );
    }
  }

  private void processLine( final String line, final int lineNumber )
  {
    /* Ignore lines which already have been processed */
    if( lineNumber <= m_lastLineNumber )
      return;

    m_lastLineNumber = lineNumber;

    /* Ignore comment lines */
    if( line.length() > 0 && line.charAt( 0 ) == '#' )
      return;

    final String[] strings = line.trim().split( "\\s+" );
    if( strings.length != 25 )
      return;

    try
    {
      final BigDecimal[] values = new BigDecimal[strings.length];
      for( int i = 0; i < strings.length; i++ )
        values[i] = new BigDecimal( strings[i].trim() );

      final int stepNr = values[1].intValueExact();
      addLine( stepNr, values );

      m_stepNr = stepNr;
    }
    catch( final NumberFormatException e )
    {
      // at the moment ignored, as this always happens for the first line...
      // TODO: change calc core to produce better output
    }

  }

  private void addLine( final int stepNr, final BigDecimal[] values )
  {
    if( stepNr != m_stepNr )
    {
      /* Save old observation */
      if( m_obs != null )
      {
        try
        {
          final Feature obsFeature = m_workspace.getRootFeature();
          ObservationFeatureFactory.toFeature( m_obs, obsFeature );
          final File obsFile = new File( m_outputDir, "Iteration-Step_" + m_stepNr + ".gml" );
          GmlSerializer.serializeWorkspace( obsFile, m_workspace, "UTF-8" );
        }
        catch( final Throwable e )
        {
          // TODO Auto-generated catch block
          e.printStackTrace();
        }
      }

      /* Create new observation */
      try
      {
        final URL obsTemplate = getClass().getResource( "resource/template/iterObs.gml" );
        m_workspace = GmlSerializer.createGMLWorkspace( obsTemplate, null );
        final Feature obsFeature = m_workspace.getRootFeature();
        m_obs = ObservationFeatureFactory.toObservation( obsFeature );

        m_obs.setName( "Zeitschritt " + stepNr );
        m_obs.setDescription( "RMA10s-Iteration Zeitschritt " + stepNr ); // TODO: name of calc unit?
        // m_obs.setTime( stepTime );

        // TODO: put components into map
        m_components.clear();

        final IComponent[] components = m_obs.getResult().getComponents();
        for( final IComponent component : components )
          m_components.put( component.getId(), component );
      }
      catch( final Exception e )
      {
        // TODO Auto-generated catch block
        e.printStackTrace();

        m_obs = null;
        m_workspace = null;
      }
    }

    if( m_obs == null )
      return;

    final TupleResult result = m_obs.getResult();
    final IRecord record = result.createRecord();

    record.setValue( m_components.get( "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#OrdinalNumber" ), values[2].toBigIntegerExact() );
    // record.setValue( m_components.get( "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#NSZF" ), values[] );
    record.setValue( m_components.get( "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#MaxVelocityX" ), values[4] );
    record.setValue( m_components.get( "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#MaxVelocityY" ), values[6] );
    record.setValue( m_components.get( "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#MaxVelocityNode" ), values[5].toBigIntegerExact() );
    record.setValue( m_components.get( "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#MaxDepth" ), values[8] );
    record.setValue( m_components.get( "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#MaxDepthNode" ), values[9].toBigIntegerExact() );
    record.setValue( m_components.get( "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#AverageVelocityX" ), values[18] );
    record.setValue( m_components.get( "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#AverageVelocityY" ), values[19] );
    record.setValue( m_components.get( "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#AverageDepth" ), values[20] );

    result.add( record );
  }
}
