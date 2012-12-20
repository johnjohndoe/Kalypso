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
package org.kalypso.kalypsomodel1d2d.sim;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.LineNumberReader;
import java.io.StringReader;
import java.math.BigDecimal;
import java.net.URL;
import java.nio.charset.Charset;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.datatype.XMLGregorianCalendar;

import org.apache.commons.io.IOUtils;
import org.apache.commons.vfs2.FileObject;
import org.apache.commons.vfs2.FileSystemException;
import org.apache.commons.vfs2.FileUtil;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.dict.Kalypso1D2DDictConstants;
import org.kalypso.kalypsomodel1d2d.sim.i18n.Messages;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.ComponentUtilities;
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
public class IterationInfo implements IIterationInfo
{
  public class IterationBean
  {
    public final File file;

    public final String name;

    public final IStatus status;

    public IterationBean( final String pStrName, final File pFile, final IStatus pStatus )
    {
      this.file = pFile;
      this.name = pStrName;
      this.status = pStatus;
    }

    @Override
    public String toString( )
    {
      return this.name;
    }
  }

  private final DateFormat m_timeStepFormat = new SimpleDateFormat( ISimulation1D2DConstants.TIMESTEP_DISPLAY_FORMAT );

  private final FileObject m_itrFile;

  private final File m_outputDir;

  /** The observations of time steps */
  private final IObservation<TupleResult> m_timeSteps;

  private final Map<String, IComponent> m_components = new HashMap<>();

  private final List<IterationBean> m_iterations = new ArrayList<>();

  /** The underlying workspace of the current observation */
  private GMLWorkspace m_workspace;

  /** The current observation of step m_stepNr */
  private IObservation<TupleResult> m_obs = null;

  private int m_stepNr = -1;

  private int m_lastLineNumber;

  public IterationInfo( final FileObject iterObsFile, final File outputDir, final IObservation<TupleResult> timeSteps )
  {
    m_itrFile = iterObsFile;
    m_outputDir = outputDir;
    m_timeSteps = timeSteps;

    m_timeStepFormat.setTimeZone( KalypsoCorePlugin.getDefault().getTimeZone() );

    /* Create observation from template */
    final URL obsTemplate = getClass().getResource( "resource/template/iterObs.gml" ); //$NON-NLS-1$
    try
    {
      m_workspace = GmlSerializer.createGMLWorkspace( obsTemplate, null );
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      // TODO: error handling!

    }
    final Feature obsFeature = m_workspace.getRootFeature();
    m_obs = ObservationFeatureFactory.toObservation( obsFeature );

    final IComponent[] components = m_obs.getResult().getComponents();
    for( final IComponent component : components )
      m_components.put( component.getId(), component );
  }

  @Override
  public int getStepNr( )
  {
    return m_stepNr;
  }

  @Override
  public void readIterFile( ) throws IOException
  {
    m_itrFile.refresh();
    if( !m_itrFile.exists() )
      return;

    /* Read file and write outputs */
    LineNumberReader lnr = null;
    try
    {
      // final InputStream inputStream = m_itrFile.getContent().getInputStream();
      final byte[] content = FileUtil.getContent( m_itrFile );
      // lnr = new LineNumberReader( new BufferedReader( new InputStreamReader( inputStream ) ) );
      lnr = new LineNumberReader( new StringReader( new String( content, Charset.defaultCharset() ) ) );
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
      // FIXME: stati are never used; what happened here?!
//      if( lnr == null )
//        StatusUtilities.createStatus( IStatus.WARNING, ISimulation1D2DConstants.CODE_RMA10S, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.IterationInfo.1" ), e ); //$NON-NLS-1$
//
//      final String msg = Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.IterationInfo.2", lnr.getLineNumber() ); //$NON-NLS-1$
//      StatusUtilities.createStatus( IStatus.WARNING, ISimulation1D2DConstants.CODE_RMA10S, msg, e );
    }
    finally
    {
      IOUtils.closeQuietly( lnr );
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

    final String[] strings = line.trim().split( "\\s+" ); //$NON-NLS-1$
    if( strings.length != 25 )
      return;

    final BigDecimal[] values = new BigDecimal[strings.length];
    for( int i = 0; i < strings.length; i++ )
    {
      final String lStrActSub = strings[i].trim();

      if( "nan".equalsIgnoreCase( lStrActSub ) ) //$NON-NLS-1$
      {
        values[i] = new BigDecimal( -9999.0 );
      }
      else
      {
        values[i] = new BigDecimal( lStrActSub );
      }
    }
    int stepNr = -1;
    try
    {
      stepNr = values[1].intValueExact();
    }
    catch( final Exception e )
    {
    }
    addLine( stepNr, values );

    m_stepNr = stepNr;
  }

  private void addLine( final int stepNr, final BigDecimal[] values )
  {
    if( stepNr != m_stepNr )
    {
      if( m_stepNr != -1 )
        finishCurrent();

      final Date stepDate = getDateForStep( stepNr );
      if( stepDate == null )
        m_obs.setName( "Unbekannt" ); //$NON-NLS-1$
      else
      {
        // REMARK: convert to calendar with correct time zone, so formatting works correct
        final Calendar calendar = Calendar.getInstance( KalypsoCorePlugin.getDefault().getTimeZone() );
        calendar.setTime( stepDate );

        final String stepName = m_timeStepFormat.format( stepDate );

        m_obs.setName( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.IterationInfo.7", stepName ) ); //$NON-NLS-1$
      }
    }

    final TupleResult result = m_obs.getResult();
    final IRecord record = result.createRecord();

    record.setValue( m_components.get( Kalypso1D2DDictConstants.DICT_COMPONENT_ORDINAL_NUMBER ), values[2].toBigIntegerExact() );
    // record.setValue( m_components.get( "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#NSZF" ), values[] );
    record.setValue( m_components.get( Kalypso1D2DDictConstants.DICT_COMPONENT_MAX_VELOCITY_X ), values[4] );
    record.setValue( m_components.get( Kalypso1D2DDictConstants.DICT_COMPONENT_MAX_VELOCITY_Y ), values[6] );
    record.setValue( m_components.get( Kalypso1D2DDictConstants.DICT_COMPONENT_MAX_VELOCITY_NODE ), values[5].toBigIntegerExact() );
    record.setValue( m_components.get( Kalypso1D2DDictConstants.DICT_COMPONENT_MAX_DEPTH ), values[8] );
    record.setValue( m_components.get( Kalypso1D2DDictConstants.DICT_COMPONENT_MAX_DEPTH_NODE ), values[9].toBigIntegerExact() );
    record.setValue( m_components.get( Kalypso1D2DDictConstants.DICT_COMPONENT_AVERAGE_VELOCITY_X ), values[18] );
    record.setValue( m_components.get( Kalypso1D2DDictConstants.DICT_COMPONENT_AVERAGE_VELOCITY_Y ), values[19] );
    record.setValue( m_components.get( Kalypso1D2DDictConstants.DICT_COMPONENT_AVERAGE_DEPTH ), values[20] );

    result.add( record );
  }

  private Date getDateForStep( final int stepNr )
  {
    if( stepNr == 0 )
      return ISimulation1D2DConstants.STEADY_DATE;

    final TupleResult result = m_timeSteps.getResult();
    if( m_stepNr >= result.size() )
      return null;

    final IComponent componentTime = ComponentUtilities.findComponentByID( result.getComponents(), Kalypso1D2DDictConstants.DICT_COMPONENT_TIME );
    final XMLGregorianCalendar stepCal = (XMLGregorianCalendar)result.get( stepNr ).getValue( componentTime );
    return DateUtilities.toDate( stepCal );
  }

  /**
   * To be called when calculation has finished to finish-off the last observation.
   */
  @Override
  public void finish( )
  {
    finishCurrent();
    try
    {
      m_itrFile.close();
    }
    catch( final FileSystemException e )
    {
      // gobble
    }
    m_obs = null;
    m_workspace.dispose();
    m_workspace = null;
  }

  /**
   * Saves the current operation into a file.
   */
  private void finishCurrent( )
  {
    // If nothing was processed at all, just return
    if( m_stepNr == -1 )
      return;

    final String obsName;
    final String obsDesc;
    final File obsFile;
    IStatus status;

    final Date stepDate = getDateForStep( m_stepNr );

    if( stepDate == null )
    {
      obsName = "Unbekannt"; //$NON-NLS-1$
      obsDesc = ""; //$NON-NLS-1$
      obsFile = null;
      final String msg = Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.IterationInfo.10", m_stepNr ); //$NON-NLS-1$
      status = new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, msg );
    }
    else
    {
      final String fileName;
      if( ISimulation1D2DConstants.STEADY_DATE.equals( stepDate ) )
      {
        obsName = Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.IterationInfo.11" ); //$NON-NLS-1$
        obsDesc = Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.IterationInfo.12" ); //$NON-NLS-1$
        fileName = "Iteration_steady.gml"; //$NON-NLS-1$
      }
      else
      {
        final Calendar calendar = Calendar.getInstance( KalypsoCorePlugin.getDefault().getTimeZone() );
        calendar.setTime( stepDate );
        obsName = m_timeStepFormat.format( stepDate );
        obsDesc = Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.IterationInfo.15", calendar ); //$NON-NLS-1$
        fileName = String.format( "Iteration_%1$te.%1$tm.%1$tY_%1$tH_%1$tM_%1$tZ.gml", calendar ); //$NON-NLS-1$
      }

      m_obs.setName( obsName );
      m_obs.setDescription( obsDesc );
      // obs.setTime( stepDate.getTime() ); // todo: we need the real time

      obsFile = new File( m_outputDir, fileName );

      try
      {
        /* Save the observation */
        final Feature obsFeature = m_workspace.getRootFeature();
        ObservationFeatureFactory.toFeature( m_obs, obsFeature );
        GmlSerializer.serializeWorkspace( obsFile, m_workspace, "UTF-8" ); //$NON-NLS-1$

        status = new Status( IStatus.OK, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.IterationInfo.18", obsName ) ); //$NON-NLS-1$
      }
      catch( final Throwable e )
      {
        status = new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.IterationInfo.20", obsName ), e ); //$NON-NLS-1$
      }
    }

    m_iterations.add( new IterationBean( obsName, obsFile, status ) );
    m_obs.getResult().clear();
  }

  @Override
  public IObservation<TupleResult> getCurrentIteration( )
  {
    if( m_stepNr == -1 )
      return null;

    return m_obs;
  }

  @Override
  public IterationBean[] getIterations( )
  {
    return m_iterations.toArray( new IterationBean[m_iterations.size()] );
  }
}