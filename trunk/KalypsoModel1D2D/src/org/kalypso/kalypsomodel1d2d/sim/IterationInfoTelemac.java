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
import java.math.BigInteger;
import java.net.URL;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.io.IOUtils;
import org.apache.commons.vfs2.FileObject;
import org.apache.commons.vfs2.FileSystemException;
import org.apache.commons.vfs2.FileUtil;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.KalypsoCorePlugin;
//import org.kalypso.kalypsomodel1d2d.conv.TelemacDataConverterHelper;
import org.kalypso.kalypsomodel1d2d.schema.dict.Kalypso1D2DDictConstants;
import org.kalypso.kalypsomodel1d2d.sim.i18n.Messages;
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
 * @author ig
 */
public class IterationInfoTelemac implements IIterationInfo
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

    /**
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString( )
    {
      return this.name;
    }
  }

  private final FileObject m_itrFile;

  private final File m_outputDir;

  /** The observations of time steps */
  // private final IObservation<TupleResult> m_timeSteps;

  private final Map<String, IComponent> m_components = new HashMap<String, IComponent>();

  private final List<IterationBean> m_iterations = new ArrayList<IterationBean>();

  /** The underlying workspace of the current observation */
  private GMLWorkspace m_workspace;

  /** The current observation of step m_stepNr */
  private IObservation<TupleResult> m_obs = null;

  private Integer m_stepNr = -1;

  private int m_lastLineNumber;

  private boolean m_boolInResBlock = false;

  private String m_strActDate;

  private Integer m_stepNrTelemac = 0;

  private Date m_firstDate = null;

  private Calendar m_calendarFirst;

  public IterationInfoTelemac( final FileObject iterObsFile, final File outputDir, final IObservation<TupleResult> timeSteps )
  {
    m_itrFile = iterObsFile;
    m_outputDir = outputDir;
    // m_timeSteps = timeSteps;

    /* Create observation from template */
    final URL obsTemplate = getClass().getResource( "resource/template/iterObsTemplateTelemac.gml" ); //$NON-NLS-1$
    try
    {
      m_workspace = GmlSerializer.createGMLWorkspace( obsTemplate, null );
    }
    catch( final Exception e )
    {
      KalypsoCorePlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
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
      final byte[] content = FileUtil.getContent( m_itrFile );
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
      if( lnr == null )
        StatusUtilities.createStatus( IStatus.WARNING, ISimulation1D2DConstants.CODE_RMA10S, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.IterationInfo.1" ), e ); //$NON-NLS-1$

      final String msg = Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.IterationInfo.2", lnr.getLineNumber() ); //$NON-NLS-1$
      StatusUtilities.createStatus( IStatus.WARNING, ISimulation1D2DConstants.CODE_RMA10S, msg, e );
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
    if( line == null || line.trim().length() == 0 || (line.length() > 0 && (line.charAt( 0 ) == '/' || line.trim().startsWith( "**" ))) ) //$NON-NLS-1$  //$NON-NLS-2$
      return;

    if( line != null && line.trim().startsWith( "ADVECTION STEP" ) ) //$NON-NLS-1$
    {
      m_boolInResBlock = true;
    }
    else 
      if( line != null && line.trim().startsWith( "RELATIVE ERROR IN VOLUME AT T = " ) ) //$NON-NLS-1$
      {
        final String lStrDate = line.substring( line.indexOf( "T =" ) + 1 ); //$NON-NLS-1$
        if( !lStrDate.equals( m_strActDate ) )
        {
          m_stepNrTelemac++;
          m_strActDate = lStrDate;
        }
        
      }
    else
    {
      if( line != null && line.trim().startsWith( "LISTING OF TELEMAC-2D" ) ) //$NON-NLS-1$
      {
        m_boolInResBlock = false;
      }
    }
    if( m_boolInResBlock )
    {
      try
      {
        if( line.trim().startsWith( "1 #" ) ) { //$NON-NLS-1$
          addLine( line.trim().substring( 2 ) );
        }
        else
        {
          addLine( line );
        }
      }
      catch( final Exception e )
      {
        KalypsoCorePlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
      }

      m_stepNr = m_stepNrTelemac;
    }
  }

  private void addLine( final String values )
  {
    final Date stepDate = getDateForStep( m_strActDate );

    if( stepDate == null )
    {
      m_obs.setName( m_strActDate );
    }
    else
    {
      if( m_firstDate == null )
      {
        m_firstDate = stepDate;
        m_calendarFirst = Calendar.getInstance( KalypsoCorePlugin.getDefault().getTimeZone() );
        m_calendarFirst.setTime( stepDate );
      }
      // REMARK: convert to calendar with correct time zone, so formatting works correct
      final Calendar calendar = Calendar.getInstance( KalypsoCorePlugin.getDefault().getTimeZone() );
      calendar.setTime( stepDate );
      m_obs.setName( Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.IterationInfo.7", m_calendarFirst ) + "-" + Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.IterationInfo.7", calendar ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    }

    final TupleResult result = m_obs.getResult();
    final IRecord record = result.createRecord();

    record.setValue( m_components.get( Kalypso1D2DDictConstants.DICT_COMPONENT_ORDINAL_NUMBER ), new BigInteger( m_stepNrTelemac.toString() ) );
    record.setValue( m_components.get( Kalypso1D2DDictConstants.DICT_COMPONENT_STEP_INFO_TELEMAC ), values );

    result.add( record );
  }

  private Date getDateForStep( final Object stepNrObj )
  {
    if( stepNrObj instanceof String )
    {
      final String strActDateTelemac = (String) stepNrObj;
//      return TelemacDataConverterHelper.getDateForStepFromString( strActDateTelemac.trim() );
      return  new Date ( new Long( strActDateTelemac.trim() ) );
    }
//    else if( stepNrObj == null && m_strActDate != null )
//    {
//      return TelemacDataConverterHelper.getDateForStepFromString( m_strActDate.trim() );
//    }
    else
    {
      return null;
    }
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
    final String obsName;
    final String obsDesc;
    final File obsFile;
    IStatus status;

    final Date stepDate = getDateForStep( m_strActDate );
    
    if( stepDate == null ){
      return;
    }
    
    final String fileName;
    // REMARK: convert to calendar with correct time zone, so formatting works correct
    final Calendar calendar = Calendar.getInstance( KalypsoCorePlugin.getDefault().getTimeZone() );
    
    calendar.setTime( stepDate );
    obsName = String.format( "%1$te.%1$tm.%1$tY %1$tH:%1$tM %1$tZ", m_calendarFirst ) + String.format( "-%1$te.%1$tm.%1$tY %1$tH:%1$tM %1$tZ", calendar ); //$NON-NLS-1$ //$NON-NLS-2$
    obsDesc = Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.IterationInfo.15", m_calendarFirst ) + "-" + Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.IterationInfo.15", calendar ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    fileName = String.format( "Iteration_Telemac%1$te.%1$tm.%1$tY_%1$tH_%1$tM_%1$tZ-%1$te.%1$tm.%1$tY_%1$tH_%1$tM_%1$tZ.gml", m_calendarFirst, calendar ); //$NON-NLS-1$

    m_obs.setName( obsName );
    m_obs.setDescription( obsDesc );

    obsFile = new File( m_outputDir, fileName );

    try
    {
      /* Save the observation */
      final Feature obsFeature = m_workspace.getRootFeature();
      ObservationFeatureFactory.toFeature( m_obs, obsFeature );
      GmlSerializer.serializeWorkspace( obsFile, m_workspace, "UTF-8" ); //$NON-NLS-1$

      status = StatusUtilities.createStatus( IStatus.OK, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.IterationInfo.18", obsName ), null ); //$NON-NLS-1$ //$NON-NLS-2$
    }
    catch( final Throwable e )
    {
      status = StatusUtilities.createStatus( IStatus.ERROR, Messages.getString( "org.kalypso.kalypsomodel1d2d.sim.IterationInfo.20", obsName ), e ); //$NON-NLS-1$
    }

    m_iterations.add( new IterationBean( obsName, obsFile, status ) );
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
