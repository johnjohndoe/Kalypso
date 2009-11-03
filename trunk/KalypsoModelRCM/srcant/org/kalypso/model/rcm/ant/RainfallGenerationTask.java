/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.model.rcm.ant;

import java.net.URL;
import java.util.ArrayList;
import java.util.Date;
import java.util.Hashtable;
import java.util.List;
import java.util.logging.Level;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Task;
import org.eclipse.ant.core.AntCorePlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.contribs.java.util.logging.ILogger;
import org.kalypso.contribs.java.util.logging.LoggerUtilities;
import org.kalypso.model.rcm.util.RainfallGenerationOp;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;

/**
 * This task generates rainfall for catchment areas.
 * 
 * @author Gernot Belger
 */
public class RainfallGenerationTask extends Task
{
  public final static class Generator
  {
    private String m_id;

    private long m_from;

    private long m_to;

    public String getRcmId( )
    {
      return m_id;
    }

    public void setRcmId( final String id )
    {
      m_id = id;
    }

    public long getFrom( )
    {
      return m_from;
    }

    public void setFrom( final long from )
    {
      m_from = from;
    }

    public long getTo( )
    {
      return m_to;
    }

    public void setTo( final long to )
    {
      m_to = to;
    }
  }

  private final List<Generator> m_generators = new ArrayList<Generator>();

  private URL m_rcmUrl;

  private URL m_catchmentUrl;

  private String m_catchmentFeaturePath;

  private String m_catchmentObservationPath;

  private String m_catchmentAreaPath;

  private String m_targetFilter;

  private Date m_targetFrom;

  private Date m_targetTo;

  public void addConfiguredGenerator( final Generator generator )
  {
    m_generators.add( generator );
  }

  public void setRcmUrl( final URL rcmUrl )
  {
    m_rcmUrl = rcmUrl;
  }

  public void setCatchmentUrl( final URL catchmentUrl )
  {
    m_catchmentUrl = catchmentUrl;
  }

  public void setCatchmentFeaturePath( final String catchmentFeaturePath )
  {
    m_catchmentFeaturePath = catchmentFeaturePath;
  }

  public void setCatchmentObservationPath( final String catchmentObservationPath )
  {
    m_catchmentObservationPath = catchmentObservationPath;
  }

  public void setCatchmentAreaPath( final String catchmentAreaPath )
  {
    m_catchmentAreaPath = catchmentAreaPath;
  }

  public void setTargetFilter( final String targetFilter )
  {
    m_targetFilter = targetFilter;
  }

  public void setTargetFrom( final Long targetFrom )
  {
    m_targetFrom = new Date( targetFrom );
  }

  public void setTargetTo( final Long targetTo )
  {
    m_targetTo = new Date( targetTo );
  }

  /**
   * @see org.apache.tools.ant.Task#execute()
   */
  @Override
  public void execute( ) throws BuildException
  {
    try
    {
      // Initialise ant-stuff: progress monitor and logging
      final Project antProject = getProject();
      final String message = getDescription();
      final String taskMessage = message == null || message.length() == 0 ? getTaskName() : message;

      final Hashtable< ? , ? > references = antProject == null ? null : antProject.getReferences();
      final IProgressMonitor monitor = references == null ? null : (IProgressMonitor) references.get( AntCorePlugin.ECLIPSE_PROGRESS_MONITOR );

      // REMARK: It is NOT possible to put this inner class into an own .class file (at least not inside the plugin
      // code) else we get an LinkageError when accessing the Project class.
      final ILogger logger = new ILogger()
      {
        /**
         * @see org.kalypso.contribs.java.util.logging.ILogger#log(java.util.logging.Level, int, java.lang.String)
         */
        public void log( final Level level, final int msgCode, final String logMessage )
        {
          final String outString = LoggerUtilities.formatLogStylish( level, msgCode, logMessage );
          final int antLevel = mapLevelToAnt( level );

          antProject.log( RainfallGenerationTask.this, outString, antLevel );
        }
      };

      logger.log( Level.INFO, LoggerUtilities.CODE_NEW_MSGBOX, taskMessage );

      // Real work starts here: create the operation, convert and validate parameters
      final SubMonitor progress = SubMonitor.convert( monitor, taskMessage, 100 );
      progress.subTask( "Operation wird initialisiert" );

      /* Load the catchment workspace. */
      final GMLWorkspace catchmentWorkspace = GmlSerializer.createGMLWorkspace( m_catchmentUrl, null );

      /* Tansform the catchment workspace. */
      final TransformVisitor transformVisitor = new TransformVisitor( KalypsoDeegreePlugin.getDefault().getCoordinateSystem() );
      catchmentWorkspace.accept( transformVisitor, catchmentWorkspace.getRootFeature(), TransformVisitor.DEPTH_INFINITE );

      ProgressUtilities.worked( progress, 4 );

      final RainfallGenerationOp operation = new RainfallGenerationOp( m_rcmUrl, catchmentWorkspace, m_catchmentFeaturePath, m_catchmentObservationPath, m_catchmentAreaPath, null, m_targetFilter, m_targetFrom, m_targetTo );
      for( final Generator generator : m_generators )
      {
        final Date fromDate = new Date( generator.getFrom() );
        final Date toDate = new Date( generator.getTo() );
        final String id = generator.getRcmId();
        operation.addGenerator( id, fromDate, toDate );
      }
      ProgressUtilities.worked( progress, 1 );

      // call the operation
      try
      {
        final SubMonitor subMon = progress.newChild( 95, SubMonitor.SUPPRESS_NONE );
        operation.execute( logger, subMon );
      }
      catch( final CoreException ce )
      {
        final IStatus status = ce.getStatus();
        antProject.log( this, status.getMessage(), status.getException(), Project.MSG_ERR );
      }
    }
    catch( final Throwable e )
    {
      e.printStackTrace();
      throw new BuildException( e );
    }
  }

  protected int mapLevelToAnt( final Level level )
  {
    if( Level.CONFIG.equals( level ) )
      return Project.MSG_DEBUG;
    if( Level.FINE.equals( level ) )
      return Project.MSG_VERBOSE;
    if( Level.FINER.equals( level ) )
      return Project.MSG_DEBUG;
    if( Level.FINEST.equals( level ) )
      return Project.MSG_DEBUG;
    if( Level.INFO.equals( level ) )
      return Project.MSG_INFO;
    if( Level.SEVERE.equals( level ) )
      return Project.MSG_ERR;
    if( Level.WARNING.equals( level ) )
      return Project.MSG_WARN;

    return Project.MSG_WARN;
  }
}
