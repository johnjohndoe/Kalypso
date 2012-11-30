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
package org.kalypso.model.rcm.ant;

import java.lang.reflect.InvocationTargetException;
import java.net.URL;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Map;
import java.util.logging.Level;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Task;
import org.eclipse.ant.core.AntCorePlugin;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.commons.tokenreplace.IStringResolver;
import org.kalypso.commons.tokenreplace.PropertiesStringResolver;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.contribs.java.util.logging.ILogger;
import org.kalypso.contribs.java.util.logging.LoggerUtilities;
import org.kalypso.model.rcm.IRainfallModelProvider;
import org.kalypso.model.rcm.RainfallGenerationOperation;
import org.kalypso.model.rcm.binding.IRainfallCatchmentModel;
import org.kalypso.model.rcm.util.IRainfallConfigurator;
import org.kalypso.model.rcm.util.RainfallExtensionUtilities;
import org.kalypso.model.rcm.util.UrlRainfallModellProvider;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.utils.log.GeoStatusLog;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * This task generates rainfall for catchment areas.
 *
 * @author Gernot Belger
 */
public class RainfallGenerationTask extends Task
{
  private URL m_rcmUrl;

  public void setRcmUrl( final URL rcmUrl )
  {
    m_rcmUrl = rcmUrl;
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
        @Override
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

      ProgressUtilities.worked( progress, 4 );

      final IStatus status = executeOperation( antProject, progress.newChild( 95, SubMonitor.SUPPRESS_NONE ) );
      // TODO: write status into logger
    }
    catch( final Throwable e )
    {
      e.printStackTrace();
      throw new BuildException( e );
    }
  }

  private IStatus executeOperation( final Project antProject, final IProgressMonitor monitor )
  {
    try
    {
      final Map<String, Object> properties = new HashMap<String, Object>();
      if( antProject != null )
      {
        final Hashtable<String, Object> antProperties = antProject.getProperties();
        properties.putAll( antProperties );
      }
      final IStringResolver variables = new PropertiesStringResolver( properties, "${", "}" );

      updateRcmGml( variables );

      final IRainfallModelProvider provider = new UrlRainfallModellProvider( m_rcmUrl );
      final RainfallGenerationOperation operation = new RainfallGenerationOperation( provider, variables );
      return operation.execute( monitor );
    }
    catch( final CoreException ce )
    {
      final IStatus status = ce.getStatus();
      antProject.log( this, status.getMessage(), status.getException(), Project.MSG_ERR );
      return Status.OK_STATUS;
    }
    catch( final InvocationTargetException e )
    {
      e.printStackTrace();
      throw new BuildException( e.getTargetException() );
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

  private void updateRcmGml( final IStringResolver variables ) throws CoreException
  {
    try
    {
      final IRainfallConfigurator configurator = RainfallExtensionUtilities.createRainfallConfigurator( RainfallExtensionUtilities.RAINFALL_CONFIGURATOR_ID );
      configurator.updateRcmGml( m_rcmUrl, variables );
    }
    catch( final CoreException ex )
    {
      if( m_rcmUrl != null )
        logMessage( ex );

      throw ex;
    }
  }

  private void logMessage( final CoreException ex )
  {
    try
    {
      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( m_rcmUrl, null );
      final IRainfallCatchmentModel rcmModel = (IRainfallCatchmentModel) workspace.getRootFeature();
      final String logPath = rcmModel.getLogPath();
      final URL context = workspace.getContext();
      final URL url = new URL( context, logPath );
      final IFile member = ResourceUtilities.findFileFromURL( url );
      if( member != null )
      {
        final GeoStatusLog log = new GeoStatusLog( member.getLocation().toFile() );
        log.log( ex.getStatus() );
        log.serialize();
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }
}