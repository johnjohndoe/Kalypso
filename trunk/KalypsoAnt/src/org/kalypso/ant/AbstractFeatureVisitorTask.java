/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ant;

import java.io.BufferedWriter;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.InvocationTargetException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.IOUtils;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Task;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.kalypso.commons.java.net.UrlResolver;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.IErrorHandler;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.contribs.eclipse.swt.widgets.GetShellFromDisplay;
import org.kalypso.contribs.java.lang.reflect.ClassUtilities;
import org.kalypso.contribs.java.net.IUrlResolver;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * Abstract tast for task which starts a visitor on some features.
 * <p>
 * TODO: give argument of accept-depth (default is DEPTH_INFINITE)
 * </p>
 * <p>
 * The featurePath argument supports multiple pathes separated by ';'
 * </p>
 * 
 * @author belger
 */
public abstract class AbstractFeatureVisitorTask extends Task implements ICoreRunnableWithProgress, IErrorHandler
{
  /**
   * Separator between feature-pathes ; can be used to give this Task multiple feature-pathes. The task will then
   * iterate over every feature in every given feature-path
   */
  public static final String FEATURE_PATH_SEPARATOR = ";";

  /** Href to GML */
  private String m_gml;

  /**
   * Depth for visiting features.
   * <p>
   * Supported values are:
   * <ul>
   * <li>zero</li>
   * <li>infinite</li>
   * <li>infinite_links</li>
   * </ul>
   * </p>
   * 
   * @see FeatureVisitor
   */
  private String m_depth = "infinite";

  /**
   * Feature-Path innerhalb des GMLs. Alle durch diesen Pfad denotierten Features werden behandelt.
   */
  private String[] m_featurePath;

  /** Kontext (=URL), gegen welche die Links innerhalb des GML aufgelöst werden. */
  private URL m_context;

  /** if true, the task is executed whithin a progress dialog */
  private boolean m_runAsync;

  private boolean m_doSaveGml = false;

  /** if true, illegal feature pathes are ignored */
  private boolean m_ignoreIllegalFeaturePath = false;

  /**
   * @param doSaveGml
   *          If true, the read gml will be safed at the end of the process.
   */
  public AbstractFeatureVisitorTask( final boolean doSaveGml )
  {
    m_doSaveGml = doSaveGml;
  }

  public void setIgnoreIllegalFeaturePath( boolean ignoreIllegalFeaturePath )
  {
    m_ignoreIllegalFeaturePath = ignoreIllegalFeaturePath;
  }

  public void setRunAsync( final boolean runAsync )
  {
    m_runAsync = runAsync;
  }

  public final void setContext( final URL context )
  {
    m_context = context;
  }

  public final void setFeaturePath( final String featurePath )
  {
    m_featurePath = featurePath.split( FEATURE_PATH_SEPARATOR );
  }

  public final void setGml( final String gml )
  {
    m_gml = gml;
  }

  public void setDepth( String depth )
  {
    m_depth = depth;
  }

  /**
   * @see org.apache.tools.ant.Task#execute()
   */
  public final void execute() throws BuildException
  {
    try
    {
      if( m_runAsync )
        executeInDialog();
      else
      {
        final IStatus status = execute( new NullProgressMonitor() );
        if( !status.isOK() )
        {
          final String message = StatusUtilities.messageFromStatus( status );
          throw new BuildException( message, status.getException() );
        }
      }
    }
    catch( final BuildException be )
    {
      throw be;
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      throw new BuildException( e.getLocalizedMessage(), e );
    }
  }

  protected abstract FeatureVisitor createVisitor( final URL context, final IUrlResolver resolver,
      final PrintWriter logWriter, final IProgressMonitor monitor ) throws CoreException, InvocationTargetException,
      InterruptedException;

  protected abstract void validateInput();

  /**
   * @see org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress#execute(org.eclipse.core.runtime.IProgressMonitor)
   */
  public IStatus execute( final IProgressMonitor monitor ) throws InterruptedException
  {
    PrintWriter logPW = null;
    try
    {
      monitor.beginTask( ClassUtilities.getOnlyClassName( getClass() ), m_featurePath.length );

      final StringWriter logwriter = new StringWriter();
      logPW = new PrintWriter( new BufferedWriter( logwriter ) );

      validateInput();

      final int depth;
      if( m_depth.compareToIgnoreCase( "infinite" ) == 0 )
        depth = FeatureVisitor.DEPTH_INFINITE;
      else if( m_depth.compareToIgnoreCase( "infinite_links" ) == 0 )
        depth = FeatureVisitor.DEPTH_INFINITE;
      else if( m_depth.compareToIgnoreCase( "zero" ) == 0 )
        depth = FeatureVisitor.DEPTH_ZERO;
      else
        throw new BuildException( "Unsupported value of 'depth': " + m_depth );

      final IUrlResolver resolver = new UrlResolver();
      final URL gmlURL = resolver.resolveURL( m_context, m_gml );

      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( gmlURL );

      final List stati = new ArrayList();
      for( int i = 0; i < m_featurePath.length; i++ )
      {
        if( monitor.isCanceled() )
          throw new InterruptedException();

        final SubProgressMonitor subProgressMonitor = new SubProgressMonitor( monitor, 1 );
        try
        {
          final FeatureVisitor visitor = createVisitor( m_context, resolver, logPW, subProgressMonitor );

          final String fp = m_featurePath[i];
          workspace.accept( visitor, fp, depth );

          final IStatus statusFromVisitor = statusFromVisitor( visitor );
          if( !statusFromVisitor.isOK() )
            stati.add( statusFromVisitor );
        }
        catch( final IllegalArgumentException e )
        {
          final IStatus status = StatusUtilities.statusFromThrowable( e );
          if( m_ignoreIllegalFeaturePath )
          {
            logPW.println( "Feature wird ignoriert (" + status.getMessage() + ")" );
          }
          else
          {
            logPW.println( status.getMessage() );
            stati.add( status );
          }
        }
        catch( final Throwable t )
        {
          final IStatus status = StatusUtilities.statusFromThrowable( t );
          logPW.println( status.getMessage() );
          stati.add( status );
        }
        finally
        {
          subProgressMonitor.done();
        }
      }

      if( m_doSaveGml )
      {
        OutputStreamWriter writer = null;
        try
        {
          writer = resolver.createWriter( gmlURL );
          GmlSerializer.serializeWorkspace( writer, workspace );
          writer.close();
        }
        finally
        {
          IOUtils.closeQuietly( writer );
        }
      }

      logPW.close();
      final Project antProject = getProject();
      final String logString = logwriter.toString();
      if( antProject == null )
        System.out.print( logString );
      else
        antProject.log( logString );

      return new MultiStatus( KalypsoGisPlugin.getId(), 0, (IStatus[])stati.toArray( new IStatus[stati.size()] ), "",
          null );
    }
    catch( final Exception e )
    {
      if( e instanceof InterruptedException )
        throw (InterruptedException)e;

      return StatusUtilities.statusFromThrowable( e );
    }
    finally
    {
      IOUtils.closeQuietly( logPW );

      monitor.done();
    }
  }

  /**
   * Returns a status for the used visitor. Default implementation return the OK-status.
   * <p>
   * Should be overwritten by implementors.
   * </p>
   * 
   * @param visitor
   *          The visitor previously create by {@link #createVisitor(URL, IUrlResolver, PrintWriter, IProgressMonitor)}.
   */
  protected IStatus statusFromVisitor( final FeatureVisitor visitor )
  {
    if( visitor != null )
    {
      // only, to avoid yellow thingies
    }

    return Status.OK_STATUS;
  }

  private void executeInDialog()
  {
    final Display display = PlatformUI.getWorkbench().getDisplay();
    final Shell shell = new GetShellFromDisplay( display ).getShell();

    RunnableContextHelper.executeInProgressDialog( shell, this, this );
  }
}