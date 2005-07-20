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
import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.InvocationTargetException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.IOUtils;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Task;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.kalypso.commons.java.net.UrlResolver;
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
 * 
 * @author belger
 */
public abstract class AbstractFeatureVisitorTask extends Task implements ICoreRunnableWithProgress, IErrorHandler
{
  /** Href to GML */
  private String m_gml;

  /**
   * Feature-Path innerhalb des GMLs. Alle durch diesen Pfad denotierten Features werden behandelt.
   */
  private String[] m_featurePath;

  /** Kontext (=URL), gegen welche die Links innerhalb des GML aufgel�st werden. */
  private URL m_context;

  /** if true, the task is executed whithin a progress dialog */
  private boolean m_runAsync;

  
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
    m_featurePath = featurePath.split( ";" );
  }

  public final void setGml( final String gml )
  {
    m_gml = gml;
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
          throw new BuildException( status.getMessage(), status.getException() );
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
      final PrintWriter logWriter, final IProgressMonitor monitor ) throws CoreException, InvocationTargetException, InterruptedException;

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
          workspace.accept( visitor, fp, FeatureVisitor.DEPTH_INFINITE );
        }
        catch( final Throwable t )
        {
          stati.add( RunnableContextHelper.statusFromThrowable( t ) );
        }
        finally
        {
          subProgressMonitor.done();
        }
      }

      logPW.close();
      log( logwriter.toString() );

      return new MultiStatus( KalypsoGisPlugin.getId(), 0, (IStatus[])stati.toArray( new IStatus[stati.size()] ), "", null );
    }
    catch( final Exception e )
    {
      if( e instanceof InterruptedException )
        throw (InterruptedException)e;

      return RunnableContextHelper.statusFromThrowable( e );
    }
    finally
    {
      IOUtils.closeQuietly( logPW );
      
      monitor.done();
    }
  }

  private void executeInDialog()
  {
    final Display display = PlatformUI.getWorkbench().getDisplay();
    final Shell shell = new GetShellFromDisplay( display ).getShell();

    RunnableContextHelper.executeInProgressDialog( shell, this, this );
  }
}