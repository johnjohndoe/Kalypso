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
package org.kalypso.ogc.gml.loader;

import java.io.File;
import java.io.FileWriter;
import java.io.OutputStreamWriter;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Platform;
import org.kalypso.commons.command.ICommandManager;
import org.kalypso.commons.command.ICommandManagerListener;
import org.kalypso.commons.performance.TimeLogger;
import org.kalypso.commons.resources.SetContentHelper;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.java.net.IUrlResolver;
import org.kalypso.contribs.java.net.UrlResolver;
import org.kalypso.core.IKalypsoCoreConstants;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.core.i18n.Messages;
import org.kalypso.core.util.pool.IModelAdaptor;
import org.kalypso.core.util.pool.KeyInfo;
import org.kalypso.core.util.pool.ModelAdapterExtension;
import org.kalypso.core.util.pool.ResourcePool;
import org.kalypso.loader.AbstractLoader;
import org.kalypso.loader.LoaderException;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;

/**
 * Lädt einen GMLWorkspace aus einem GML
 * 
 * @author Belger
 */
public class GmlLoader extends AbstractLoader
{
  private final IUrlResolver m_urlResolver = new UrlResolver();

  /** A special command listener, which sets the dirty flag on the corresponding KeyInfo for the loaded workspace. */
  private final ICommandManagerListener m_commandManagerListener = new ICommandManagerListener()
  {
    public void onCommandManagerChanged( final ICommandManager source )
    {
      final Object[] objects = getObjects();
      for( final Object element : objects )
      {
        final CommandableWorkspace workspace = (CommandableWorkspace) element;
        final ICommandManager cm = workspace.getCommandManager();
        if( cm == source )
        {
          final ResourcePool pool = KalypsoCorePlugin.getDefault().getPool();
          final KeyInfo info = pool.getInfo( workspace );
          if( info != null )
          {
            info.setDirty( source.isDirty() );
          }
        }
      }
    }
  };

  /**
   * @see org.kalypso.loader.AbstractLoader#loadIntern(java.lang.String, java.net.URL,
   *      org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  protected Object loadIntern( final String source, final URL context, final IProgressMonitor monitor ) throws LoaderException
  {
    final boolean doTrace = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.core/perf/serialization/gml" ) ); //$NON-NLS-1$
    final List<IStatus> resultList = new ArrayList<IStatus>();

    try
    {
      monitor.beginTask( Messages.getString( "org.kalypso.ogc.gml.loader.GmlLoader.1" ), 1000 ); //$NON-NLS-1$

      final URL gmlURL = m_urlResolver.resolveURL( context, source );

      final PooledXLinkFeatureProviderFactory factory = new PooledXLinkFeatureProviderFactory();

      final GMLWorkspace gmlWorkspace = GmlSerializer.createGMLWorkspace( gmlURL, factory );

      CommandableWorkspace workspace = new CommandableWorkspace( gmlWorkspace );

      workspace.addCommandManagerListener( m_commandManagerListener );

      TimeLogger perfLogger = null;
      if( doTrace )
      {
        perfLogger = new TimeLogger( Messages.getString( "org.kalypso.ogc.gml.loader.GmlLoader.2" ) ); //$NON-NLS-1$
      }

      final String targetCRS = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
      monitor.subTask( Messages.getString( "org.kalypso.ogc.gml.loader.GmlLoader.3" ) ); //$NON-NLS-1$
      workspace.accept( new TransformVisitor( targetCRS ), workspace.getRootFeature(), FeatureVisitor.DEPTH_INFINITE );

      final IResource gmlFile = ResourceUtilities.findFileFromURL( gmlURL );
      if( gmlFile != null )
      {
        addResource( gmlFile, workspace );
      }

      if( perfLogger != null )
      {
        perfLogger.takeInterimTime();
        perfLogger.printCurrentTotal( Messages.getString( "org.kalypso.ogc.gml.loader.GmlLoader.4" ) ); //$NON-NLS-1$
      }

      final Feature rootFeature = workspace.getRootFeature();
      final IModelAdaptor[] modelAdaptors = ModelAdapterExtension.getModelAdaptor( rootFeature.getFeatureType().getQName().toString() );

      for( final IModelAdaptor modelAdaptor : modelAdaptors )
      {
        monitor.subTask( Messages.getString( "org.kalypso.ogc.gml.loader.GmlLoader.5" ) ); //$NON-NLS-1$
        workspace = modelAdaptor.adapt( workspace );
        resultList.add( modelAdaptor.getResult() );
      }

      if( workspace.isDirty() )
      {
        // some adaptation occured, so directly save workspace
        // but create a backup (.bak) of old workspace
        monitor.subTask( Messages.getString( "org.kalypso.ogc.gml.loader.GmlLoader.6" ) ); //$NON-NLS-1$

        if( gmlFile != null )
        {
          final IPath backupPath = gmlFile.getFullPath().addFileExtension( "bak" ); //$NON-NLS-1$
          backup( gmlFile, backupPath, monitor, 0, resultList );
        }

        monitor.subTask( Messages.getString( "org.kalypso.ogc.gml.loader.GmlLoader.8" ) ); //$NON-NLS-1$
        save( source, context, monitor, workspace );
      }

      setStatus( StatusUtilities.createStatus( resultList, String.format( Messages.getString( "org.kalypso.ogc.gml.loader.GmlLoader.9" ), gmlURL.toExternalForm() ) ) ); //$NON-NLS-1$

      return workspace;
    }
    catch( final LoaderException le )
    {
      le.printStackTrace();
      setStatus( StatusUtilities.statusFromThrowable( le ) );
      throw le;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      setStatus( StatusUtilities.statusFromThrowable( e ) );
      throw new LoaderException( Messages.getString( "org.kalypso.ogc.gml.loader.GmlLoader.10" ) + source + Messages.getString( "org.kalypso.ogc.gml.loader.GmlLoader.11" ) + e.getLocalizedMessage(), e ); //$NON-NLS-1$ //$NON-NLS-2$
    }
    finally
    {
      monitor.done();
    }
  }

  private void backup( final IResource gmlFile, final IPath backupPath, final IProgressMonitor monitor, final int count, final List<IStatus> resultList )
  {
    final IWorkspaceRoot root = gmlFile.getWorkspace().getRoot();
    final IPath currentTry = backupPath.addFileExtension( "" + count ); //$NON-NLS-1$
    final IResource backupResource = root.findMember( currentTry );
    if( backupResource != null )
    {
      backup( gmlFile, backupResource.getFullPath(), monitor, count + 1, resultList );
    }
    else
    {
      try
      {
        resultList.add( StatusUtilities.createInfoStatus( String.format( Messages.getString( "org.kalypso.ogc.gml.loader.GmlLoader.13" ), gmlFile.getName(), currentTry.toOSString() ) ) ); //$NON-NLS-1$
        gmlFile.copy( currentTry, false, monitor );
      }
      catch( final CoreException e )
      {
        resultList.add( e.getStatus() );
      }
    }
  }

  /**
   * @see org.kalypso.loader.ILoader#getDescription()
   */
  public String getDescription( )
  {
    return Messages.getString( "org.kalypso.ogc.gml.loader.GmlLoader.14" ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.loader.ILoader#save(java.lang.String, java.net.URL, org.eclipse.core.runtime.IProgressMonitor,
   *      java.lang.Object)
   */
  @Override
  public void save( final String source, final URL context, final IProgressMonitor monitor, final Object data ) throws LoaderException
  {
    IFile file = null;
    IMarker lockMarker = null;
    try
    {
      final GMLWorkspace workspace = (GMLWorkspace) data;

      final URL gmlURL = m_urlResolver.resolveURL( context, source );

      // ists im Workspace?
      file = ResourceUtilities.findFileFromURL( gmlURL );

      if( file != null )
      {
        // REMARK: see AbstractLoaderResourceDeltaVisitor for an explanation
        lockMarker = file.createMarker( IKalypsoCoreConstants.RESOURCE_LOCK_MARKER_TYPE );

        final SetContentHelper thread = new SetContentHelper()
        {
          @Override
          protected void write( final OutputStreamWriter writer ) throws Throwable
          {
            GmlSerializer.serializeWorkspace( writer, workspace );
          }
        };

        thread.setFileContents( file, false, true, new NullProgressMonitor() );
      }
      else if( file == null && gmlURL.getProtocol().equals( "file" ) ) //$NON-NLS-1$
      {
        final OutputStreamWriter w = new FileWriter( new File( gmlURL.getFile() ) );
        GmlSerializer.serializeWorkspace( w, workspace );
      }
      else
      {
        throw new LoaderException( Messages.getString( "org.kalypso.ogc.gml.loader.GmlLoader.16" ) + gmlURL ); //$NON-NLS-1$
      }
    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();

      throw new LoaderException( Messages.getString( "org.kalypso.ogc.gml.loader.GmlLoader.17" ) + source + "\n" + e.getLocalizedMessage(), e ); //$NON-NLS-1$ //$NON-NLS-2$
    }
    catch( final Throwable e )
    {
      e.printStackTrace();
      throw new LoaderException( Messages.getString( "org.kalypso.ogc.gml.loader.GmlLoader.19" ) + e.getLocalizedMessage(), e ); //$NON-NLS-1$
    }
    finally
    {
      /* delete all markers on the corresponding resource */
      if( lockMarker != null )
      {
        try
        {
          lockMarker.delete();
        }
        catch( final CoreException e )
        {
          KalypsoCorePlugin.getDefault().getLog().log( e.getStatus() );
        }
      }
    }
  }

  /**
   * @see org.kalypso.loader.AbstractLoader#release(java.lang.Object)
   */
  @Override
  public void release( final Object object )
  {
    final CommandableWorkspace workspace = (CommandableWorkspace) object;
    workspace.removeCommandManagerListener( m_commandManagerListener );

    super.release( object );

    workspace.dispose();
  }
}