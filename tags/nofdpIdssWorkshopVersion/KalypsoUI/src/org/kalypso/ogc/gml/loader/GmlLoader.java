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

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.commons.command.ICommandManager;
import org.kalypso.commons.command.ICommandManagerListener;
import org.kalypso.commons.resources.SetContentHelper;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.java.net.IUrlResolver;
import org.kalypso.contribs.java.net.UrlResolver;
import org.kalypso.core.IKalypsoCoreConstants;
import org.kalypso.loader.AbstractLoader;
import org.kalypso.loader.LoaderException;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ogc.gml.serialize.GmlSerializerFeatureProviderFactory;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.pool.KeyInfo;
import org.kalypso.util.pool.ResourcePool;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.IFeatureProviderFactory;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * L�dt einen GMLWorkspace aus einem GML
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
      for( int i = 0; i < objects.length; i++ )
      {
        final CommandableWorkspace workspace = (CommandableWorkspace) objects[i];
        final ICommandManager cm = workspace.getCommandManager();
        if( cm == source )
        {
          final ResourcePool pool = KalypsoGisPlugin.getDefault().getPool();
          final KeyInfo info = pool.getInfo( workspace );
          if( info != null )
            info.setDirty( source.isDirty() );
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
    try
    {
      monitor.beginTask( "GML laden", 1000 );

      final URL gmlURL = m_urlResolver.resolveURL( context, source );

//      final IFeatureProviderFactory factory = new PooledXLinkFeatureProviderFactory();
      final IFeatureProviderFactory factory = new GmlSerializerFeatureProviderFactory();
      final GMLWorkspace gmlWorkspace = GmlSerializer.createGMLWorkspace( gmlURL, m_urlResolver, factory );

      final CommandableWorkspace workspace = new CommandableWorkspace( gmlWorkspace );

      workspace.addCommandManagerListener( m_commandManagerListener );

      final CS_CoordinateSystem targetCRS = KalypsoGisPlugin.getDefault().getCoordinatesSystem();
      workspace.accept( new TransformVisitor( targetCRS ), workspace.getRootFeature(), FeatureVisitor.DEPTH_INFINITE );

      final IResource gmlFile = ResourceUtilities.findFileFromURL( gmlURL );
      if( gmlFile != null )
        addResource( gmlFile, workspace );

      return workspace;
    }
    catch( final LoaderException le )
    {
      le.printStackTrace();

      throw le;
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      throw new LoaderException( "GML konnte nicht geladen werden: " + source + ". Grund: " + e.getLocalizedMessage(), e );
    }
    finally
    {
      monitor.done();
    }
  }

  /**
   * @see org.kalypso.loader.ILoader#getDescription()
   */
  public String getDescription( )
  {
    return "GML Layer";
  }

  /**
   * @see org.kalypso.loader.ILoader#save(java.lang.String, java.net.URL, org.eclipse.core.runtime.IProgressMonitor,
   *      java.lang.Object)
   */
  @Override
  public void save( final String source, final URL context, final IProgressMonitor monitor, final Object data ) throws LoaderException
  {
    IFile file = null;
    try
    {
      final GMLWorkspace workspace = (GMLWorkspace) data;

      final URL gmlURL = m_urlResolver.resolveURL( context, source );

      // ists im Workspace?
      file = ResourceUtilities.findFileFromURL( gmlURL );

      if( file != null )
      {
        file.createMarker( IKalypsoCoreConstants.RESOURCE_LOCK_MARKER_TYPE );

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
      else if( file == null && gmlURL.getProtocol().equals( "file" ) )
      {
        final OutputStreamWriter w = new FileWriter( new File( gmlURL.getFile() ) );
        GmlSerializer.serializeWorkspace( w, workspace );
      }
      else
        throw new LoaderException( "Die URL kann nicht beschrieben werden: " + gmlURL );
    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();

      throw new LoaderException( "Der angegebene Pfad ist ung�ltig: " + source + "\n" + e.getLocalizedMessage(), e );
    }
    catch( final Throwable e )
    {
      e.printStackTrace();
      throw new LoaderException( "Fehler beim Speichern der URL\n" + e.getLocalizedMessage(), e );
    }
    finally
    {
      /* delete all markers on the corresponding resource */
      if( file != null )
      {
        try
        {
          final IMarker[] markers = file.findMarkers( source, false, IResource.DEPTH_ZERO );
          for( final IMarker marker : markers )
            marker.delete();
        }
        catch( final CoreException e )
        {
          KalypsoGisPlugin.getDefault().getLog().log( e.getStatus() );
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