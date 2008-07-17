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
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.commons.command.ICommandManager;
import org.kalypso.commons.command.ICommandManagerListener;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.java.net.UrlResolver;
import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.i18n.Messages;
import org.kalypso.loader.AbstractLoader;
import org.kalypso.loader.LoaderException;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.util.pool.KeyInfo;
import org.kalypso.util.pool.ResourcePool;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;

/**
 * @author Belger
 */
public class ShapeLoader extends AbstractLoader
{
  private final UrlResolver m_urlResolver = new UrlResolver();

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
   * @see org.kalypso.loader.ILoader#getDescription()
   */
  public String getDescription( )
  {
    return "ESRI Shape"; //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.loader.AbstractLoader#loadIntern(java.lang.String, java.net.URL,
   *      org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  protected Object loadIntern( final String location, final URL context, final IProgressMonitor monitor ) throws LoaderException
  {
    /* Files that get deleted at the end of this operation. */
    final List<File> filesToDelete = new ArrayList<File>();

    try
    {
      // eventuelle vorhandenen Information zum CRS abschneiden
      final int index = location.indexOf( '#' );

      final String sourceSrs;
      final String shpSource;
      if( index != -1 && index + 1 < location.length() )
      {
        sourceSrs = location.substring( index + 1 );
        shpSource = location.substring( 0, index );
      }
      else
      {
        sourceSrs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
        shpSource = location;
      }

      IResource shpResource = null;
      IResource dbfResource = null;
      IResource shxResource = null;

      final URL sourceURL = UrlResolverSingleton.resolveUrl( context, shpSource );

      final URL shpURL = UrlResolverSingleton.resolveUrl( context, shpSource + ".shp" ); //$NON-NLS-1$
      final URL dbfURL = UrlResolverSingleton.resolveUrl( context, shpSource + ".dbf" ); //$NON-NLS-1$
      final URL shxURL = UrlResolverSingleton.resolveUrl( context, shpSource + ".shx" ); //$NON-NLS-1$

      // leider können Shapes nicht aus URL geladen werden -> protocoll checken
      final File sourceFile;
      final IPath resource = ResourceUtilities.findPathFromURL( sourceURL );
      if( resource != null )
      {
        sourceFile = ResourceUtilities.makeFileFromPath( resource );

        shpResource = ResourceUtilities.findFileFromURL( shpURL );
        dbfResource = ResourceUtilities.findFileFromURL( dbfURL );
        shxResource = ResourceUtilities.findFileFromURL( shxURL );
      }
      else if( sourceURL.getProtocol().startsWith( "file" ) ) //$NON-NLS-1$
      {
        sourceFile = new File( sourceURL.getPath() );
      }
      else
      {
        /* If everything else fails, we copy the resources to local files */
        sourceFile = File.createTempFile( "shapeLocalizedFiled", "" ); //$NON-NLS-1$ //$NON-NLS-2$
        final String sourceFilePath = sourceFile.getAbsolutePath();

        final File shpFile = new File( sourceFilePath + ".shp" ); //$NON-NLS-1$
        final File dbfFile = new File( sourceFilePath + ".dbf" ); //$NON-NLS-1$
        final File shxFile = new File( sourceFilePath + ".shx" ); //$NON-NLS-1$

        filesToDelete.add( sourceFile );
        filesToDelete.add( shpFile );
        filesToDelete.add( dbfFile );
        filesToDelete.add( shxFile );

        FileUtils.copyURLToFile( shpURL, shpFile );
        FileUtils.copyURLToFile( dbfURL, dbfFile );
        FileUtils.copyURLToFile( shxURL, shxFile );
      }

      if( sourceFile == null )
        throw new LoaderException( Messages.getString( "org.kalypso.ogc.gml.loader.ShapeLoader.10" ) + shpSource ); //$NON-NLS-1$

      // Workspace laden
      final String sourceCrs = sourceSrs;
      final String targetCRS = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();

      final GMLWorkspace gmlWorkspace = ShapeSerializer.deserialize( sourceFile.getAbsolutePath(), sourceCrs );
      final CommandableWorkspace workspace = new CommandableWorkspace( gmlWorkspace );
      workspace.addCommandManagerListener( m_commandManagerListener );

      try
      {
        workspace.accept( new TransformVisitor( targetCRS ), workspace.getRootFeature(), FeatureVisitor.DEPTH_INFINITE );
      }
      catch( final Throwable e1 )
      {
        e1.printStackTrace();
      }

      if( shpResource != null )
        addResource( shpResource, workspace );
      if( dbfResource != null )
        addResource( dbfResource, workspace );
      if( shxResource != null )
        addResource( shxResource, workspace );

      return workspace;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new LoaderException( e );
    }
    finally
    {
      for( final File file : filesToDelete )
        file.delete();
    }
  }

  @Override
  public void save( final String source, final URL context, final IProgressMonitor monitor, final Object data ) throws LoaderException
  {
    try
    {
      final GMLWorkspace workspace = (GMLWorkspace) data;
      final URL shpURL = m_urlResolver.resolveURL( context, source.split( "#" )[0] ); //$NON-NLS-1$

      final IFile file = ResourceUtilities.findFileFromURL( shpURL );
      if( file != null )
      {
        ShapeSerializer.serialize( workspace, file.getLocation().toFile().getAbsolutePath(), null );
      }
      else
        throw new LoaderException( Messages.getString( "org.kalypso.ogc.gml.loader.ShapeLoader.12" ) + shpURL ); //$NON-NLS-1$
    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();
      throw new LoaderException( Messages.getString( "org.kalypso.ogc.gml.loader.ShapeLoader.13" ) + source + "\n" + e.getLocalizedMessage(), e ); //$NON-NLS-1$ //$NON-NLS-2$
    }
    catch( final Throwable e )
    {
      e.printStackTrace();
      throw new LoaderException( Messages.getString( "org.kalypso.ogc.gml.loader.ShapeLoader.15" ) + e.getLocalizedMessage(), e ); //$NON-NLS-1$
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