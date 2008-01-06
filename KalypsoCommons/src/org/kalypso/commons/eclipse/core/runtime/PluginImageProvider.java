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
package org.kalypso.commons.eclipse.core.runtime;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.ui.internal.decorators.DecoratorOverlayIcon;
import org.kalypso.contribs.java.io.StreamUtilities;

/**
 * This class is a helper class to provide images from a central place.
 * <p>
 * It is intended to be used by every plug-in which contributes images.
 * </p>
 * <p>
 * This class is not intended to be sub-classed. Each UI-plug-in should have one instance.
 * </p>
 * 
 * @author Gernot Belger
 */
public class PluginImageProvider
{
  private final AbstractUIPlugin m_plugin;

  private final File m_imageTmpDir;

  public static interface ImageKey
  {
    public String getImagePath( );

    public String name( );
  }

  public PluginImageProvider( final AbstractUIPlugin plugin )
  {
    m_plugin = plugin;

    final File stateLocation = plugin.getStateLocation().toFile();
    m_imageTmpDir = new File( stateLocation, "pluginImages" );
  }

  /**
   * Deletes all previously via {@link #getTmpURL(ImageKey)} created temporary image files.
   * <p>
   * Should be called on every startup
   * </p>
   */
  public void resetTmpFiles( )
  {
    FileUtilities.deleteRecursive( m_imageTmpDir );
  }

  /**
   * Utility method for image re-use Plug-in.
   * 
   * @param key
   * @return
   */
  public Image getImage( final ImageKey key )
  {
    registerImage( key );

    return getImageRegistry().get( key.name() );
  }

  private void registerImage( final ImageKey key )
  {
    if( hasImage( key ) )
      return;

    final ImageRegistry registry = getImageRegistry();
    final ImageDescriptor imageDescriptor = getImageDescriptor( key );
    registry.put( key.name(), imageDescriptor );
  }

  private boolean hasImage( final ImageKey key )
  {
    return getImageRegistry().getDescriptor( key.name() ) != null;
  }

  private ImageRegistry getImageRegistry( )
  {
    return m_plugin.getImageRegistry();
  }

  /**
   * Returns an image descriptor for the image file at the given plug-in relative path.
   * 
   * @param path
   *            the path
   * @return the image descriptor
   */
  public ImageDescriptor getImageDescriptor( final String path )
  {
    return AbstractUIPlugin.imageDescriptorFromPlugin( PluginUtilities.id( m_plugin ), path );
  }

  /** Returns the image descriptor for the given key. Does not create an image. */
  public ImageDescriptor getImageDescriptor( final ImageKey key )
  {
    final String path = key.getImagePath();

    final ImageDescriptor imageDescriptor = getImageDescriptor( path );
    if( imageDescriptor == null )
      throw new NullPointerException( "Image-Path invalid for key: " + key + ", " + path );

    return imageDescriptor;
  }

  /**
   * @param decorators
   *            Must be an array of size 5: top-left, top-right, bottom-left, bottom-right, underlay
   */
  public Image getDecoratedImage( final ImageKey baseImageKey, final ImageKey[] decorators )
  {
    // create decorated name from components
    final StringBuffer nameBuffer = new StringBuffer( baseImageKey.name() );
    for( final ImageKey descriptor : decorators )
    {
      nameBuffer.append( '#' );
      if( descriptor == null )
        nameBuffer.append( "null" );
      else
      {
        nameBuffer.append( descriptor.getClass().getName() );
        nameBuffer.append( descriptor.name() );
      }
    }
    final String name = nameBuffer.toString();

    // if this icon is already registered, just return it
    final ImageRegistry registry = getImageRegistry();
    if( registry.getDescriptor( name ) != null )
      return registry.get( name );

    final ImageDescriptor icon = getDecoratedImageDescriptor( baseImageKey, decorators );
    registry.put( name, icon );
    return registry.get( name );
  }

  /**
   * @param decorators
   *            Must be an array of size 5: top-left, top-right, bottom-left, bottom-right, underlay
   */
  public ImageDescriptor getDecoratedImageDescriptor( final ImageKey baseImageKey, final ImageKey[] decorators )
  {
    // else, create the OverlayIcon
    final Image baseImage = getImage( baseImageKey );
    final ImageDescriptor[] decoratorDescriptors = new ImageDescriptor[decorators.length];
    for( int i = 0; i < decoratorDescriptors.length; i++ )
      decoratorDescriptors[i] = decorators[i] == null ? null : getImageDescriptor( decorators[i] );

    final Rectangle bounds = baseImage.getBounds();
    final Point size = new Point( bounds.width, bounds.height );

    // put it into the registry and return the image
    final ImageDescriptor icon = new DecoratorOverlayIcon( baseImage, decoratorDescriptors, size );
    return icon;
  }

  /** This method writes the image to a temporary file and returns a file url pointing to it. */
  public URL getTmpURL( final ImageKey key )
  {
    final URL url = PluginUtilities.findResource( PluginUtilities.id( m_plugin ), key.getImagePath() );
    return getTmpUrl( key.getImagePath(), url );
  }

  /**
   * Copies a resource-file into the tmp-directory and returns a (file-)url to it.
   * 
   * @param clazz
   *            The class to which the resource path will be resolved
   * @param path
   *            Path into the java-resources
   * @see Class#getResource(java.lang.String)
   */
  public URL getTmpUrl( final Class< ? > clazz, final String path )
  {
    return getTmpUrl( path, clazz.getResource( path ) );
  }

  private URL getTmpUrl( final String path, final URL url )
  {
    if( url == null )
      return null;
    try
    {
      final File imageDir = m_imageTmpDir;
      final File classDir = new File( imageDir, "resources" );
      final File imageFile = new File( classDir, path );
      if( imageFile.exists() )
        return imageFile.toURL();

      imageFile.getParentFile().mkdirs();

      final InputStream is = url.openStream();
      final FileOutputStream os = new FileOutputStream( imageFile );
      StreamUtilities.streamCopy( is, os );
      return imageFile.toURL();
    }
    catch( final IOException e )
    {
      PluginUtilities.logToPlugin( m_plugin, IStatus.ERROR, "Could not create temporary file for " + url.toString(), e );
      return null;
    }

  }
}
