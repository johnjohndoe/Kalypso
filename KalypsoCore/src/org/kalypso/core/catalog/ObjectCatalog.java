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
package org.kalypso.core.catalog;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.util.List;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.java.net.IUrlResolver2;
import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.core.catalog.urn.IURNGenerator;
import org.kalypso.core.repository.Storage;

/**
 * @author doemming
 */
public abstract class ObjectCatalog<O> extends Storage
{
  private final CatalogManager m_manager;

  private final Class<O> m_supportingClass;

  @SuppressWarnings("unchecked") //$NON-NLS-1$
  public ObjectCatalog( File repositoryBase, CatalogManager manager, Class<O> supportingClass )
  {
    super( repositoryBase );
    m_manager = manager;
    m_supportingClass = supportingClass;
  }

  public O getValue( IUrlResolver2 resolver, String systemID, String publicID )
  {
    InputStream is = null;
    try
    {
      final ICatalog baseCatalog = m_manager.getBaseCatalog();
      final String uri = baseCatalog.resolve( systemID, publicID );
      // BUGFIX: if uri now start with 'urn', the id was not resolved, so we just return
      // Maybe there is a better way to handle that?
      if( uri.startsWith( "urn" ) ) //$NON-NLS-1$
        return null;
      
      final URL urlFeatureStyle = resolver.resolveURL( uri );

      final IUrlResolver2 catalogResolver = new IUrlResolver2()
      {
        public URL resolveURL( final String href ) throws MalformedURLException
        {
          final String ref = baseCatalog.resolve( href, href );
          return UrlResolverSingleton.resolveUrl( urlFeatureStyle, ref );
        }
      };

      is = new BufferedInputStream( urlFeatureStyle.openStream() );
      final O object = read( catalogResolver, is );
      is.close();
      return object;
    }
    catch( final Exception e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      KalypsoCorePlugin.getDefault().getLog().log( status );
      return null;
    }
    finally
    {
      IOUtils.closeQuietly( is );
    }
  }

  public URI getStore( final O object )
  {
    final IURNGenerator generator = m_manager.getURNGeneratorFor( m_supportingClass );
    if( generator == null )
      throw new UnsupportedOperationException();
    final String objectURN = generator.generateURNFor( object );
    return getStore( objectURN );
  }

  public void addRelative( final O object, final URI storeLocation ) throws Exception
  {
    add( object, storeLocation, true );
  }

  /**
   * caller may store object first
   */
  public void add( final O object, final URI storeLocation ) throws Exception
  {
    add( object, storeLocation, false );
  }

  private void add( final O object, final URI storeLocation, boolean relative ) throws Exception
  {
    final IURNGenerator generator = m_manager.getURNGeneratorFor( m_supportingClass );
    if( generator == null )
      throw new UnsupportedOperationException();

    store( object, storeLocation );
    final String objectURN = generator.generateURNFor( object );
    final String uriAsString = storeLocation.toString();
    final String systemID = objectURN;
    final String publicID = null;
    if( relative )
      m_manager.getBaseCatalog().addEntryRelative( uriAsString, systemID, publicID );
    else
      m_manager.getBaseCatalog().addEntry( uriAsString, systemID, publicID );

  }

  public List<String> getEntryURNs( Object parent ) throws Exception
  {
    final IURNGenerator generator = m_manager.getURNGeneratorFor( m_supportingClass );
    final String patternURN = generator.generateURNPatternForRelated( parent );
    return m_manager.getBaseCatalog().getEntryURNS( patternURN );
  }

  public O getDefault( final IUrlResolver2 resolver, final Object parent )
  {
    try
    {
      final IURNGenerator generator = m_manager.getURNGeneratorFor( m_supportingClass );
      if( generator == null )
        return null;
      final String defaultURN = generator.generateDefaultURNForRelated( parent );
      // System.out.println( "defaultURN: " + defaultURN );
      return getValue( resolver, defaultURN, defaultURN );
    }
    catch( final Exception e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      KalypsoCorePlugin.getDefault().getLog().log( status );
      return null;
    }
  }

  protected URI getStore( String entryURN )
  {
    final String storageHref = getStorageHref( entryURN );
    final File file = new File( getStorageBase(), storageHref );
    return file.toURI();
  }

  private URI store( final O object, final URI locationURI )
  {
    FileOutputStream outStream = null;
    try
    {
      final File file = new File( locationURI );
      outStream = new FileOutputStream( file );
      write( object, outStream );
      return file.toURI();
    }
    catch( Exception e )
    {
      e.printStackTrace();
      return null;
    }
    finally
    {
      IOUtils.closeQuietly( outStream );
    }
  }

  protected abstract O read( IUrlResolver2 catalogResolver, InputStream stream );

  protected abstract void write( O object, OutputStream os );

}
