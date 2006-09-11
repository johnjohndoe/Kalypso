/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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

import java.io.File;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.Unmarshaller;
import javax.xml.namespace.QName;

import oasis.names.tc.entity.xmlns.xml.catalog.Catalog;
import oasis.names.tc.entity.xmlns.xml.catalog.ObjectFactory;

import org.apache.commons.io.IOUtils;
import org.kalypso.core.catalog.urn.IURNGenerator;
import org.kalypso.jwsdp.JaxbUtilities;

/**
 * TODO: This catalog manager does two things, managing the catalogs and also knowning about what things are managed
 * (URNGenerator, ...)
 * <p>
 * In my opinion, this is already too much. We should rather split those two concepts for clarity. Gernot.
 * </p>
 * <p>
 * the default-catalog is dynamic, but changes will not be saved
 * </p>
 * 
 * @author doemming
 */
public class CatalogManager
{
  public final static JAXBContext JAX_CONTEXT_CATALOG = JaxbUtilities.createQuiet( ObjectFactory.class );

  public final static ObjectFactory OBJECT_FACTORY_CATALOG = new ObjectFactory();

  public final Hashtable<Class, IURNGenerator> m_urnGenerators = new Hashtable<Class, IURNGenerator>();

  private final File m_baseDir;

  private final Hashtable<URI, DynamicCatalog> m_openCatalogs = new Hashtable<URI, DynamicCatalog>();

  /**
   * Normally you should not instantiate this manager yourself but get it via
   * {@link org.kalypso.core.KalypsoCorePlugin#getCatalogManager()}
   */
  public CatalogManager( final File baseDir )
  {
    m_baseDir = baseDir;
  }

  public void register( final IURNGenerator urnGenerator )
  {
    final Class key = urnGenerator.getSupportingClass();
    if( m_urnGenerators.containsKey( key ) )
      throw new UnsupportedOperationException( "URNGenerator allready registeref for class " + key.toString() );
    m_urnGenerators.put( key, urnGenerator );
  }

  public ICatalog getBaseCatalog( )
  {
    final String baseURN = new String( "urn:" );
    final String path = CatalogUtilities.getPathForCatalog( baseURN );

    final File catalogFile = new File( m_baseDir, path );

    try
    {
      ensureExisting( baseURN );
      return getCatalog( catalogFile.toURI() );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      return null;
    }
  }

  @SuppressWarnings("unchecked")
  public ICatalog getCatalog( final URI catalogURI )
  {
    InputStream is = null;
    try
    {
      if( !m_openCatalogs.containsKey( catalogURI ) )
      {
        // load existing
        final URL catalogURL = catalogURI.toURL();
        final Unmarshaller unmarshaller = JAX_CONTEXT_CATALOG.createUnmarshaller();

        // REMARK: do not use 'unmarshaller.unmarshal( catalogURL )'
        // It does leave the stream open
        is = catalogURL.openStream();
        final JAXBElement<Catalog> object = (JAXBElement<Catalog>) unmarshaller.unmarshal( is );
        is.close();

        final Catalog catalog = object.getValue();

        final DynamicCatalog newOpenCatalog = new DynamicCatalog( this, catalogURL, catalog );
        m_openCatalogs.put( catalogURI, newOpenCatalog );
      }
      return m_openCatalogs.get( catalogURI );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      // TODO generate new type of exception CatalogException
      throw new UnsupportedOperationException( e );
    }
    finally
    {
      IOUtils.closeQuietly( is );
    }
  }

  /**
   * saves all open catalogs
   */
  public void saveAll( )
  {
    final List<URI> catalogsToClose = new ArrayList<URI>();
    for( final ICatalog catalog : m_openCatalogs.values() )
    {
      try
      {
        if( catalog instanceof DynamicCatalog )
        {
          final DynamicCatalog dynCatalog = (DynamicCatalog) catalog;
          dynCatalog.save( m_baseDir );
          final URL location = dynCatalog.getLocation();
          catalogsToClose.add( location.toURI() );
        }
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
    }
    // close catalogs that have been saved
    for( final URI catalogURI : catalogsToClose )
      m_openCatalogs.remove( catalogURI );
  }

  public void ensureExisting( final String baseURN ) throws MalformedURLException, URISyntaxException
  {
    if( !baseURN.endsWith( ":" ) )
      throw new UnsupportedOperationException( "catalog URN must end with ':' " + baseURN );

    final String href = CatalogUtilities.getPathForCatalog( baseURN );
    final URL catalogURL = new URL( m_baseDir.toURL(), href );
    // TODO: problem: not all urls are uris (example: file with spaces), so we might get an URISyntaxException here
    final URI catalogURI = catalogURL.toURI();
    if( m_openCatalogs.containsKey( catalogURI ) )
      return;
    boolean exists = true;
    InputStream stream = null;
    try
    {
      stream = catalogURL.openStream();
    }
    catch( Exception e )
    {
      exists = false;
    }
    finally
    {
      IOUtils.closeQuietly( stream );
    }

    if( exists )
      return;
    // create
    final ObjectFactory catalogFac = new ObjectFactory();
    final Catalog catalog = catalogFac.createCatalog();
    final Map<QName, String> attributes = catalog.getOtherAttributes();
    // convert URN to path
    // final String path = CatalogUtilities.getPathForCatalog( baseURN );

    attributes.put( CatalogUtilities.BASE, baseURN );
    catalog.setId( baseURN );

    // final File catalogFile = new File( m_baseDir, path );
    // final URL catalogURL = catalogFile.toURL();
    final DynamicCatalog newOpenCatalog = new DynamicCatalog( this, catalogURL, catalog );
    m_openCatalogs.put( catalogURI, newOpenCatalog );
  }

  /**
   * @see org.kalypso.core.catalog.IURNGenerator#generateURNFor(java.lang.Object)
   */
  public IURNGenerator getURNGeneratorFor( final Class supportingClass )
  {
    return m_urnGenerators.get( supportingClass );
  }
}
