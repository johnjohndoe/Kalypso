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

import java.io.File;
import java.io.FileWriter;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.Marshaller;
import javax.xml.namespace.QName;

import oasis.names.tc.entity.xmlns.xml.catalog.Catalog;
import oasis.names.tc.entity.xmlns.xml.catalog.DelegateURI;
import oasis.names.tc.entity.xmlns.xml.catalog.Public;
import oasis.names.tc.entity.xmlns.xml.catalog.System;

import org.apache.commons.io.IOUtils;
import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypso.jwsdp.JaxbUtilities;

/**
 * @author doemming
 */
public class DynamicCatalog implements ICatalog
{

  /**
   * system entry matches the specified system identifier, it is used
   */
  private static final int SYSTEM_RESOLVE_1 = 1;

  /**
   * no system entry matches the specified system identifier<br>
   * but a rewrite entry matches, it is used
   */
  private static final int SYSTEM_REWRITE_2 = 2;

  /**
   * public entry matches the specified public identifier and either prefer is public or no system identifier is
   * provided, it is used.
   */
  private static final int PUBLIC_RESOLVE_3 = 3;

  /**
   * no exact match was found, but it matches one or more of the partial identifiers specified in delegates entries, the
   * delegate catalogs are searched for a matching identifier
   */
  private static final int DELEGATE_SEARCH_4 = 4;

  private Catalog m_catalog;

  private URL m_context = null;

  private final CatalogManager m_manager;

  protected DynamicCatalog( CatalogManager manager, final File baseDir, final Catalog catalog )
  {
    m_manager = manager;
    m_catalog = catalog;
    final String base = getBase();
    final String relativaPathForCatalog = CatalogUtilities.getPathForCatalog( base );
    try
    {
      m_context = UrlResolverSingleton.resolveUrl( baseDir.toURL(), relativaPathForCatalog );
    }
    catch( MalformedURLException e )
    {
      // do not throw exception, because context may never be used .e.g. read-only catalog
      e.printStackTrace();
    }
  }

  public String getCatalogID( )
  {
    return m_catalog.getId();
  }

  public String getBase( )
  {
    final Map<QName, String> otherAttributes = m_catalog.getOtherAttributes();
    return otherAttributes.get( CatalogUtilities.BASE );
  }

  private void internalAddEntry( final String uri, final String systemID, final String publicID )
  {
    // TODO check if entry allready exists
    if( uri == null || uri.length() < 1 )
      throw new UnsupportedOperationException();
    final List<Object> publicOrSystemOrUri = m_catalog.getPublicOrSystemOrUri();
    if( systemID != null && systemID.length() > 0 )
    {
      final System system = CatalogManager.OBJECT_FACTORY_CATALOG.createSystem();
      system.setSystemId( systemID );
      system.setUri( uri );
      publicOrSystemOrUri.add( CatalogManager.OBJECT_FACTORY_CATALOG.createSystem( system ) );
    }
    if( publicID != null && publicID.length() > 0 )
    {
      final Public public_ = CatalogManager.OBJECT_FACTORY_CATALOG.createPublic();
      public_.setPublicId( publicID );
      public_.setUri( uri );
      publicOrSystemOrUri.add( CatalogManager.OBJECT_FACTORY_CATALOG.createPublic( public_ ) );
    }
  }

  /**
   * @see org.kalypso.core.catalog.ICatalog#addEntry(java.lang.String, java.lang.String, java.lang.String)
   */
  public void addEntry( final String uri, final String systemID, final String publicID )
  {
    if( systemID != null && !"".equals( systemID ) )
      addEntry( uri, systemID, SYSTEM_ID );
    if( publicID != null && !"".equals( publicID ) )
      addEntry( uri, publicID, PUBLIC_ID );
  }

  private static final int SYSTEM_ID = 1;

  private static final int PUBLIC_ID = 2;

  private void addEntry( final String uri, final String entryID, int entryType ) // , final String publicID )
  {
    if( entryID == null || "".equals( entryID ) )
      return;
    if( !entryID.startsWith( "urn:" ) )
    {
      switch( entryType )
      {
        case SYSTEM_ID:
          internalAddEntry( uri, entryID, null );
          break;
        case PUBLIC_ID:
          internalAddEntry( uri, null, entryID );
          break;
        default:
          throw new UnsupportedOperationException();
      }
      return;
    }
    int max = CatalogUtilities.getMaxLevel( entryID );
    final String baseURN = CatalogUtilities.getUrnSection( entryID, 1, max - 1 ) + ":";
    // check the internal policy for our dynamic catalogs

    // either
    // 1. the requested catalog is this catalog
    // or
    // 2. it is a delegated catalog

    // check 1: this catalog ?
    final String myBaseURN = getBase();
    if( myBaseURN.equals( baseURN ) )
    {
      switch( entryType )
      {
        case SYSTEM_ID:
          internalAddEntry( uri, entryID, null );
          break;
        case PUBLIC_ID:
          internalAddEntry( uri, null, entryID );
          break;
        default:
          throw new UnsupportedOperationException();
      }
      return;
    }

    // check 2: a delegate ?
    final List<Object> publicOrSystemOrUri = m_catalog.getPublicOrSystemOrUri();
    for( final Object object : publicOrSystemOrUri )
    {
      final Object item = ((JAXBElement<Object>) object).getValue();
      if( item instanceof DelegateURI )
      {
        final DelegateURI delegateURI = (DelegateURI) item;
        final String uriStartString = delegateURI.getUriStartString();
        if( baseURN.startsWith( uriStartString ) )
        {
          final String catalogID = delegateURI.getCatalog();
          final String uriToCatalog = resolve( catalogID, catalogID );
          final ICatalog catalog;
          try
          {
            catalog = m_manager.getCatalog( new URI( uriToCatalog ) );
            catalog.addEntry( uri, entryID, null );
            return;
          }
          catch( URISyntaxException e )
          {
            e.printStackTrace();
            throw new UnsupportedOperationException( e );
          }
        }
      }
    }

    // catalog seems to be non existing
    int maxLevel = CatalogUtilities.getMaxLevel( myBaseURN );
    final String urnSection = CatalogUtilities.getUrnSection( baseURN, maxLevel + 1 );
    final String newCatalogURIBase = CatalogUtilities.addURNSection( myBaseURN, urnSection ) + ":";
    final String newCatalogURN = CatalogUtilities.createCatalogURN( newCatalogURIBase );
    m_manager.ensureExisting( newCatalogURIBase );

    // create a delegate to a new catalog
    // the catalog will be created on demand
    final DelegateURI catalogURIEntry = CatalogManager.OBJECT_FACTORY_CATALOG.createDelegateURI();
    catalogURIEntry.setUriStartString( newCatalogURIBase );
    catalogURIEntry.setCatalog( newCatalogURN );
    publicOrSystemOrUri.add( CatalogManager.OBJECT_FACTORY_CATALOG.createDelegateURI( catalogURIEntry ) );

    final System catalogSystemEntry = CatalogManager.OBJECT_FACTORY_CATALOG.createSystem();
    catalogSystemEntry.setSystemId( newCatalogURN );
    catalogSystemEntry.setUri( urnSection + File.separator + CatalogUtilities.CATALOG_FILE_NAME );
    publicOrSystemOrUri.add( CatalogManager.OBJECT_FACTORY_CATALOG.createSystem( catalogSystemEntry ) );
    // next time catalog will be available
    addEntry( uri, entryID, entryType );
  }

  /**
   * resolve relative to the catalog, used to resolve relative uri-entries inside the catalog
   */
  private URL resolve( final String href ) throws MalformedURLException
  {
    return UrlResolverSingleton.resolveUrl( m_context, href );
  }

  /**
   * 
   */
  @SuppressWarnings("unchecked")
  public String resolve( final String systemID, final String publicID )
  {
    final List<String> result = internResolve( systemID, publicID, null, false, false );
    if( result.size() > 0 )
      return result.get( 0 );
    return systemID;
  }

  /**
   * @see org.kalypso.core.catalog.ICatalog#getEnryURNS(java.lang.String)
   */
  public List<String> getEnryURNS( String urnPattern )
  {
    final List<String> result = internResolve( urnPattern, urnPattern, null, true, true );
    return result;
  }

  private List<String> internResolve( final String systemID, final String publicID, List<String> collector, boolean doCollectURN, boolean supportPattern )
  {
    if( collector == null )
      collector = new ArrayList<String>();

    final List<Object> publicOrSystemOrUri = m_catalog.getPublicOrSystemOrUri();
    final List entries = new ArrayList();
    entries.addAll( publicOrSystemOrUri );
    // TODO add entries from imported catalogs or extension-point catalogs
    for( int step = SYSTEM_RESOLVE_1; step <= DELEGATE_SEARCH_4; step++ )
    {
      for( Object entry : entries )
      {
        final Object item = ((JAXBElement<Object>) entry).getValue();
        switch( step )
        {
          case SYSTEM_RESOLVE_1:
            if( item instanceof System && systemID != null )
            {
              final System system = (System) item;
              final String sysID = system.getSystemId();
              if( matches( systemID, sysID, supportPattern ) )
              {
                try
                {
                  final String uri = resolve( system.getUri() ).toExternalForm();
                  if( doCollectURN )
                    collector.add( sysID );
                  else
                  {
                    collector.add( uri );
                    return collector;
                  }
                }
                catch( Exception e )
                {
                  e.printStackTrace();
                }
              }
            }
            break;
          case SYSTEM_REWRITE_2:
            // TODO
            break;
          case PUBLIC_RESOLVE_3:
            if( item instanceof Public && publicID != null )
            {
              final Public public_ = (Public) item;
              final String pubID = public_.getPublicId();
              if( matches( publicID, pubID, supportPattern ) )
              {
                try
                {
                  final String uri = resolve( public_.getUri() ).toExternalForm();
                  if( doCollectURN )
                    collector.add( pubID );
                  else
                  {
                    collector.add( uri );
                    return collector;
                  }
                }
                catch( MalformedURLException e )
                {
                  e.printStackTrace();
                }
              }
            }
            break;
          case DELEGATE_SEARCH_4:
            if( item instanceof DelegateURI )
            {
              final DelegateURI delegateURI = (DelegateURI) item;
              final String uriStartString = delegateURI.getUriStartString();
              if( (systemID.startsWith( uriStartString ) && systemID != null) //
                  || // 
                  (publicID.startsWith( uriStartString ) && publicID != null) )
              {
                final String catalogID = delegateURI.getCatalog();
                String uriToCatalog = resolve( catalogID, catalogID );
                final URI catalogURI;
                try
                {
                  catalogURI = new URI( uriToCatalog );
                  final ICatalog catalog = m_manager.getCatalog( catalogURI );
                  if( catalog instanceof DynamicCatalog )
                  {
                    final DynamicCatalog dynCatalog = (DynamicCatalog) catalog;
                    collector = dynCatalog.internResolve( systemID, publicID, collector, doCollectURN, supportPattern );
                    if( !doCollectURN )
                      return collector;
                  }
                  else
                  {
                    if( doCollectURN )
                    {
                      final String uriFromSystemID = catalog.resolve( systemID, null );
                      if( systemID != null && !systemID.equals( uriFromSystemID ) )
                        collector.add( systemID );
                      final String uriFromPublicID = catalog.resolve( null, publicID );
                      if( publicID != null && !publicID.equals( uriFromPublicID ) )
                        collector.add( publicID );
                    }
                    else
                    {
                      final String uri = catalog.resolve( systemID, publicID );
                      collector.add( uri );
                      return collector;
                    }
                  }

                }
                catch( URISyntaxException e )
                {
                  // invalid catalog entry
                  e.printStackTrace();
                }
              }
            }
            break;
          default:
            break;
        }
      }
    }
    return collector;
  }

  private boolean matches( String requestedID, String entryID, boolean supportPattern )
  {
    if( !supportPattern )
      return requestedID.equals( entryID );
    if( requestedID.endsWith( "*" ) )
    {
      final String prefixToTest = requestedID.substring( 0, requestedID.length() - 1 );
      return entryID.startsWith( prefixToTest );
    }
    return requestedID.equals( entryID );
  }

  void save( final File baseDir ) throws Exception
  {

    final Marshaller marshaller = JaxbUtilities.createMarshaller( CatalogManager.JAX_CONTEXT_CATALOG );
    final String catalogBaseURN = getBase();
    final String path = CatalogUtilities.getPathForCatalog( catalogBaseURN );
    FileWriter writer = null;
    try
    {
      final File file = new File( baseDir, path );
      final File parent = file.getParentFile();
      if( !parent.exists() )
        parent.mkdirs();
      writer = new FileWriter( file );
      final JAXBElement<Catalog> root = CatalogManager.OBJECT_FACTORY_CATALOG.createCatalog( m_catalog );
      marshaller.marshal( root, writer );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw e;
    }
    finally
    {
      IOUtils.closeQuietly( writer );
    }
  }

  public URL getLocation( )
  {
    return m_context;
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    return "XML-Catalog: \n ID=" + m_catalog.getId().toString() + "\n Base=" + getBase();
  }

  /**
   * @see org.kalypso.core.catalog.ICatalog#getEnryURNS(java.lang.String)
   */

}
