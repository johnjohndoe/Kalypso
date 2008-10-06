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
import oasis.names.tc.entity.xmlns.xml.catalog.NextCatalog;
import oasis.names.tc.entity.xmlns.xml.catalog.Public;
import oasis.names.tc.entity.xmlns.xml.catalog.System;

import org.apache.commons.io.IOUtils;
import org.kalypso.commons.java.io.FileUtilities;
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
   * system entry matches the specified system identifier of <i>imported</i> catalogs (next catalog), it is used
   */
  private static final int SYSTEM_RESOLVE_NEXT_CATALOG_2 = 2;

  /**
   * no system entry matches the specified system identifier<br>
   * but a rewrite entry matches, it is used
   */
  private static final int SYSTEM_REWRITE_3 = 3;

  /**
   * public entry matches the specified public identifier and either prefer is public or no system identifier is
   * provided, it is used.
   */
  private static final int PUBLIC_RESOLVE_4 = 4;

  /**
   * no exact match was found, but it matches one or more of the partial identifiers specified in delegates entries, the
   * delegate catalogs are searched for a matching identifier
   */
  private static final int DELEGATE_SEARCH_5 = 5;

  private final Catalog m_catalog;

  private URL m_context = null;

  private final CatalogManager m_manager;

  protected DynamicCatalog( final CatalogManager manager, final URL context, final Catalog catalog )
  {
    m_manager = manager;
    m_catalog = catalog;
    m_context = context;
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

  private void internalAddEntry( String uri, final String systemID, final String publicID, final boolean relative )
  {
    // TODO check if entry already exists
    if( uri == null || uri.length() < 1 )
      throw new UnsupportedOperationException();
    if( relative )
    {
      try
      {
        final URI uri2 = new URI( uri );
        final String absolute = uri2.toURL().toExternalForm();
        final String relativePath = FileUtilities.getRelativePathTo( m_context.toExternalForm(), absolute );
        uri = relativePath;
      }
      catch( final Exception e )
      {
        // no
      }
    }
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
   * @see org.kalypso.core.catalog.ICatalog#addNextCatalog(java.net.URL)
   */
  public void addNextCatalog( final URL catalogURL )
  {
    final List<Object> publicOrSystemOrUri = m_catalog.getPublicOrSystemOrUri();
    final NextCatalog nextCatalog = CatalogManager.OBJECT_FACTORY_CATALOG.createNextCatalog();
    nextCatalog.setCatalog( catalogURL.toExternalForm() );
    final JAXBElement<NextCatalog> element = CatalogManager.OBJECT_FACTORY_CATALOG.createNextCatalog( nextCatalog );
    publicOrSystemOrUri.add( element );
  }

  /**
   * @see org.kalypso.core.catalog.ICatalog#addEntry(java.lang.String, java.lang.String, java.lang.String)
   */
  public void addEntry( final String uri, final String systemID, final String publicID )
  {
    if( systemID != null && !"".equals( systemID ) )
      addEntry( uri, systemID, SYSTEM_ID, false );
    if( publicID != null && !"".equals( publicID ) )
      addEntry( uri, publicID, PUBLIC_ID, false );
  }

  private static final int SYSTEM_ID = 1;

  private static final int PUBLIC_ID = 2;

  /**
   * @see org.kalypso.core.catalog.ICatalog#addEntryRelative(java.lang.String, java.lang.String, java.lang.String)
   */
  public void addEntryRelative( final String uri, final String systemID, final String publicID )
  {
    if( systemID != null && !"".equals( systemID ) )
      addEntry( uri, systemID, SYSTEM_ID, true );
    if( publicID != null && !"".equals( publicID ) )
      addEntry( uri, publicID, PUBLIC_ID, true );
  }

  @SuppressWarnings("unchecked")
  private void addEntry( final String uri, final String entryID, final int entryType, final boolean relative )
  {
    if( entryID == null || "".equals( entryID ) )
      return;
    if( !entryID.startsWith( "urn:" ) )
    {
      switch( entryType )
      {
        case SYSTEM_ID:
          internalAddEntry( uri, entryID, null, relative );
          break;
        case PUBLIC_ID:
          internalAddEntry( uri, null, entryID, relative );
          break;
        default:
          throw new UnsupportedOperationException();
      }
      return;
    }
    final int max = CatalogUtilities.getMaxLevel( entryID );
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
          internalAddEntry( uri, entryID, null, relative );
          break;
        case PUBLIC_ID:
          internalAddEntry( uri, null, entryID, relative );
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
          // delegate matches
          final String catalogID = delegateURI.getCatalog();
          final String uriToCatalog = resolve( catalogID, catalogID );
          final ICatalog catalog;
          try
          {
            catalog = m_manager.getCatalog( new URI( uriToCatalog ) );
            switch( entryType )
            {
              case SYSTEM_ID:
                if( relative )
                  catalog.addEntryRelative( uri, entryID, null );
                else
                  catalog.addEntry( uri, entryID, null );
                break;
              case PUBLIC_ID:
                if( relative )
                  catalog.addEntryRelative( uri, null, entryID );
                else
                  catalog.addEntry( uri, null, entryID );
                break;
              default:
                throw new UnsupportedOperationException();
            }
            return;
          }
          catch( final URISyntaxException e )
          {
            e.printStackTrace();
            throw new UnsupportedOperationException( e );
          }
        }
      }
    }

    // catalog seems to be non existing
    final int maxLevel = CatalogUtilities.getMaxLevel( myBaseURN );
    final String urnSection = CatalogUtilities.getUrnSection( baseURN, maxLevel + 1 );
    final String newCatalogURIBase = CatalogUtilities.addURNSection( myBaseURN, urnSection ) + ":";
    final String newCatalogURN = CatalogUtilities.createCatalogURN( newCatalogURIBase );
    try
    {
      m_manager.ensureExisting( newCatalogURIBase );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new UnsupportedOperationException( e );
    }

    // create a delegate to a new catalog
    // the catalog will be created on demand
    final DelegateURI catalogURIEntry = CatalogManager.OBJECT_FACTORY_CATALOG.createDelegateURI();
    catalogURIEntry.setUriStartString( newCatalogURIBase );
    catalogURIEntry.setCatalog( newCatalogURN );
    publicOrSystemOrUri.add( CatalogManager.OBJECT_FACTORY_CATALOG.createDelegateURI( catalogURIEntry ) );

    final System catalogSystemEntry = CatalogManager.OBJECT_FACTORY_CATALOG.createSystem();
    catalogSystemEntry.setSystemId( newCatalogURN );
    catalogSystemEntry.setUri( urnSection + "/" + CatalogUtilities.CATALOG_FILE_NAME );
    publicOrSystemOrUri.add( CatalogManager.OBJECT_FACTORY_CATALOG.createSystem( catalogSystemEntry ) );
    // next time catalog will be available
    addEntry( uri, entryID, entryType, relative );
  }

  /**
   * resolve relative to the catalog, used to resolve relative uri-entries inside the catalog
   */
  private URL resolve( final String href ) throws MalformedURLException
  {
    return UrlResolverSingleton.resolveUrl( m_context, href );
  }

  public String resolve( final String systemID, final String publicID )
  {
    return resolve( systemID, publicID, true );
  }

  public String resolve( final String systemID, final String publicID, final boolean resolveContext )
  {
    return resolveLocal( systemID, publicID, false, resolveContext );
  }

  private String resolveLocal( final String systemID, final String publicID, final boolean local, final boolean resolveContext )
  {
    final List<String> result = internResolve( systemID, publicID, null, false, false, local, resolveContext );
    if( result.size() > 0 )
      return result.get( 0 );
    return systemID;
  }

  /**
   * @see org.kalypso.core.catalog.ICatalog#getEnryURNS(java.lang.String)
   */
  public List<String> getEntryURNS( final String urnPattern )
  {
    return internResolve( urnPattern, urnPattern, null, true, true, false, true );
  }

  /**
   * @return true results of resolve
   */
  private List<String> internResolveDelegate( final String hrefCatalog, final String systemID, final String publicID, List<String> collector, final boolean doCollectURN, final boolean supportPattern, final boolean resolveContext )
  {
    final String uriToCatalog = resolveLocal( hrefCatalog, hrefCatalog, true, true );
    final URI catalogURI;
    try
    {
      final URL url = new URL( uriToCatalog );
      catalogURI = url.toURI();
      final ICatalog catalog = m_manager.getCatalog( catalogURI );
      if( catalog instanceof DynamicCatalog )
      {
        final DynamicCatalog dynCatalog = (DynamicCatalog) catalog;
        collector = dynCatalog.internResolve( systemID, publicID, collector, doCollectURN, supportPattern, false, resolveContext );
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
        }
      }

    }
    catch( final URISyntaxException e )
    {
      // invalid catalog entry
      e.printStackTrace();
    }
    catch( final MalformedURLException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return collector;
  }

  /**
   * @param resolveContext
   *            If true, the found entry is resolved against the catalogs location, if false it is directly returned.
   */
  @SuppressWarnings("unchecked")
  private List<String> internResolve( final String systemID, final String publicID, List<String> collector, final boolean doCollectURN, final boolean supportPattern, final boolean local, final boolean resolveContext )
  {
    if( collector == null )
      collector = new ArrayList<String>();

    final List<Object> publicOrSystemOrUri = m_catalog.getPublicOrSystemOrUri();
    final List entries = new ArrayList();
    entries.addAll( publicOrSystemOrUri );
    // TODO add entries from imported catalogs or extension-point catalogs
    for( int step = SYSTEM_RESOLVE_1; step <= DELEGATE_SEARCH_5; step++ )
    {
      for( final Object entry : entries )
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
                  if( doCollectURN )
                    collector.add( sysID );
                  else if( !resolveContext )
                  {
                    collector.add( system.getUri() );
                    return collector;
                  }
                  else
                  {
                    final String uri = resolve( system.getUri() ).toExternalForm();
                    collector.add( uri );
                    return collector;
                  }
                }
                catch( final Exception e )
                {
                  e.printStackTrace();
                }
              }
            }
            break;
          case SYSTEM_RESOLVE_NEXT_CATALOG_2:
            if( item instanceof NextCatalog && !local )
            {
              final NextCatalog nextCatalog = (NextCatalog) item;
              final String catalogHref = nextCatalog.getCatalog();
              // TODO: @Andreas: is it ok to stop here if we find something? What, if we have some public/system entries
              // after the nextCatalog entry?
              collector = internResolveDelegate( catalogHref, systemID, publicID, collector, doCollectURN, supportPattern, resolveContext );
              if( !doCollectURN && collector.size() > 0 )
                return collector;
            }
            break;
          case SYSTEM_REWRITE_3:
            // TODO
            break;
          case PUBLIC_RESOLVE_4:
            if( item instanceof Public && publicID != null )
            {
              final Public public_ = (Public) item;
              final String pubID = public_.getPublicId();
              if( matches( publicID, pubID, supportPattern ) )
              {
                try
                {
                  if( doCollectURN )
                    collector.add( pubID );
                  else if( !resolveContext )
                  {
                    collector.add( public_.getUri() );
                    return collector;
                  }
                  else
                  {
                    final String uri = resolve( public_.getUri() ).toExternalForm();
                    collector.add( uri );
                    return collector;
                  }
                }
                catch( final MalformedURLException e )
                {
                  e.printStackTrace();
                }
              }
            }
            break;
          case DELEGATE_SEARCH_5:
            if( item instanceof DelegateURI && !local )
            {
              final DelegateURI delegateURI = (DelegateURI) item;
              final String uriStartString = delegateURI.getUriStartString();
              if( (systemID != null && systemID.startsWith( uriStartString )) //
                  || // 
                  (publicID != null && publicID.startsWith( uriStartString )) )
              {
                final String catalogHref = delegateURI.getCatalog();
                collector = internResolveDelegate( catalogHref, systemID, publicID, collector, doCollectURN, supportPattern, resolveContext );
                if( !doCollectURN && collector.size() > 0 )
                  return collector;
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

  private boolean matches( final String requestedID, final String entryID, boolean supportPattern )
  {
    if( !supportPattern )
      return requestedID.equals( entryID );
    if( requestedID.endsWith( "*" ) )
    {
      final String prefixToTest = requestedID.substring( 0, requestedID.length() - 1 );
      return entryID.startsWith( prefixToTest );
    }
    else
    {
      final String[] requestedStrings = requestedID.split( ":" );
      final String[] entryStrings = entryID.split( ":" );
      if( requestedStrings.length == entryStrings.length )
      {
        for( int i = 0; i < entryStrings.length; i++ )
        {
          final String requestedString = requestedStrings[i];
          if( !requestedString.equals( "*" ) && !requestedString.equals( entryStrings[i] ) )
            return false;
        }
        return true;
      }
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
    catch( final Exception e )
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
    return "XML-Catalog: \n ID=" + m_catalog.getId().toString() + "\n Base=" + getBase() + "\n context: " + m_context.toString();
  }

}
