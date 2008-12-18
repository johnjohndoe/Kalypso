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
package org.kalypso.gml;

import java.lang.reflect.InvocationTargetException;
import java.net.URL;
import java.util.Map;

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.java.lang.MultiException;
import org.kalypso.contribs.org.xml.sax.DelegateContentHandler;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.GMLSchemaCatalog;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.KalypsoGMLSchemaPlugin;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.IFeatureProviderFactory;
import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;

/**
 * This {@link ContentHandler} implementation parsed a full gml-document.
 * <p>
 * This handler only parses the first line from a gml document and creates the appropriate gml schema from that.
 * <p>
 * All the rest of parsing is delegated to the {@link GMLContentHandler} content handler.
 * </p>
 * 
 * @author Gernot Belger
 */
public class GMLDocumentContentHandler extends DelegateContentHandler
{
  private final static QName XSD_SCHEMALOCATION = new QName( NS.XSD, "schemaLocation" );

  private final static ContentHandler EMPTY_HANLDER = new DefaultHandler();

  private final URL m_context;

  private final URL m_schemaLocationHint;

  private final IFeatureProviderFactory m_providerFactory;

  private final XMLReader m_xmlReader;

  private String m_schemaLocationString = null;

  public GMLDocumentContentHandler( final XMLReader xmlReader, final URL schemaLocationHint, final URL context, final IFeatureProviderFactory providerFactory )
  {
    super( EMPTY_HANLDER );

    m_xmlReader = xmlReader;
    m_schemaLocationHint = schemaLocationHint;
    m_context = context;
    m_providerFactory = providerFactory;
  }

  /**
   * @see org.xml.sax.ContentHandler#startElement(java.lang.String, java.lang.String, java.lang.String,
   *      org.xml.sax.Attributes)
   */
  @Override
  public void startElement( final String uri, final String localName, final String qName, final Attributes atts ) throws SAXException
  {
    if( getDelegate() == EMPTY_HANLDER )
    {
      // first element may have schema-location
      m_schemaLocationString = getSchemaLocation( atts );

      // SK
      // TODO: schemaLocation string should be used for every schema loaded from this context; not only for namespaces
      // defined at the root level
      initGmlSchema( uri, atts, m_schemaLocationString, m_schemaLocationHint, m_context );
      final Map<String, URL> namespaces = GMLSchemaUtilities.parseSchemaLocation( m_schemaLocationString, m_context );
      /* If a localtionHint is given, this precedes any schemaLocation in the GML-File */
      if( m_schemaLocationHint != null )
        namespaces.put( uri, m_schemaLocationHint );

      setDelegate( new GMLContentHandler( m_xmlReader, m_context, namespaces ) );
    }

    super.startElement( uri, localName, qName, atts );
  }

  /**
   * Loads the main application schema and also all (via xmlns) references schemas.
   * <p>
   * TODO: move into helper class.
   * </p>
   */
  private static GMLSchema initGmlSchema( final String uri, final Attributes atts, final String schemaLocationString, final URL locationHint, final URL context ) throws SAXException
  {
    // the main schema is the schema defining the root elements namespace
    // REMARK: schemaLocationHint only used for main schema
    final GMLSchema gmlSchema = loadGMLSchema( uri, null, schemaLocationString, locationHint, context );
    final String version = gmlSchema == null ? null : gmlSchema.getGMLVersion();

    // Also force all dependent schemas (i.e. for which xmlns entries exist) as dependency into
    // the main schema.
    // This allows to introduce necessary schemata (for example which introduce new elements
    // via substitution).
    final int attLength = atts.getLength();
    for( int i = 0; i < attLength; i++ )
    {
      // STRANGE: shouldn't it work like this?
      // if( NS.XML_PREFIX_DEFINITION_XMLNS.equals( atts.getURI( i ) ) )
      // But atts.getURI gives empty string for xmlns entries.
      // so we ask for the qname
      final String qname = atts.getQName( i );
      if( qname != null && qname.startsWith( "xmlns:" ) )
      {
        final String xmlnsUri = atts.getValue( i );
        // HM: are there any other possible namespaces we do NOT want to load?
        if( !xmlnsUri.equals( NS.XSD ) )
        {
          // make sure that all dependent schemas are loaded
          loadGMLSchema( xmlnsUri, version, schemaLocationString, locationHint, context );
        }
      }
    }

    return gmlSchema;
  }

  private static GMLSchema loadGMLSchema( final String uri, final String gmlVersion, final String schemaLocationString, final URL schemaLocationHint, final URL context ) throws SAXException
  {
    final MultiException schemaNotFoundExceptions = new MultiException();

    GMLSchema schema = null;

    // 1. try : use hint
    if( schemaLocationHint != null )
    {
      final GMLSchemaCatalog schemaCatalog = KalypsoGMLSchemaPlugin.getDefault().getSchemaCatalog();
      schema = schemaCatalog.getSchema( null, schemaLocationHint );
    }

    try
    {
      // 2. try : from uri + schemalocation attributes
      if( schema == null )
      {
        final Map<String, URL> namespaces = GMLSchemaUtilities.parseSchemaLocation( schemaLocationString, context );
        final URL schemaLocation = namespaces.get( uri );

        final GMLSchemaCatalog schemaCatalog = KalypsoGMLSchemaPlugin.getDefault().getSchemaCatalog();
        schema = schemaCatalog.getSchema( uri, gmlVersion, schemaLocation );
      }
    }
    catch( final Exception e )
    {
      /* Log it, because the following SaxException eats the inner exception */
      KalypsoDeegreePlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
      if( schema == null )
        schemaNotFoundExceptions.addException( new SAXException( "Schema unknown. Could not load schema with namespace: " + uri + " (schemaLocationHint was " + schemaLocationHint
            + ") (schemaLocation was " + schemaLocationString + "): ", e ) );
    }

    // 3. try
    if( schema == null )
    {
      try
      {
        final GMLSchemaCatalog schemaCatalog = KalypsoGMLSchemaPlugin.getDefault().getSchemaCatalog();
        schema = schemaCatalog.getSchema( uri.toString(), (String) null );
      }
      catch( final Exception e )
      {
        /* Log it, because the following SaxException eats the innner exception */
        KalypsoDeegreePlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
        schemaNotFoundExceptions.addException( new SAXException( "Schema unknown. Could not load schema with namespace: " + uri + " (schemaLocationHint was " + schemaLocationHint
            + ") (schemaLocation was " + schemaLocationString + ")", e ) );
      }
    }

    if( schema == null )
    {
      if( schemaNotFoundExceptions.isEmpty() )
        throw new SAXException( "Schema unknown. Could not load schema with namespace: " + uri + " (schemaLocationHint was " + schemaLocationHint + ") (schemaLocation was " + schemaLocationString
            + ")" );
      else
        throw new SAXException( schemaNotFoundExceptions );
    }

    if( !schemaNotFoundExceptions.isEmpty() )
    {
      System.out.println( "warning: errors occured with schemalocation" );
      schemaNotFoundExceptions.printStackTrace();
    }

    return schema;
  }

  private static String getSchemaLocation( final Attributes atts )
  {
    for( int i = 0; i < atts.getLength(); i++ )
    {
      final QName attQName = new QName( atts.getURI( i ), atts.getLocalName( i ) );
      if( XSD_SCHEMALOCATION.equals( attQName ) )
      {
        final String value = atts.getValue( i );
        return value == null ? null : value.trim();
      }
    }
    // no schemalocation found in attributes
    return null;
  }

  public GMLWorkspace getWorkspace( ) throws GMLException
  {
    try
    {
      final GMLContentHandler delegate = (GMLContentHandler) getDelegate();
      final Feature rootFeature = delegate.getRootFeature();

      /* At this point the schema is in the catalog, as it was put there during gml-parsing */
      final GMLSchemaCatalog schemaCatalog = KalypsoGMLSchemaPlugin.getDefault().getSchemaCatalog();
      final GMLSchema rootSchema = schemaCatalog.getSchema( rootFeature.getFeatureType().getQName().getNamespaceURI(), (String) null );

      return FeatureFactory.createGMLWorkspace( rootSchema, rootFeature, m_context, m_schemaLocationString, m_providerFactory, null );
    }
    catch( final InvocationTargetException e )
    {
      throw new GMLException( e.getTargetException() );
    }
  }

}
