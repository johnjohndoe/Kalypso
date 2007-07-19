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

import java.net.URL;
import java.util.Map;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.xml.NS;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.java.lang.MultiException;
import org.kalypso.contribs.org.xml.sax.DelegateContentHandler;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.GMLSchemaCatalog;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.gmlschema.GMLSchemaFactory;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.IGMLSchema;
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

  private final boolean m_useSchemaCatalog;

  private final URL m_context;

  private final URL m_schemaLocationHint;

  private final IFeatureProviderFactory m_providerFactory;

  private final XMLReader m_xmlReader;

  private IGMLSchema m_gmlSchema = null;

  private String m_schemaLocationString = null;

  public GMLDocumentContentHandler( final XMLReader xmlReader, final URL schemaLocationHint, final boolean useGMLSchemaCatalog, final URL context, final IFeatureProviderFactory providerFactory )
  {
    super( EMPTY_HANLDER );

    m_xmlReader = xmlReader;
    m_schemaLocationHint = schemaLocationHint;
    m_useSchemaCatalog = useGMLSchemaCatalog;
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
      // first element may have schemalocation
      m_schemaLocationString = getSchemaLocation( atts );

      m_gmlSchema = initGmlSchema( uri, atts, m_schemaLocationString, m_schemaLocationHint, m_useSchemaCatalog, m_context );
      /* This must happen only for the root element. If we fail an exception MUST be thrown. */
      Assert.isNotNull( m_gmlSchema );

      setDelegate( new GMLContentHandler( m_xmlReader, m_context, m_gmlSchema ) );
    }

    super.startElement( uri, localName, qName, atts );
  }

  /**
   * Loads the main application schema and also all (via xmlns) references schemas.
   * <p>
   * TODO: move into helper class.
   * </p>
   */
  private static GMLSchema initGmlSchema( final String uri, final Attributes atts, final String schemaLocationString, final URL locationHint, final boolean useSchemaCatalog, final URL context ) throws SAXException
  {
    // the main schema is the schema defining the root elements namespace
    // REMARK: schemaLocationHint only used for main schema
    final GMLSchema gmlSchema = loadGMLSchema( uri, schemaLocationString, locationHint, useSchemaCatalog, context );

    // Also force all dependent schemas (i.e. for which xmlns entries exist) as dependency into
    // the main schema.
    // This allows to introduce necessary schemata (for example which introduce new elements
    // vis substitution).
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
        // HM: are there any other possible namespaces wo do NOT want to load?
        if( !xmlnsUri.equals( NS.XSD ) )
        {
          try
          {
            gmlSchema.getGMLSchemaForNamespaceURI( xmlnsUri );
          }
          catch( final GMLSchemaException e )
          {
            // Just log it, this is pobably not a critical error
            final IStatus status = StatusUtilities.statusFromThrowable( e );
            KalypsoDeegreePlugin.getDefault().getLog().log( status );
          }
        }
      }
    }

    return gmlSchema;
  }

  private static GMLSchema loadGMLSchema( final String uri, final String schemaLocationString, final URL schemaLocationHint, final boolean useSchemaCatalog, final URL context ) throws SAXException
  {
    final MultiException schemaNotFoundExceptions = new MultiException();

    GMLSchema schema = null;

    // 1. try : use hint
    if( schemaLocationHint != null )
    {
      try
      {
        if( useSchemaCatalog )
        {
          final GMLSchemaCatalog schemaCatalog = KalypsoGMLSchemaPlugin.getDefault().getSchemaCatalog();
          schema = schemaCatalog.getSchema( null, schemaLocationHint );
        }
        else
          schema = GMLSchemaFactory.createGMLSchema( null, schemaLocationHint );
      }
      catch( final GMLSchemaException e )
      {
        schemaNotFoundExceptions.addException( new SAXException( e ) );
      }
    }

    try
    {
      // 2. try : from uri + schemalocation attributes
      if( schema == null )
      {
        final Map<String, URL> namespaces = GMLSchemaUtilities.parseSchemaLocation( schemaLocationString, context );
        final URL schemaLocation = namespaces.get( uri );

        if( useSchemaCatalog )
        {
          final GMLSchemaCatalog schemaCatalog = KalypsoGMLSchemaPlugin.getDefault().getSchemaCatalog();
          schema = schemaCatalog.getSchema( uri, null, schemaLocation );
        }
        else if( schemaLocation != null )
          schema = GMLSchemaFactory.createGMLSchema( null, schemaLocation );
      }
    }
    catch( final Exception e )
    {
      /* Log it, because the following SaxException eats the innner exception */
      KalypsoDeegreePlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
      if( schema == null )
        schemaNotFoundExceptions.addException( new SAXException( "Schema unknown. Could not load schema with namespace: " + uri + " (schemaLocationHint was " + schemaLocationHint
            + ") (schemaLocation was " + schemaLocationString + "): ", e ) );
    }

    // 3. try
    if( schema == null && useSchemaCatalog )
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
    final GMLContentHandler delegate = (GMLContentHandler) getDelegate();
    final Feature rootFeature = delegate.getRootFeature();
    final GMLWorkspace workspace = FeatureFactory.createGMLWorkspace( m_gmlSchema, rootFeature, m_context, m_schemaLocationString, m_providerFactory );

    return workspace;
  }

}
