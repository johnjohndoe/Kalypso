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

import org.kalypso.commons.xml.NS;
import org.kalypso.contribs.java.lang.MultiException;
import org.kalypso.contribs.java.util.logging.ILogger;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.GMLSchemaCatalog;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.gmlschema.GMLSchemaFactory;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.tools.FeatureUtils;
import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;

/**
 * to parse gml from xmlReader,
 * 
 * @author doemming
 */
public class GMLContentHandler implements ContentHandler, FeatureTypeProvider
{
  private static final int FIRST_FEATURE = 1;

  /** a new property begins or a feature gets closed */
  private static final int START_PROPERTY_END_FEATURE = 2;

  /** a new value begins or a property gets closed */
  private static final int START_VALUE_END_PROPERTY = 3;

  private FeatureParser m_featureParser;

  private PropertyParser m_propParser;

  private int m_status = 0;

  private int m_indent = 0;

  private GMLSchema m_gmlSchema = null;

  private final static QName XSD_SCHEMALOCATION = new QName( NS.XSD, "schemaLocation" );

  private final XMLReader m_xmlReader;

  private Feature m_rootFeature = null;

  private final URL m_schemaLocationHint;

  private final boolean m_useSchemaCatalog;

  private ToStringContentHandler m_exceptionContentHandler = null;

  StringBuffer m_errorBuffer = new StringBuffer();

  private final URL m_context;

  private String m_schemaLocationString = null;

  /**
   * uses GMLSchemaCatalog
   */
  public GMLContentHandler( XMLReader xmlReader, final URL context )
  {
    m_context = context;
    m_xmlReader = xmlReader;
    m_featureParser = new FeatureParser( this );
    m_propParser = new PropertyParser();
    m_schemaLocationHint = null;
    m_useSchemaCatalog = true;
  }

  public GMLContentHandler( final XMLReader xmlReader, final URL schemaLocationHint, final boolean useGMLSchemaCatalog, final URL context )
  {
    m_xmlReader = xmlReader;
    m_schemaLocationHint = schemaLocationHint;
    m_useSchemaCatalog = useGMLSchemaCatalog;
    m_context = context;
    m_featureParser = new FeatureParser( this );
    m_propParser = new PropertyParser();
  }

  /**
   * @see org.kalypso.gml.FeatureTypeProvider#getFeatureType(javax.xml.namespace.QName)
   */
  public IFeatureType getFeatureType( QName nameFE )
  {
    return m_gmlSchema.getFeatureType( nameFE );
  }

  /**
   * @see org.xml.sax.ContentHandler#startDocument()
   */
  public void startDocument( )
  {
    // GML allways starts with features
    m_status = FIRST_FEATURE;
  }

  /**
   * @see org.xml.sax.ContentHandler#startElement(java.lang.String, java.lang.String, java.lang.String,
   *      org.xml.sax.Attributes)
   */
  public void startElement( String uri, String localName, String qName, Attributes atts ) throws SAXException
  {
    // handle OGC Exceptions
    if( m_exceptionContentHandler != null )
    {
      m_exceptionContentHandler.startElement( uri, localName, qName, atts );
      return;
    }
    // deegree1-service
    if( localName != null && localName.endsWith( "Exception" ) )
    {
      m_exceptionContentHandler = new ToStringContentHandler( new ILogger()
      {

        public void log( String message )
        {
          m_errorBuffer.append( message );
        }

      } );
      m_exceptionContentHandler.startElement( uri, localName, qName, atts );
      return;
    }
    // deegree2-service
    if( localName != null && localName.equals( "ServiceExceptionReport" ) )
    {
      m_exceptionContentHandler = new ToStringContentHandler( new ILogger()
      {

        public void log( String message )
        {
          m_errorBuffer.append( message );
        }

      } );
      m_exceptionContentHandler.startElement( uri, localName, qName, atts );
      return;
    }

    // handle GML
    m_indent++;
    indent();
    final MultiException schemaNotFoundExceptions = new MultiException();
    // System.out.println( "<" + uri + ":" + localName + ">" );
    if( m_gmlSchema == null )
    {
      // first element may have schemalocation
      m_schemaLocationString = getSchemalocation( atts );

      GMLSchema schema = null;

      // 1. try : use hint
      if( m_schemaLocationHint != null )
      {
        try
        {
          if( m_useSchemaCatalog )
            schema = GMLSchemaCatalog.getSchema( m_schemaLocationHint );
          else
            schema = GMLSchemaFactory.createGMLSchema( m_schemaLocationHint );
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
          final Map<String, URL> namespaces = GMLSchemaUtilities.parseSchemaLocation( m_schemaLocationString, m_context );
          final URL schemaLocation = namespaces.get( uri );

          if( m_useSchemaCatalog )
            schema = GMLSchemaCatalog.getSchema( uri, schemaLocation );
          else if( schemaLocation != null )
            schema = GMLSchemaFactory.createGMLSchema( schemaLocation );
        }
      }
      catch( final Exception e )
      {
        if( schema == null )
          schemaNotFoundExceptions.addException( new SAXException( "Schema unknown. Could not load schema with namespace: " + uri + " (schemaLocationHint was " + m_schemaLocationHint
              + ") (schemaLocation was " + m_schemaLocationString + ")", e ) );
      }

      // 3. try
      if( schema == null && m_useSchemaCatalog )
        try
        {
          schema = GMLSchemaCatalog.getSchema( uri.toString() );
        }
        catch( Exception e )
        {
          schemaNotFoundExceptions.addException( new SAXException( "Schema unknown. Could not load schema with namespace: " + uri + " (schemaLocationHint was " + m_schemaLocationHint
              + ") (schemaLocation was " + m_schemaLocationString + ")", e ) );
        }

      if( schema == null )
      {
        if( schemaNotFoundExceptions.isEmpty() )
          throw new SAXException( "Schema unknown. Could not load schema with namespace: " + uri + " (schemaLocationHint was " + m_schemaLocationHint + ") (schemaLocation was "
              + m_schemaLocationString + ")" );
        else
          throw new SAXException( schemaNotFoundExceptions );

      }
      if( !schemaNotFoundExceptions.isEmpty() )
      {
        System.out.println( "warning: errors occured with schemalocation" );
        schemaNotFoundExceptions.printStackTrace();
      }
      m_gmlSchema = schema;
    }

    if( uri == null || uri.length() < 1 )
      uri = m_gmlSchema.getTargetNamespace();

    switch( m_status )
    {
      case FIRST_FEATURE:
      {
        try
        {
          m_featureParser.createFeature( null, uri, localName, atts );
        }
        catch( final GMLException e )
        {
          throw new SAXException( e );
        }

        m_rootFeature = m_featureParser.getCurrentFeature();
        m_status = START_PROPERTY_END_FEATURE;
        break;
      }

      case START_PROPERTY_END_FEATURE:
      {
        final Feature feature = m_featureParser.getCurrentFeature();
        m_propParser.createProperty( feature, uri, localName, atts );

        final IPropertyType pt = m_propParser.getCurrentPropertyType();
        // final Feature parentFE = m_featureParser.getCurrentFeature();

        if( pt instanceof IValuePropertyType )
        {
          final IValuePropertyType vpt = (IValuePropertyType) pt;

          m_propParser.setContent( feature, vpt, m_xmlReader, uri, localName, qName, atts );
          // we skip the end tag
        }
        else if( pt instanceof IRelationType )// its a relation
        {
          int index = atts.getIndex( NS.XLINK, "href" );
          if( index >= 0 )// its a xlink
          {
            final IRelationType rt = (IRelationType) pt;

            final String href = atts.getValue( index );
            // final String role = getAttributeValue( atts, NS.XLINK, "role", null );
            // final String arcrole = getAttributeValue( atts, NS.XLINK, "arcrole", null );
            // final String title = getAttributeValue( atts, NS.XLINK, "title", null );
            // final String show = getAttributeValue( atts, NS.XLINK, "show", "replace" );
            // final String actuate = getAttributeValue( atts, NS.XLINK, "actuate", "onRequest" );

            // TODO: replace String with ProxyFeature implementation
            // final IFeatureProvider featureProvider = new XLinkFeatureProvider( feature, rt.getTargetFeatureType(),
            // href, role, arcrole, title, show, actuate );
            // new DelegatedFeature_Impl( feature, featureProvider );

            final String refID2 = href.replaceAll( "^#", "" );
            FeatureUtils.addChild( feature, rt, refID2 );
          }
        }
        else
        {
          System.out.println( "unknown: " + uri + " " + localName );
          // unknown element in schema, probably this property is removed
          // from schema and still occurs in the xml
          // instance document
          // we just ignore it
        }

        m_status = START_VALUE_END_PROPERTY;
        break;
      }

      case START_VALUE_END_PROPERTY:
      {
        final IPropertyType pt = m_propParser.getCurrentPropertyType();
        final Feature parentFE = m_featureParser.getCurrentFeature();
        if( pt instanceof IRelationType )
        {
          try
          {
            m_featureParser.createFeature( parentFE, uri, localName, atts );
          }
          catch( final GMLException e )
          {
            throw new SAXException( e );
          }

          final Feature childFE = m_featureParser.getCurrentFeature();
          FeatureUtils.addChild( parentFE, (IRelationType) pt, childFE );
          m_status = START_PROPERTY_END_FEATURE;
        }
        else
          throw new SAXException( "GML Type not supported for: " + qName ); // they sould not be
        // parsed here
        // else
        // m_propParser.setContent( parentFE, (IValuePropertyType) pt, uri,
        // localName, qName, atts );
        break;
      }

      default:
        break;
    }
  }

  // DONT REMOVE: used by proto-implementation of DelegateFeature obove
  // private String getAttributeValue( final Attributes atts, final String uri, final String localName, final String
  // defaultValue )
  // {
  // final String value = atts.getValue( uri, localName );
  // return value == null ? defaultValue : value;
  // }

  /**
   * @see org.xml.sax.ContentHandler#endElement(java.lang.String, java.lang.String, java.lang.String)
   */
  public void endElement( String uri, String localName, String qName )
  {
    if( m_exceptionContentHandler != null )
    {
      m_exceptionContentHandler.endElement( uri, localName, qName );
      return;
    }

    if( uri == null || uri.length() < 1 )
      uri = m_gmlSchema.getTargetNamespace();

    indent();
    // System.out.println( "</" + uri + ":" + localName + ">" );
    m_indent--;
    switch( m_status )
    {
      case FIRST_FEATURE:
        m_status = START_VALUE_END_PROPERTY;
        break;
      case START_PROPERTY_END_FEATURE:
        m_featureParser.popFeature();
        m_status = START_VALUE_END_PROPERTY;
        break;
      case START_VALUE_END_PROPERTY:
        m_propParser.popPT();
        m_status = START_PROPERTY_END_FEATURE;
        break;
      default:
        break;
    }
  }

  private void indent( )
  {
    // System.out.print( StringUtils.repeat( " ", m_indent ) );
  }

  /**
   * @see org.xml.sax.ContentHandler#ignorableWhitespace(char[], int, int)
   */
  public void ignorableWhitespace( char[] ch, int start, int length )
  {
    if( m_exceptionContentHandler != null )
    {
      m_exceptionContentHandler.ignorableWhitespace( ch, start, length );
      return;
    }
    // TODO call characters() from here ??
  }

  /**
   * @see org.xml.sax.ContentHandler#characters(char[], int, int)
   */
  public void characters( char[] ch, int start, int length )
  {
    if( m_exceptionContentHandler != null )
    {
      m_exceptionContentHandler.characters( ch, start, length );
      return;
    }

    final Feature feature = m_featureParser.getCurrentFeature();
    switch( m_status )
    {
      case FIRST_FEATURE:
        // nothing
        break;
      case START_PROPERTY_END_FEATURE:
        // nothing
        break;
      case START_VALUE_END_PROPERTY:
        final StringBuffer buffer = new StringBuffer();
        for( int i = start; i < length; i++ )
          buffer.append( ch[i] );

        m_propParser.setContent( feature, buffer.toString() );
        break;
      default:
        break;
    }
  }

  /**
   * @see org.xml.sax.ContentHandler#startPrefixMapping(java.lang.String, java.lang.String)
   */
  public void startPrefixMapping( String prefix, String uri )
  {
    if( m_exceptionContentHandler != null )
      m_exceptionContentHandler.startPrefixMapping( prefix, uri );
  }

  /**
   * @see org.xml.sax.ContentHandler#endDocument()
   */
  public void endDocument( )
  {
    if( m_exceptionContentHandler != null )
      m_exceptionContentHandler.endDocument();
  }

  /**
   * @see org.xml.sax.ContentHandler#endPrefixMapping(java.lang.String)
   */
  public void endPrefixMapping( String prefix )
  {
    if( m_exceptionContentHandler != null )
      m_exceptionContentHandler.endPrefixMapping( prefix );
  }

  /**
   * @see org.xml.sax.ContentHandler#processingInstruction(java.lang.String, java.lang.String)
   */
  public void processingInstruction( String target, String data )
  {
    if( m_exceptionContentHandler != null )
      m_exceptionContentHandler.processingInstruction( target, data );
  }

  /**
   * @see org.xml.sax.ContentHandler#setDocumentLocator(org.xml.sax.Locator)
   */
  public void setDocumentLocator( Locator locator )
  {
    if( m_exceptionContentHandler != null )
      m_exceptionContentHandler.setDocumentLocator( locator );
  }

  /**
   * @see org.xml.sax.ContentHandler#skippedEntity(java.lang.String)
   */
  public void skippedEntity( String name )
  {
    if( m_exceptionContentHandler != null )
      m_exceptionContentHandler.skippedEntity( name );
  }

  private String getSchemalocation( Attributes atts )
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

  public GMLSchema getGMLSchema( )
  {
    return m_gmlSchema;
  }

  public Feature getRootFeature( ) throws GMLException
  {
    if( m_rootFeature != null )
      return m_rootFeature;

    if( m_exceptionContentHandler != null )
      throw new GMLException( m_errorBuffer.toString() );

    throw new GMLException( "Could not load GML, Root-Feature was not created." );
  }

  public String getSchemaLocationString( )
  {
    return m_schemaLocationString;
  }
}
