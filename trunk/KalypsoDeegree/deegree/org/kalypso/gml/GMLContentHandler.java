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

import javax.xml.namespace.QName;

import org.kalypso.commons.java.net.UrlResolver;
import org.kalypso.commons.xml.NS;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.GMLSchemaCatalog;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.gmlschema.GMLSchemaFactory;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.gml.GMLException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.tools.FeatureUtils;
import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.Locator;
import org.xml.sax.XMLReader;

/**
 * to parse gml from xmlReader,
 * 
 * @author doemming
 */
public class GMLContentHandler implements ContentHandler, FeatureTypeProvider
{
  private static final int FIRST_FEATURE = 1;

  private static final int START_PROPERTY_END_FEATURE = 2;

  private static final int START_VALUE_END_PROPERTY = 3;

  private FeatureParser m_featureParser;

  private PropertyParser m_propParser;

  private int m_status = 0;

  private int m_indent = 0;

  private GMLSchema m_gmlSchema = null;

  private final static QName XSD_SCHEMALOCATION = new QName( NS.NS_XSD, "schemaLocation" );

  private final XMLReader m_xmlReader;

  private Feature m_rootFeature = null;

  private final URL m_schemaLocationHint;

  // 
  private final boolean m_useSchemaCatalog;

  private ToStringContentHandler m_exceptionContentHandler = null;

  private final URL m_context;

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
  public void startElement( String uri, String localName, String qName, Attributes atts )
  {
    if( m_exceptionContentHandler != null )
    {
      m_exceptionContentHandler.startElement( uri, localName, qName, atts );
      return;
    }
    if( localName != null && localName.endsWith( "Exception" ) )
    {
      m_exceptionContentHandler = new ToStringContentHandler();
      m_exceptionContentHandler.startElement( uri, localName, qName, atts );
      return;
    }
    // TODO test if first element is a OGC Exception
    m_indent++;
    indent();
    // System.out.println( "<" + uri + ":" + localName + ">" );
    if( m_gmlSchema == null )
    {
      GMLSchema schema = null;
      // 1. try : use hint
      if( m_schemaLocationHint != null )
        try
        {
          if( m_useSchemaCatalog )
            schema = GMLSchemaCatalog.getSchema( m_schemaLocationHint );
          else
            schema = GMLSchemaFactory.createGMLSchema( m_schemaLocationHint );
        }
        catch( GMLSchemaException e )
        {
          // TODO Auto-generated catch block
        }
      // 2. try : from schemalocation attributes
      if( schema == null )
        schema = getSchema( atts );
      // 3. try : from namespace of root element
      if( schema == null && m_useSchemaCatalog )
        schema = GMLSchemaCatalog.getSchema( uri );

      // TODO: maybe schema is unknown, so better throw an exception with a better error message
      if( schema == null )
        throw new UnsupportedOperationException( "could not load schema" );
      m_gmlSchema = schema;
    }

    if( uri == null || uri.length() < 1 )
      uri = m_gmlSchema.getTargetNamespace();

    switch( m_status )
    {
      case FIRST_FEATURE:
        m_featureParser.createFeature( uri, localName, atts );
        m_rootFeature = m_featureParser.getCurrentFeature();
        m_status = START_PROPERTY_END_FEATURE;
        break;
      case START_PROPERTY_END_FEATURE:
      {
        final Feature feature = m_featureParser.getCurrentFeature();
        m_propParser.createProperty( feature, uri, localName, atts );
        // 
        final IPropertyType pt = m_propParser.getCurrentPropertyType();
        final Feature parentFE = m_featureParser.getCurrentFeature();
        if( pt instanceof IValuePropertyType )
        {
          final IValuePropertyType vpt = (IValuePropertyType) pt;
          m_propParser.setContent( parentFE, vpt, m_xmlReader, uri, localName, qName, atts );
          // we skip the end tag
        }
        else if( pt instanceof IRelationType )// its a relation
        {
          int index = atts.getIndex( NS.XLINK, "href" );
          if( index >= 0 )// its a xlink
          {
            final String refID = atts.getValue( index );// #
            String refID2 = refID.replaceAll( "^#", "" );
            FeatureUtils.addChild( parentFE, (IRelationType) pt, refID2 );
          }
        }
        else
        {
          // System.out.println( "unknown: " + uri + " " + localName );
          // unknown element in schema, probably this property is removed from schema and still occurs in the xml
          // instance document
          // we just ignore it
        }

        m_status = START_VALUE_END_PROPERTY;
      }
        break;
      case START_VALUE_END_PROPERTY:
      {
        final IPropertyType pt = m_propParser.getCurrentPropertyType();
        final Feature parentFE = m_featureParser.getCurrentFeature();
        if( pt instanceof IRelationType )
        {
          m_featureParser.createFeature( uri, localName, atts );
          final Feature childFE = m_featureParser.getCurrentFeature();
          FeatureUtils.addChild( parentFE, (IRelationType) pt, childFE );
          m_status = START_PROPERTY_END_FEATURE;
        }
        else
          throw new UnsupportedOperationException(); // they sould not be parsed here
        // else
        // m_propParser.setContent( parentFE, (IValuePropertyType) pt, uri, localName, qName, atts );
      }
        break;
      default:
        break;
    }
  }

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
    // System.out.println( "debug" );
    // nothing
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
    {
      m_exceptionContentHandler.startPrefixMapping( prefix, uri );
      return;
    }
    // nothing
  }

  /**
   * @see org.xml.sax.ContentHandler#endDocument()
   */
  public void endDocument( )
  {
    if( m_exceptionContentHandler != null )
    {
      m_exceptionContentHandler.endDocument();
      return;
    }
    // System.out.println( "debug" );
    // TODO finish
  }

  /**
   * @see org.xml.sax.ContentHandler#endPrefixMapping(java.lang.String)
   */
  public void endPrefixMapping( String prefix )
  {
    if( m_exceptionContentHandler != null )
    {
      m_exceptionContentHandler.endPrefixMapping( prefix );
      return;
    }
  }

  /**
   * @see org.xml.sax.ContentHandler#processingInstruction(java.lang.String, java.lang.String)
   */
  public void processingInstruction( String target, String data )
  {
    if( m_exceptionContentHandler != null )
    {
      m_exceptionContentHandler.processingInstruction( target, data );
      return;
    }
    // nothing
  }

  /**
   * @see org.xml.sax.ContentHandler#setDocumentLocator(org.xml.sax.Locator)
   */
  public void setDocumentLocator( Locator locator )
  {
    if( m_exceptionContentHandler != null )
    {
      m_exceptionContentHandler.setDocumentLocator( locator );
      return;
    }
  }

  /**
   * @see org.xml.sax.ContentHandler#skippedEntity(java.lang.String)
   */
  public void skippedEntity( String name )
  {
    if( m_exceptionContentHandler != null )
    {
      m_exceptionContentHandler.skippedEntity( name );
      return;
    }
    // nothing
  }

  private GMLSchema getSchema( Attributes atts )
  {
    // get schema from schemalocation
    int length = atts.getLength();
    for( int i = 0; i < length; i++ )
    {
      // System.out.println( "debug" );
      final QName attQName = new QName( atts.getURI( i ), atts.getLocalName( i ) );
      if( XSD_SCHEMALOCATION.equals( attQName ) )
      {
        final String locationValue = atts.getValue( i );
        final String[] strings = locationValue.split( "\\s+" );
        GMLSchema schema = null;
        if( m_useSchemaCatalog )
          try
          {
            String namespaceURI = strings[0];
            schema = GMLSchemaCatalog.getSchema( namespaceURI );
            if( schema != null )
              return schema;
          }
          catch( Exception e )
          {
            e.printStackTrace();
          }

        final URL schemaLocationURL;
        try
        {
          // TODO use context
          final String locationURI = strings[1];
          if( m_context != null )
          {
            UrlResolver resolver = new UrlResolver();
            schemaLocationURL = resolver.resolveURL( m_context, locationURI );
          }
          else
            schemaLocationURL = new URL( locationURI );
          if( m_useSchemaCatalog )
            schema = GMLSchemaCatalog.getSchema( schemaLocationURL );
          else
            schema = GMLSchemaFactory.createGMLSchema( schemaLocationURL );
        }
        catch( Exception e )
        {
          // TODO produce error message
          e.printStackTrace();
        }

        return schema;
      }
    }
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
      throw new GMLException( m_exceptionContentHandler.getOGCException() );
    throw new GMLException( "could not load GML" );
  }

}
