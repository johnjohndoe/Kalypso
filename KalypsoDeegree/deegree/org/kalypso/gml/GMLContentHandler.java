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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;
import org.kalypso.contribs.javax.xml.namespace.QNameUtilities;
import org.kalypso.contribs.org.xml.sax.AttributesUtilities;
import org.kalypso.contribs.org.xml.sax.DelegateContentHandler;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.GMLSchemaCatalog;
import org.kalypso.gmlschema.KalypsoGMLSchemaPlugin;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler2;
import org.kalypso.gmlschema.types.ISimpleMarshallingTypeHandler;
import org.kalypso.gmlschema.types.TypeRegistryException;
import org.kalypso.gmlschema.types.UnmarshallResultEater;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;
import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.XMLReader;

/**
 * A {@link org.xml.sax.ContentHandler} implementation which parses GML fragment and produces a {@link Feature}
 * hierarchy from it.<br>
 * This content handler only parses the feature-property structure and delegates the parsing of any (non-feature)
 * property-values to their corresponding {@link IMarshallingTypeHandler}s.
 *
 * @author Andreas von Doemming
 */
public class GMLContentHandler extends DelegateContentHandler implements UnmarshallResultEater
{
  /* Additional schema locations parsed from the 'schemaLocation' attribute of the root element */
  private final Map<String, URL> m_schemaLocations = new HashMap<String, URL>();

  private final URL m_context;

  private final XMLReader m_xmlReader;

  /* The current feature in scope... */
  private Feature m_scopeFeature = null;

  private IPropertyType m_scopeProperty = null;

  /** Buffer for simple content, if this is about to be parsed */
  private StringBuffer m_simpleContent = null;

  private Feature m_rootFeature;

  private final ContentHandler m_parentHandler;

  private String m_version;

  /**
   * @param schemaLocations
   *          If non-<code>null</code>, these locations will be used to load the corresponding schmema's (i.e. given to
   *          the catalog). If the schema is already cached or a location is registered, the location will probably
   *          ignored.
   */
  public GMLContentHandler( final XMLReader xmlReader, final URL context, final Map<String, URL> schemaLocations )
  {
    m_xmlReader = xmlReader;
    m_context = context;
    if( schemaLocations != null )
      m_schemaLocations.putAll( schemaLocations );

    m_parentHandler = m_xmlReader.getContentHandler();
  }

  /**
   * @see org.xml.sax.ContentHandler#startDocument()
   */
  @Override
  public void startDocument( )
  {
    m_scopeFeature = null;
    m_scopeProperty = null;
  }

  /**
   * @see org.xml.sax.ContentHandler#startElement(java.lang.String, java.lang.String, java.lang.String,
   *      org.xml.sax.Attributes)
   */
  @Override
  public void startElement( final String uri, final String localName, final String qName, final Attributes atts ) throws SAXException
  {
    if( getDelegate() != null )
    {
      super.startElement( uri, localName, qName, atts );
      return;
    }

    final QName qname = new QName( uri, localName );

    if( (m_scopeFeature == null && m_scopeProperty == null) || (m_scopeFeature != null && m_scopeProperty instanceof IRelationType) )
      startFeature( atts, qname );
    else if( m_scopeFeature != null && m_scopeProperty == null )
      startProperty( uri, localName, qName, atts, qname );
    else
    {
      final String msg = String.format( "GML not well-balanced. Feature scope is: %s\tProperty scope is: %s\tStarting element: %s ", m_scopeFeature, m_scopeProperty, qName );
      // this really should never happen as sax already checks the well-balancedness
      throw new SAXException( msg );
    }
  }

  private void startProperty( final String uri, final String localName, final String qName, final Attributes atts, final QName qname ) throws SAXException
  {
    final Feature feature = m_scopeFeature;

    final IFeatureType featureType = feature.getFeatureType();
    final IPropertyType pt = featureType.getProperty( qname );
    if( pt == null )
    {
      final String msg = String.format( "Found unknwon property '%s' for FeatureType '%s'", qname, featureType.getQName() );
      throw new SAXException( msg );
    }

    /* Go into scope with that feature */
    m_scopeProperty = pt;

    if( pt instanceof IValuePropertyType )
    {
      final IValuePropertyType vpt = (IValuePropertyType) pt;
      if( vpt.getTypeHandler() instanceof ISimpleMarshallingTypeHandler )
        m_simpleContent = new StringBuffer();
      else
        startValueProperty( uri, localName, qName, atts, vpt );
    }
    else if( pt instanceof IRelationType )// its a relation
      startXLinkedFeature( atts, feature, (IRelationType) pt );
    else
    {
      /* Should never happen. either its a value or a relation. */
      throw new SAXException( "Unknown IPropertyType instance: " + pt );
    }
  }

  private void startValueProperty( final String uri, final String localName, final String qName, final Attributes atts, final IValuePropertyType vpt ) throws SAXException
  {
    // TODO: we should distinguish between simple and complex types here and use different types of typeHandlers for
    // that. Reason1: get rid of overhead for simple types, just read the content as string and translate to the
    // corresponding simple type. Reason2: If it is clear for the complex types that they are complex, we may
    // enhance parsing there (for example do not read the end element of the property).

    final IMarshallingTypeHandler typeHandler = vpt.getTypeHandler();

    if( typeHandler instanceof IMarshallingTypeHandler2 )
    {
      final IMarshallingTypeHandler2 th2 = (IMarshallingTypeHandler2) typeHandler;
      final ContentHandler contentHandler = th2.createContentHandler( m_xmlReader, this, uri, localName, qName, atts );
      setDelegate( contentHandler );
    }
    else
    {
      try
      {
        final GMLSchema schema = findSchema( uri );
        final String gmlVersion = schema.getGMLVersion();

        // TODO: we SHOULD provide here the full information to the handler: qname, att, ...
        typeHandler.unmarshal( m_xmlReader, m_context, this, gmlVersion );
      }
      catch( final TypeRegistryException e )
      {
        e.printStackTrace();

        m_xmlReader.getErrorHandler().warning( new SAXParseException( "Failed to unmarshall property value: " + vpt, getLocator(), e ) );
      }
    }
  }

  private GMLSchema findSchema( final String namespace ) throws SAXParseException
  {
    try
    {
      final GMLSchemaCatalog schemaCatalog = KalypsoGMLSchemaPlugin.getDefault().getSchemaCatalog();
      final URL locationHint = m_schemaLocations.get( namespace );
      final GMLSchema schema = schemaCatalog.getSchema( namespace, m_version, locationHint );

      if( m_version == null )
        m_version = schema.getGMLVersion();

      return schema;
    }
    catch( final InvocationTargetException e )
    {
      final Exception targetException = (Exception) e.getTargetException();
      targetException.printStackTrace();
      throw new SAXParseException( "Unknown schema for namespace: " + namespace, getLocator(), targetException );
    }
  }

  private void startXLinkedFeature( final Attributes atts, final Feature parentFeature, final IRelationType parentRelation )
  {
    final String href = AttributesUtilities.getAttributeValue( atts, NS.XLINK, "href", null );

    if( href != null )// its a xlink
    {
      // REMARK: for backwards compability, we still set the href as property-value
      // for internal links. This should be changed soon...
      if( href.startsWith( "#" ) )
      {
        final String refID2 = href.replaceAll( "^#", "" );
        FeatureHelper.addChild( parentFeature, parentRelation, refID2 );
      }
      else
      {
        final String role = AttributesUtilities.getAttributeValue( atts, NS.XLINK, "role", null );
        final String arcrole = AttributesUtilities.getAttributeValue( atts, NS.XLINK, "arcrole", null );
        final String title = AttributesUtilities.getAttributeValue( atts, NS.XLINK, "title", null );
        final String show = AttributesUtilities.getAttributeValue( atts, NS.XLINK, "show", "replace" );
        final String actuate = AttributesUtilities.getAttributeValue( atts, NS.XLINK, "actuate", "onRequest" );

        final IFeatureType targetFeatureType = parentRelation.getTargetFeatureType();
        final Feature childFeature = new XLinkedFeature_Impl( parentFeature, parentRelation, targetFeatureType, href, role, arcrole, title, show, actuate );
        FeatureHelper.addChild( parentFeature, parentRelation, childFeature );
      }

      // normally the new feature should be set into scope as well, but it is not a real feature...
    }
    /*
     * 'else' <p> The other case is an inline feature. That is parsed by entering again into startElement with the
     * property type as scope.</p>
     */

    // TODO: We should make sure that this element has no further content.
  }

  private void startFeature( final Attributes atts, final QName qname ) throws SAXException
  {
    /* Root feature or new sub-feature. */
    final GMLSchema schema = findSchema( qname.getNamespaceURI() );
    final IFeatureType featureType = schema.getFeatureType( qname );
    if( featureType == null )
      throw new SAXException( "No feature type found for: " + qname );

    final String fid = idFromAttributes( atts );

    final Feature childFE = FeatureFactory.createFeature( m_scopeFeature, (IRelationType) m_scopeProperty, fid, featureType, false );
    if( m_scopeFeature != null )
      FeatureHelper.addChild( m_scopeFeature, (IRelationType) m_scopeProperty, childFE );

    m_scopeFeature = childFE;

    m_scopeProperty = null;
  }

  /**
   * @see org.xml.sax.ContentHandler#endElement(java.lang.String, java.lang.String, java.lang.String)
   */
  @Override
  public void endElement( final String uri, final String localName, final String qName ) throws SAXException
  {
    /* First check, if a current scope is closing */
    final String localUri = uri == null || uri.isEmpty() ? null /* schema.getTargetNamespace() */: uri;

    if( m_simpleContent != null )
    {
      endSimpleContent( localUri, localName );
      m_scopeProperty = null;
      return;
    }

    if( m_scopeProperty != null && QNameUtilities.equals( m_scopeProperty.getQName(), localUri, localName ) )
    {
      // REMARK: this case happens, after the type-marshaler has already read the content of the element

      /* Closing current property */
      m_scopeProperty = null;
      setDelegate( null );
      return;
    }

    if( m_scopeFeature != null && QNameUtilities.equals( m_scopeFeature.getFeatureType().getQName(), localUri, localName ) )
    {
      /* Closing current feature, scope changes back to parent feature. */
      final Feature parent = m_scopeFeature.getOwner();
      /* If the root gets closed we know the result feature. */
      if( parent == null )
        m_rootFeature = m_scopeFeature;

      m_scopeFeature = parent;
      setDelegate( null );
      return;
    }

    /* If still not handled, maybe we have an active delegate */
    if( getDelegate() != null )
    {
      super.endElement( uri, localName, qName );
      return;
    }
  }

  /**
   * @see org.kalypso.contribs.org.xml.sax.DelegateContentHandler#characters(char[], int, int)
   */
  @Override
  public void characters( final char[] ch, final int start, final int length ) throws SAXException
  {
    if( m_simpleContent == null )
      super.characters( ch, start, length );
    else
      m_simpleContent.append( ch, start, length );
  }

  /**
   * @see org.kalypso.contribs.org.xml.sax.DelegateContentHandler#ignorableWhitespace(char[], int, int)
   */
  @Override
  public void ignorableWhitespace( final char[] ch, final int start, final int len )
  {
    if( m_simpleContent != null )
      m_simpleContent.append( ch, start, len );
  }

  public Feature getRootFeature( ) throws GMLException
  {
    if( m_rootFeature == null )
      throw new GMLException( "Could not load GML, Root-Feature was not created." );

    return m_rootFeature;
  }

  private static String idFromAttributes( final Attributes atts )
  {
    final int id1 = atts.getIndex( NS.GML2, "fid" );
    if( id1 != -1 )
      return atts.getValue( id1 );

    final int id2 = atts.getIndex( "fid" );
    if( id2 != -1 )
      return atts.getValue( id2 );

    final int id3 = atts.getIndex( NS.GML2, "id" );
    if( id3 != -1 )
      return atts.getValue( id3 );

    final int id4 = atts.getIndex( "id" );
    if( id4 != -1 )
      return atts.getValue( id4 );

    return null;
  }

  /**
   * Will be called, if simple content was parsed.
   */
  private void endSimpleContent( final String uri, final String localName ) throws SAXException
  {
    if( m_scopeProperty == null )
      throw new SAXException( "Simple Content found outside of a property." );

    if( !(m_scopeProperty instanceof IValuePropertyType) )
      throw new SAXException( String.format( "Tried to parse simple content for non simple type: ", m_scopeProperty.getQName() ) );

    if( !QNameUtilities.equals( m_scopeProperty.getQName(), uri, localName ) )
      throw new SAXException( String.format( "Unbalanced XML at </{%s}%s>", uri, localName ) );

    final IValuePropertyType valuePT = (IValuePropertyType) m_scopeProperty;
    final ISimpleMarshallingTypeHandler< ? > simpleHandler = (ISimpleMarshallingTypeHandler< ? >) valuePT.getTypeHandler();
    final String simpleString = m_simpleContent.toString();

    try
    {
      final Object value = simpleHandler.convertToJavaValue( simpleString );
      if( m_scopeProperty.isList() )
        ((List) m_scopeFeature.getProperty( m_scopeProperty )).add( value );
      else
        m_scopeFeature.setProperty( m_scopeProperty, value );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final String msg = String.format( "Failed to parsed simple type. Content was '%s' for property '%s'", simpleString, m_scopeProperty.getQName() );
      throw new SAXParseException( msg, getLocator(), e );
    }
    finally
    {
      // Always delete; simple content always stops at the next tag
      m_simpleContent = null;
    }
  }

  /**
   * @see org.kalypso.gmlschema.types.UnMarshallResultEater#eat(java.lang.Object)
   */
  // TODO: we need here the full information to recall endElement here
  // maybe even get a flag if we should all endElement?
  @SuppressWarnings("unchecked")
  public void unmarshallSuccesful( final Object value )
  {
    // TODO: this should be done by the unmarshallers that set the content handler to another value
    m_xmlReader.setContentHandler( m_parentHandler );
    setDelegate( null );

    try
    {
      // TODO: is this always correct? What about empty elements of list properties?
      if( value == null )
        return;

      if( m_scopeProperty.isList() )
      {
        final List<Object> list = (List<Object>) m_scopeFeature.getProperty( m_scopeProperty );
        list.add( value );
      }
      else
        m_scopeFeature.setProperty( m_scopeProperty, value );
    }
    finally
    {
      // This should not be necessary, the inner content handler parses too much at the moment!
      // equivalent to endElement( -property-qname- ); but we do not have that information at the moment
      m_scopeProperty = null;
    }
  }
}
