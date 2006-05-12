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
import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;
import org.kalypso.commons.xml.NSPrefixProvider;
import org.kalypso.commons.xml.NSUtilities;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.TypeRegistryException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.xml.sax.ContentHandler;
import org.xml.sax.SAXException;
import org.xml.sax.ext.LexicalHandler;
import org.xml.sax.helpers.AttributesImpl;

/**
 * @author doemming
 */
public class SAXFactory
{
  private final NSPrefixProvider m_nsMapper = NSUtilities.getNSProvider();

  private List<String> m_usedPrefixes = new ArrayList<String>();

  private final QName m_xlinkQN;

  private final ContentHandler m_handler;

  public SAXFactory( final ContentHandler handler ) throws SAXException
  {
    m_handler = handler;
    
    // initialize after handler is set
    m_xlinkQN = getPrefixedQName( new QName( NS.XLINK, "href" ) );
  }

  public void process( final GMLWorkspace workspace ) throws SAXException
  {
    final Feature rootFeature = workspace.getRootFeature();

    // handle prefixes...
    // theses are mandatory
    m_handler.startPrefixMapping( m_nsMapper.getPreferredPrefix( NS.GML2, null ), NS.GML2 );
    m_handler.startPrefixMapping( m_nsMapper.getPreferredPrefix( NS.XLINK, null ), NS.XLINK );
    m_handler.startPrefixMapping( m_nsMapper.getPreferredPrefix( NS.XSD, null ), NS.XSD );

    // final Set<String> uriSet = new HashSet<String>();
    final IFeatureType[] featureTypes = workspace.getGMLSchema().getAllFeatureTypes();
    for( int i = 0; i < featureTypes.length; i++ )
    {
      final QName qName = featureTypes[i].getQName();
      // generate used prefixes
      getPrefixedQName( qName );
      // uriSet.add( qName.getNamespaceURI() );
    }

    // for( final String uri : uriSet )
    // {
    // final String prefix = m_nsMapper.getPreferredPrefix( uri, null );
    // m_handler.startPrefixMapping( prefix, uri );
    // }
    final AttributesImpl a = new AttributesImpl();
    final String schemaLocationString = workspace.getSchemaLocationString();
    if( schemaLocationString != null && schemaLocationString.length() > 0 )
    {
      final String qName = m_nsMapper.getPreferredPrefix( NS.XSD, null ) + ":" + "schemaLocation";
      a.addAttribute( NS.XSD, "schemaLocation", qName, "CDATA", schemaLocationString );
    }
    process( rootFeature, a );
  }

  private void process( final Feature feature, final AttributesImpl a ) throws SAXException
  {
    final IFeatureType featureType = feature.getFeatureType();
    final QName prefixedQName = getPrefixedQName( feature.getFeatureType().getQName() );

    final String id = feature.getId();
    if( id != null && id.length() > 0 )
    {
      final String version = featureType.getGMLSchema().getGMLVersion();
      final QName idQName = getPrefixedQName( GMLSchemaUtilities.getIdAttribute( version ) );
      final String localPart = idQName.getLocalPart();
      a.addAttribute( idQName.getNamespaceURI(), localPart, idQName.getPrefix() + ":" + localPart, "CDATA", id );
    }

    final IPropertyType[] properties = featureType.getProperties();

    final String localPart = prefixedQName.getLocalPart();
    final String uri = prefixedQName.getNamespaceURI();
    // m_handler.ignorableWhitespace(new char[]{' '}, 0, 1);
    m_handler.startElement( uri, localPart, prefixedQName.getPrefix() + ":" + localPart, a );
    for( int i = 0; i < properties.length; i++ )
    {
      final IPropertyType pt = properties[i];
      if( pt instanceof IRelationType )
        process( feature, (IRelationType) pt );
      else if( pt instanceof IValuePropertyType )
        process( feature, (IValuePropertyType) pt );
      else
        throw new UnsupportedOperationException();
    }
    // m_handler.ignorableWhitespace(new char[]{' '}, 0, 1);
    m_handler.endElement( uri, localPart, prefixedQName.getPrefix() + ":" + localPart );
  }

  private QName getPrefixedQName( QName qName ) throws SAXException
  {
    final String uri = qName.getNamespaceURI();
    final String prefix = m_nsMapper.getPreferredPrefix( uri, null );
    if( !(m_usedPrefixes.contains( prefix )) )
      m_handler.startPrefixMapping( prefix, uri );
    m_usedPrefixes.add( prefix );
    return new QName( qName.getNamespaceURI(), qName.getLocalPart(), prefix );
  }

  private void process( final Feature feature, final IValuePropertyType vpt ) throws SAXException
  {
    final QName prefixedQName = getPrefixedQName( vpt.getQName() );
    final String uri = prefixedQName.getNamespaceURI();
    final String localPart = prefixedQName.getLocalPart();

    final Object value = feature.getProperty( vpt );
    if( value == null )
    {
      // write empty tag if this property is required
      if( vpt.getMinOccurs() > 0 )
      {
        m_handler.startElement( uri, localPart, prefixedQName.getPrefix() + ":" + localPart, null );

        // TODO: put default value if element is not nullable?

        m_handler.endElement( uri, localPart, prefixedQName.getPrefix() + ":" + localPart );
      }

      return;
    }
    else if( vpt.isList() )
    {
      for( final Object next : ((List) value) )
      {
        next.getClass(); // unused

        // m_handler.ignorableWhitespace(new char[]{' '}, 0, 1);
        m_handler.startElement( uri, localPart, prefixedQName.getPrefix() + ":" + localPart, null );

        // TODO parse content
        // m_handler.ignorableWhitespace(new char[]{' '}, 0, 1);
        m_handler.endElement( uri, localPart, prefixedQName.getPrefix() + ":" + localPart );
      }
    }
    else
    {
      final IMarshallingTypeHandler th = (IMarshallingTypeHandler) vpt.getTypeHandler();

      final URL context = null;
      LexicalHandler lexicalHandler = null;
      try
      {
        // m_handler.startElement(uri, localPart, getQName(qName),
        // null);
        th.marshal( prefixedQName, value, m_handler, lexicalHandler, context );
        // th.marshal(vpt.getQName(), value, m_handler, lexicalHandler,
        // context);
        // m_handler.endElement(uri, localPart, getQName(qName));
      }
      catch( final TypeRegistryException e )
      {
        e.printStackTrace();
      }
    }

  }

  /**
   * featureMember<br>
   * ..FeatureA <br>
   * ... (...)<br>
   * ..FeatureA <br>
   * featureMember<br>
   * <br>
   * or<br>
   * <br>
   * featureMember xlink:href="#fid"<br>
   */
  private void process( final Feature feature, final IRelationType rt ) throws SAXException
  {
    final QName prefixedQName = getPrefixedQName( rt.getQName() );

    final Object property = feature.getProperty( rt );
    if( property == null )
    {
      if( rt.getMinOccurs() > 0 )
      {
        // what?
      }
      return; // TODO check ? write empty element?
    }
    if( rt.isList() )
    {
      final FeatureList list = (FeatureList) property;
      for( final Object next : list )
        processFeature( next, prefixedQName );
    }
    else
      processFeature( property, prefixedQName );
  }

  private void processFeature( final Object next, final QName prefixedQName ) throws SAXException
  {
    final String uri = prefixedQName.getNamespaceURI();
    final String localPart = prefixedQName.getLocalPart();
    if( next instanceof Feature )
    {
      m_handler.startElement( uri, localPart, prefixedQName.getPrefix() + ":" + localPart, null );

      process( (Feature) next, new AttributesImpl() );
      m_handler.endElement( uri, localPart, prefixedQName.getPrefix() + ":" + localPart );
    }
    else if( next instanceof String ) // its a ID
    {
      final String fid = (String) next;
      final AttributesImpl atts = new AttributesImpl();

      atts.addAttribute( NS.XLINK, "href", m_xlinkQN.getPrefix() + ":" + m_xlinkQN.getLocalPart(), "CDATA", "#" + fid );
      m_handler.startElement( uri, localPart, prefixedQName.getPrefix() + ":" + localPart, atts );
      m_handler.endElement( uri, localPart, prefixedQName.getPrefix() + ":" + localPart );
    }
    else
      throw new UnsupportedOperationException( "Could not process: " + next );
  }
}
