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
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;
import org.kalypso.commons.xml.NSPrefixProvider;
import org.kalypso.commons.xml.NSUtilities;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;
import org.xml.sax.ContentHandler;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.AttributesImpl;

/**
 * @author doemming
 */
public class GMLSAXFactory
{
  private final NSPrefixProvider m_nsMapper = NSUtilities.getNSProvider();

  private final List<String> m_usedPrefixes = new ArrayList<String>();

  private final QName m_xlinkQN;

  /**
   * (existing-ID,new-ID) mapping for ids, replace all given Ids in GML (feature-ID and links)
   */
  private final Map<String, String> m_idMap;

  private final XMLReader m_xmlReader;

  /**
   * @param idMap
   *            (existing-ID,new-ID) mapping for ids, replace all given Ids in GML (feature-ID and links)
   */
  public GMLSAXFactory( final XMLReader xmlReader, final Map<String, String> idMap ) throws SAXException
  {
    m_xmlReader = xmlReader;
    m_idMap = idMap;

    // initialize after handler is set
    m_xlinkQN = getPrefixedQName( new QName( NS.XLINK, "href" ) );
  }

  public void process( final GMLWorkspace workspace ) throws SAXException
  {
    final Feature rootFeature = workspace.getRootFeature();

    final ContentHandler contentHandler = m_xmlReader.getContentHandler();

    // handle prefixes...
    // theses are mandatory
    contentHandler.startPrefixMapping( m_nsMapper.getPreferredPrefix( NS.GML2, null ), NS.GML2 );
    contentHandler.startPrefixMapping( m_nsMapper.getPreferredPrefix( NS.XLINK, null ), NS.XLINK );
    contentHandler.startPrefixMapping( m_nsMapper.getPreferredPrefix( NS.XSD, null ), NS.XSD );

    final GMLSchema gmlSchema = (GMLSchema) workspace.getGMLSchema();

    final IFeatureType[] featureTypes = gmlSchema.getAllFeatureTypes();
    for( final IFeatureType element : featureTypes )
    {
      final QName qName = element.getQName();
      // generate used prefixes
      getPrefixedQName( qName );
    }

    // we may have additional schema, but no features using them (now)
    // We save these namespaces as prefixes, so if we reload the gml
    // the additional schema will also be loaded
    final Set<String> uriSet = new HashSet<String>();
    final GMLSchema[] additionalSchemas = gmlSchema.getAdditionalSchemas();
    for( final GMLSchema additionalSchema : additionalSchemas )
      uriSet.add( additionalSchema.getTargetNamespace() );
    for( final String uri : uriSet )
    {
      final String prefix = m_nsMapper.getPreferredPrefix( uri, null );
      contentHandler.startPrefixMapping( prefix, uri );
    }

    // Add schemalocation string: wouldn't it be better to create it?
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
    final ContentHandler contentHandler = m_xmlReader.getContentHandler();

    final IFeatureType featureType = feature.getFeatureType();

    String id = feature.getId();
    if( m_idMap.containsKey( id ) )
      id = m_idMap.get( id );
    if( id != null && id.length() > 0 )
    {
      final String version = featureType.getGMLSchema().getGMLVersion();
      final QName idQName = getPrefixedQName( GMLSchemaUtilities.getIdAttribute( version ) );
      final String localPart = idQName.getLocalPart();
      final String namespaceUri = idQName.getNamespaceURI();
      a.addAttribute( namespaceUri, localPart, idQName.getPrefix() + ":" + localPart, "CDATA", id );
    }

    final IPropertyType[] properties = featureType.getProperties();

    final QName prefixedQName = getPrefixedQName( feature.getFeatureType().getQName() );
    final String localPart = prefixedQName.getLocalPart();
    final String uri = prefixedQName.getNamespaceURI();

    contentHandler.startElement( uri, localPart, prefixedQName.getPrefix() + ":" + localPart, a );

    for( final IPropertyType pt : properties )
    {
      if( pt instanceof IRelationType )
        process( feature, (IRelationType) pt );
      else if( pt instanceof IValuePropertyType )
        process( feature, (IValuePropertyType) pt );
      else
        throw new UnsupportedOperationException();
    }

    contentHandler.endElement( uri, localPart, prefixedQName.getPrefix() + ":" + localPart );
  }

  private QName getPrefixedQName( final QName qName ) throws SAXException
  {
    final String uri = qName.getNamespaceURI();
    final String prefix = m_nsMapper.getPreferredPrefix( uri, null );
    if( !(m_usedPrefixes.contains( prefix )) )
      m_xmlReader.getContentHandler().startPrefixMapping( prefix, uri );
    m_usedPrefixes.add( prefix );
    return new QName( qName.getNamespaceURI(), qName.getLocalPart(), prefix );
  }

  private void process( final Feature feature, final IValuePropertyType vpt ) throws SAXException
  {
    final QName prefixedQName = getPrefixedQName( vpt.getQName() );
    final String uri = prefixedQName.getNamespaceURI();
    final String localPart = prefixedQName.getLocalPart();

    final Object value = feature.getProperty( vpt );

    final GMLSchema gmlSchema = (GMLSchema) feature.getWorkspace().getGMLSchema();
    final String version = gmlSchema.getGMLVersion();

    final IMarshallingTypeHandler th = vpt.getTypeHandler();

    if( vpt.isList() )
    {
      for( final Object singleValue : ((List< ? >) value) )
        processValue( prefixedQName, uri, localPart, th, null, singleValue, true, version );
    }
    else
    {
      final boolean isMandatory = vpt.getMinOccurs() > 0;
      processValue( prefixedQName, uri, localPart, th, null, value, isMandatory, version );
    }

  }

  private void processValue( final QName prefixedQName, final String uri, final String localPart, final IMarshallingTypeHandler th, final URL context, final Object singleValue, final boolean isMandatory, final String gmlVersion ) throws SAXException
  {
    final ContentHandler contentHandler = m_xmlReader.getContentHandler();

    if( singleValue != null )
      th.marshal( prefixedQName, singleValue, m_xmlReader, context, gmlVersion );
    else if( isMandatory )
    {
      contentHandler.startElement( uri, localPart, prefixedQName.getPrefix() + ":" + localPart, new AttributesImpl() );
      // TODO: put default value if element is not nullable?
      contentHandler.endElement( uri, localPart, prefixedQName.getPrefix() + ":" + localPart );
    }
    else
    {
      // optional null value: do nothing
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
        processFeatureLink( next, prefixedQName );
    }
    else
      processFeatureLink( property, prefixedQName );
  }

  private void processFeatureLink( final Object next, final QName prefixedQName ) throws SAXException
  {
    final ContentHandler contentHandler = m_xmlReader.getContentHandler();

    final String uri = prefixedQName.getNamespaceURI();
    final String localPart = prefixedQName.getLocalPart();

    if( next instanceof XLinkedFeature_Impl || next instanceof String )
    {
      String fid;
      if( next instanceof String )
      {
        // local xlinks, used for backwards compability, should be changes soon
        fid = (String) next;
        if( m_idMap.containsKey( fid ) )
          fid = m_idMap.get( fid );

        fid = "#" + fid;
      }
      else
        fid = ((XLinkedFeature_Impl) next).getHref();

      final AttributesImpl atts = new AttributesImpl();
      atts.addAttribute( NS.XLINK, "href", m_xlinkQN.getPrefix() + ":" + m_xlinkQN.getLocalPart(), "CDATA", fid );
      contentHandler.startElement( uri, localPart, prefixedQName.getPrefix() + ":" + localPart, atts );
      contentHandler.endElement( uri, localPart, prefixedQName.getPrefix() + ":" + localPart );
    }
    else if( next instanceof Feature )
    {
      contentHandler.startElement( uri, localPart, prefixedQName.getPrefix() + ":" + localPart, new AttributesImpl() );

      process( (Feature) next, new AttributesImpl() );

      contentHandler.endElement( uri, localPart, prefixedQName.getPrefix() + ":" + localPart );
    }
    else
      throw new UnsupportedOperationException( "Could not process: " + next );
  }
}
