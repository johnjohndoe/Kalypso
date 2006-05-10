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
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;
import org.kalypso.commons.xml.NSPrefixProvider;
import org.kalypso.commons.xml.NSUtilities;
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
	private final ContentHandler m_handler;

	private final NSPrefixProvider m_nsMapper;

	private List<String> m_usedPrefixes = new ArrayList<String>();

	private final QName m_xlinkQN = new QName(NS.XLINK, "href");

	public SAXFactory(ContentHandler handler)
	{
		m_handler = handler;
		m_nsMapper = NSUtilities.getNSProvider();
	}

	public void process(GMLWorkspace workspace) throws SAXException
	{
		final Feature rootFeature = workspace.getRootFeature();

		// handle prefixes...
		// theses are mandatory
		m_handler.startPrefixMapping(m_nsMapper.getPreferredPrefix(NS.GML2,
				null), NS.GML2);
		m_handler.startPrefixMapping(m_nsMapper.getPreferredPrefix(NS.XLINK,
				null), NS.XLINK);
		m_handler.startPrefixMapping(m_nsMapper.getPreferredPrefix(NS.NS_XSD,
				null), NS.NS_XSD);

		final Set<String> uriSet = new HashSet<String>();
		final IFeatureType[] featureTypes = workspace.getFeatureTypes();
		for (int i = 0; i < featureTypes.length; i++)
			uriSet.add(featureTypes[i].getQName().getNamespaceURI());

		for (Iterator<String> iter = uriSet.iterator(); iter.hasNext();)
		{
			final String uri = iter.next();
			final String prefix = m_nsMapper.getPreferredPrefix(uri, null);
			m_handler.startPrefixMapping(prefix, uri);
		}
		final AttributesImpl a = new AttributesImpl();
		final String schemaLocationString = workspace.getSchemaLocationString();
		if (schemaLocationString != null && schemaLocationString.length() > 0)
		{
			final String qName = m_nsMapper.getPreferredPrefix(NS.NS_XSD, null)
					+ ":" + "schemaLocation";
			a.addAttribute(NS.NS_XSD, "schemaLocation", qName, "CDATA",
					schemaLocationString);
		}
		process(rootFeature, a);
	}

	private void process(final Feature feature, AttributesImpl a)
			throws SAXException
	{
		final String id = feature.getId();
		if (id != null && id.length() > 0)
			a.addAttribute("", "fid", "fid", "CDATA", id);
		final IFeatureType featureType = feature.getFeatureType();
		final IPropertyType[] properties = featureType.getProperties();

		final QName qName = feature.getFeatureType().getQName();
		final String localPart = qName.getLocalPart();
		final String uri = qName.getNamespaceURI();
		// m_handler.ignorableWhitespace(new char[]{' '}, 0, 1);
		final QName prefixedQName = getPrefixedQName(qName);
		m_handler.startElement(uri, localPart, prefixedQName.getPrefix() + ":"
				+ localPart, a);
		for (int i = 0; i < properties.length; i++)
		{
			final IPropertyType pt = properties[i];
			if (pt instanceof IRelationType)
				process(feature, (IRelationType) pt);
			else if (pt instanceof IValuePropertyType)
				process(feature, (IValuePropertyType) pt);
			else
				throw new UnsupportedOperationException();
		}
		// m_handler.ignorableWhitespace(new char[]{' '}, 0, 1);
		m_handler.endElement(uri, localPart, prefixedQName.getPrefix() + ":"
				+ localPart);
	}

	/**
	 * 
	 */
	private QName getPrefixedQName(QName qName) throws SAXException
	{
		final String uri = qName.getNamespaceURI();
		final String prefix = m_nsMapper.getPreferredPrefix(uri, null);
		if (!(m_usedPrefixes.contains(prefix)))
			m_handler.startPrefixMapping(prefix, uri);
		m_usedPrefixes.add(prefix);
		return new QName(qName.getNamespaceURI(), qName.getLocalPart(), prefix);
	}

	private void process(final Feature feature, final IValuePropertyType vpt)
			throws SAXException
	{
		final QName qName = vpt.getQName();
		final String uri = qName.getNamespaceURI();
		final String localPart = qName.getLocalPart();

		final Object value = feature.getProperty(vpt);
		if (value == null)
			return; // TODO check
		if (vpt.isList())
		{
			final List list = (List) value;
			final Iterator iterator = list.iterator();
			while (iterator.hasNext())
			{
				/* final Object next = */iterator.next();
				// m_handler.ignorableWhitespace(new char[]{' '}, 0, 1);
				final QName prefixedQName = getPrefixedQName(qName);
				m_handler.startElement(uri, localPart, prefixedQName
						.getPrefix()
						+ ":" + localPart, null);

				// TODO parse content
				// m_handler.ignorableWhitespace(new char[]{' '}, 0, 1);
				m_handler.endElement(uri, localPart, prefixedQName.getPrefix()
						+ ":" + localPart);
			}
		} else
		{

			final IMarshallingTypeHandler th = (IMarshallingTypeHandler) vpt
					.getTypeHandler();

			final URL context = null;
			LexicalHandler lexicalHandler = null;
			try
			{
				// m_handler.startElement(uri, localPart, getQName(qName),
				// null);
				th.marshal(getPrefixedQName(qName), value, m_handler,
						lexicalHandler, context);
				// th.marshal(vpt.getQName(), value, m_handler, lexicalHandler,
				// context);
				// m_handler.endElement(uri, localPart, getQName(qName));
			} catch (TypeRegistryException e)
			{
				e.printStackTrace();
			}
			// final DocumentBuilderFactory fac =
			// DocumentBuilderFactory.newInstance();
			// fac.setNamespaceAware( true );
			// try
			// {
			// // first create DOM to support old marshalling concept
			// final DocumentBuilder builder = fac.newDocumentBuilder();
			// final Document document = builder.newDocument();
			// final Element element = document.createElementNS( uri, localPart
			// );
			// th.marshall( value, element, context );
			// // TODO supress namespace that are allredy declared
			// final DOM2SAX dom2sax = new DOM2SAX( element );
			// dom2sax.setContentHandler( m_handler );
			// dom2sax.parse();
			// }
			// catch( Exception e )
			// {
			// // TODO Auto-generated catch block
			// e.printStackTrace();
			// }
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
	private void process(final Feature feature, final IRelationType rt)
			throws SAXException
	{
		final QName qName = rt.getQName();

		final Object property = feature.getProperty(rt);
		if (property == null)
			return; // TODO check
		if (rt.isList())
		{
			final FeatureList list = (FeatureList) property;
			final Iterator iterator = list.iterator();
			while (iterator.hasNext())
			{
				final Object next = iterator.next();
				processFeature(next, qName);
			}
		} else
			processFeature(property, qName);
	}

	private void processFeature(final Object next, final QName qName)
			throws SAXException
	{
		final String uri = qName.getNamespaceURI();
		final String localPart = qName.getLocalPart();
		final QName prefixedQName = getPrefixedQName(qName);
		if (next instanceof Feature)
		{
			m_handler.startElement(uri, localPart, prefixedQName.getPrefix()
					+ ":" + localPart, null);

			process((Feature) next, new AttributesImpl());
			m_handler.endElement(uri, localPart, prefixedQName.getPrefix()
					+ ":" + localPart);
		} else if (next instanceof String) // its a ID
		{
			final String fid = (String) next;
			final AttributesImpl atts = new AttributesImpl();

			QName prefixedLinkQName = getPrefixedQName(m_xlinkQN);
			atts.addAttribute(NS.XLINK, "href", prefixedLinkQName.getPrefix()
					+ ":" + prefixedLinkQName.getLocalPart(), "CDATA", "#"
					+ fid);
			m_handler.startElement(uri, localPart, prefixedQName.getPrefix()
					+ ":" + localPart, atts);
			m_handler.endElement(uri, localPart, prefixedQName.getPrefix()
					+ ":" + localPart);
		} else
			throw new UnsupportedOperationException();

	}
}
