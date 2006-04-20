/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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

import java.util.List;
import java.util.Stack;

import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilderFactory;

import org.kalypso.commons.java.net.UrlResolver;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.UnMarshallResultEater;
import org.kalypso.gmlschema.types.TypeRegistryException;
import org.kalypsodeegree.model.feature.Feature;
import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;

/**
 * @author doemming
 */
public class PropertyParser
{

	public PropertyParser()
	{
		final DocumentBuilderFactory factory = DocumentBuilderFactory
				.newInstance();
		factory.setNamespaceAware(true);
	}

	final Stack<IPropertyType> m_stackPT = new Stack<IPropertyType>();

	public void createProperty(Feature feature, String uri, String localName,
			Attributes atts)
	{
		final IFeatureType featureType = feature.getFeatureType();
		final QName propQName = new QName(uri, localName);
		final IPropertyType property = featureType.getProperty(propQName);
		m_stackPT.push(property);
		// TODO check if it is a link
	}

	public IPropertyType getCurrentPropertyType()
	{
		if (m_stackPT.empty())
			return null;
		return m_stackPT.peek();
	}

	public void setContent(Feature feature, String content)
	{
		return;
		// final IValuePropertyType vpt = (IValuePropertyType)
		// getCurrentPropertyType();
		// final IMarshallingTypeHandler typeHandler = (IMarshallingTypeHandler)
		// vpt.getTypeHandler();

	}

	public void setContent(final Feature parentFE, final IValuePropertyType pt,
			final XMLReader xmlReader, final String uri,
			final String localName, final String qName, Attributes atts)
	{
		final IMarshallingTypeHandler typeHandler = (IMarshallingTypeHandler) pt
				.getTypeHandler();
		final UrlResolver urlResolver = null;
		final ContentHandler orgCH = xmlReader.getContentHandler();
		final UnMarshallResultEater resultEater = new UnMarshallResultEater()
		{

			public void eat(Object value)
			{
				xmlReader.setContentHandler(orgCH);
				// simulate end element tag
				try
				{
					orgCH.endElement(uri, localName, qName);
				} catch (SAXException e)
				{
					// TODO Auto-generated catch block
					e.printStackTrace();
				}

				if (value == null)
				{
					return;
				}
				if (pt.isList())
				{
					final List<Object> list = (List<Object>) parentFE
							.getProperty(pt);
					list.add(value);
				} else
					parentFE.setProperty(pt, value);
			}
		};

		try
		{
			typeHandler.unmarshal(xmlReader, urlResolver, resultEater);
		} catch (TypeRegistryException e)
		{
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		// xmlReader.setContentHandler( con );
		// // simulate startElement
		// con.startElement( uri, localName, qName, atts );
	}

	public void popPT()
	{
		m_stackPT.pop();
	}
}
