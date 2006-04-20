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
package org.kalypsodeegree.model.geometry;

import java.net.URL;

import javax.xml.bind.JAXBContext;
import javax.xml.namespace.QName;

import ogc31.www.opengis.net.gml.AbstractGeometryType;

import org.kalypso.commons.xml.NS;
import org.kalypso.contribs.java.net.IUrlResolver;
import org.kalypso.gmlschema.types.GenericBindingTypeHandler;
import org.kalypso.gmlschema.types.UnMarshallResultEater;
import org.kalypso.gmlschema.types.TypeRegistryException;
import org.kalypsodeegree_impl.model.geometry.GML3BindingGM_ObjectAdapter;
import org.xml.sax.ContentHandler;
import org.xml.sax.XMLReader;
import org.xml.sax.ext.LexicalHandler;

/**
 * a generic typehandler for GM_Object geometries based on a bindingtypehandler<br>
 * is wraps binding geometries to GM_Object geometries
 * 
 * @author doemming
 */
public class GenericGM_ObjectBindingTypeHandler extends
		GenericBindingTypeHandler
{

	private final Class m_gmObjectClass;

	public GenericGM_ObjectBindingTypeHandler(JAXBContext jaxbContext,
			QName typeQName, QName valueQName, Class gm_objectClass,
			boolean isGeometry)
	{
		super(jaxbContext, typeQName, valueQName, GML3BindingGM_ObjectAdapter
				.getBindingClassFor(gm_objectClass), isGeometry);
		m_gmObjectClass = gm_objectClass;
	}

	/**
	 * @see org.kalypso.gmlschema.types.GenericBindingTypeHandler#getValueClass()
	 */
	@Override
	public Class getValueClass()
	{
		return m_gmObjectClass;
	}

	/**
	 * @see org.kalypso.gmlschema.types.GenericBindingTypeHandler#unmarshal(org.xml.sax.XMLReader,
	 *      org.kalypso.contribs.java.net.IUrlResolver,
	 *      org.kalypso.gmlschema.types.MarshalResultEater)
	 */
	@Override
	public void unmarshal(XMLReader xmlReader, IUrlResolver urlResolver,
			final UnMarshallResultEater marshalResultEater)
			throws TypeRegistryException
	{
		final UnMarshallResultEater eater = new UnMarshallResultEater()
		{
			public void eat(Object value)
			{
				try
				{
					final GM_Object geometryValue = GML3BindingGM_ObjectAdapter
							.createGM_Object(value);
					marshalResultEater.eat(geometryValue);
				} catch (GM_Exception e)
				{
					e.printStackTrace();
				}
			}
		};
		super.unmarshal(xmlReader, urlResolver, eater);
	}

	/**
	 * @see org.kalypso.gmlschema.types.GenericBindingTypeHandler#marshal(java.lang.Object,
	 *      org.xml.sax.ContentHandler, org.xml.sax.ext.LexicalHandler,
	 *      java.net.URL)
	 */
	@Override
	public void marshal(QName propQName, Object value,
			ContentHandler contentHandler, LexicalHandler lexicalHandler,
			URL context) throws TypeRegistryException
	{
		final GM_Object geometry = (GM_Object) value;
		try
		{
			final AbstractGeometryType abstractGeomType = GML3BindingGM_ObjectAdapter
					.createBindingGeometryType(geometry);
			super.marshal(propQName, abstractGeomType, contentHandler,
					lexicalHandler, context);
		} catch (GM_Exception e)
		{
			e.printStackTrace();
			throw new TypeRegistryException(e);
		}
	}
}
