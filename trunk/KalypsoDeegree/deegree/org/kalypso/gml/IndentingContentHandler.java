/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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

import java.util.Arrays;

import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;

/**
 * a contenthandler that does indenting
 * 
 * @author doemming
 */

public class IndentingContentHandler implements ContentHandler
{
	private final char[] m_newLine = new char[]
	{ '\n' };

	private final char[] m_spaces;

	private final ContentHandler m_handler;

	private int m_indent;

	private boolean m_doNewLine_ElementClose = true;

	public IndentingContentHandler(ContentHandler handler, int number)
	{
		m_handler = handler;
		m_spaces = new char[number];
		Arrays.fill(m_spaces, ' ');
		m_indent = 0;
	}

	public void startElement(String uri, String localName, String qName,
			Attributes atts) throws SAXException
	{
		// allways newline
		if (m_indent > 0)
			m_handler.ignorableWhitespace(m_newLine, 0, m_newLine.length);
		for (int i = 0; i < m_indent; i++)
			m_handler.ignorableWhitespace(m_spaces, 0, m_spaces.length);
		m_indent++;
		m_handler.startElement(uri, localName, qName, atts);
		m_doNewLine_ElementClose = false;
	}

	public void endElement(String uri, String localName, String qName)
			throws SAXException
	{
		m_indent--;
		if (m_doNewLine_ElementClose)
		{
			m_handler.ignorableWhitespace(m_newLine, 0, m_newLine.length);
			for (int i = 0; i < m_indent; i++)
				m_handler.ignorableWhitespace(m_spaces, 0, m_spaces.length);
		}
		m_handler.endElement(uri, localName, qName);
		m_doNewLine_ElementClose = true;
	}

	/**
	 * @see org.xml.sax.ContentHandler#characters(char[], int, int)
	 */
	public void characters(char[] ch, int start, int length)
			throws SAXException
	{
		// if (!m_doNewLine_ElementClose)
		m_handler.characters(ch, start, length);
	}

	/**
	 * @see org.xml.sax.ContentHandler#ignorableWhitespace(char[], int, int)
	 */
	public void ignorableWhitespace(char[] ch, int start, int length)
			throws SAXException
	{
		// if (!m_doNewLine_ElementClose)
		m_handler.ignorableWhitespace(ch, start, length);
	}

	/**
	 * @see org.xml.sax.ContentHandler#endDocument()
	 */
	public void endDocument() throws SAXException
	{
		if (m_indent <= 1)
			m_handler.endDocument();
	}

	/**
	 * @see org.xml.sax.ContentHandler#endPrefixMapping(java.lang.String)
	 */
	public void endPrefixMapping(String prefix) throws SAXException
	{
		m_handler.endPrefixMapping(prefix);
	}

	/**
	 * @see org.xml.sax.ContentHandler#processingInstruction(java.lang.String,
	 *      java.lang.String)
	 */
	public void processingInstruction(String target, String data)
			throws SAXException
	{
		m_handler.processingInstruction(target, data);
	}

	/**
	 * @see org.xml.sax.ContentHandler#setDocumentLocator(org.xml.sax.Locator)
	 */
	public void setDocumentLocator(Locator locator)
	{
		m_handler.setDocumentLocator(locator);
	}

	/**
	 * @see org.xml.sax.ContentHandler#skippedEntity(java.lang.String)
	 */
	public void skippedEntity(String name) throws SAXException
	{
		m_handler.skippedEntity(name);
	}

	/**
	 * @see org.xml.sax.ContentHandler#startDocument()
	 */
	public void startDocument() throws SAXException
	{
		if (m_indent == 0)
			m_handler.startDocument();
	}

	/**
	 * @see org.xml.sax.ContentHandler#startPrefixMapping(java.lang.String,
	 *      java.lang.String)
	 */
	public void startPrefixMapping(String prefix, String uri)
			throws SAXException
	{
		// m_handler.startPrefixMapping(
		// m_prefixProvider.getPreferredPrefix(uri, prefix), uri );
		m_handler.startPrefixMapping(prefix, uri);
	}
}
