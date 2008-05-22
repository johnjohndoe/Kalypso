/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always. 
 * 
 * If you intend to use this software in other ways than in kalypso 
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree, 
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree.xml;

import java.io.OutputStream;
import java.io.StringReader;

import javax.xml.transform.OutputKeys;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.commons.io.IOUtils;
import org.w3c.dom.Document;

/**
 * @author doemming
 */
public class MarshallUtilities
{

  public static void marshall( Marshallable xmlObject, OutputStream os ) throws TransformerFactoryConfigurationError, Exception
  {
    try
    {
      final Document doc = XMLTools.parse( new StringReader( (xmlObject.exportAsXML()) ) );
      final Source source = new DOMSource( doc );
      Result result = new StreamResult( os );
      Transformer t = TransformerFactory.newInstance().newTransformer();
      t.setOutputProperty( "{http://xml.apache.org/xslt}indent-amount", "2" );
      t.setOutputProperty( OutputKeys.INDENT, "yes" );
      t.transform( source, result );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw e;
    }
    finally
    {
      IOUtils.closeQuietly( os );
    }
  }
}
