/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.ogc.gml.typehandler;

import java.io.StringReader;
import java.io.StringWriter;
import java.net.URL;

import javax.xml.bind.Marshaller;

import org.kalypso.contribs.java.net.IUrlResolver;
import org.kalypso.contribs.java.xml.XMLUtilities;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.zml.ObservationType;
import org.kalypsodeegree.xml.XMLTools;
import org.kalypsodeegree_impl.extension.IMarshallingTypeHandler;
import org.kalypsodeegree_impl.extension.TypeRegistryException;
import org.w3c.dom.Node;
import org.xml.sax.InputSource;

/**
 * 
 * TODO: insert type comment here
 * 
 * @author kuepfer
 */
public class ZmlInlineTypeHandler implements IMarshallingTypeHandler
{

  public static final String NAMESPACE = "inline.zml.kalypso.org";

  private static final String NAME = "ZmlInlineType";

  public static final String CLASS_NAME = IObservation.class.getName();

  public static final String TYPE_NAME = NAMESPACE + ":" + NAME;


  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#marshall(java.lang.Object, org.w3c.dom.Node,
   *      java.net.URL)
   */
  public void marshall( Object object, Node node, URL context ) throws TypeRegistryException
  {
    try
    {
      ObservationType xml = ZmlFactory.createXML( (IObservation)object, null, null );
      Marshaller marshaller = ZmlFactory.getMarshaller();
      StringWriter sw = new StringWriter();
      marshaller.marshal( xml, sw );
      XMLUtilities.setTextNode( node.getOwnerDocument(), node, sw.toString() );
      sw.close();
    }
    catch( Exception e )
    {
      throw new TypeRegistryException( e.getLocalizedMessage() );
    }

  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#unmarshall(org.w3c.dom.Node, java.net.URL,
   *      org.kalypso.contribs.java.net.IUrlResolver)
   */
  public Object unmarshall( Node node, URL context, IUrlResolver urlResolver ) throws TypeRegistryException
  {
    final String zmlStr = XMLTools.getStringValue( node );
    final StringReader reader = new StringReader( zmlStr.trim() );
    IObservation obs = null;
    try
    {
      obs = ZmlFactory.parseXML( new InputSource( reader ), "null-id", null );
    }
    catch( SensorException e )
    {
      e.printStackTrace();
    }
    reader.close();
    return obs;
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#getShortname()
   */
  public String getShortname()
  {
    return "ZmlInline";
  }

  /**
   * @see org.kalypsodeegree_impl.extension.ITypeHandler#getClassName()
   */
  public String getClassName()
  {
    return CLASS_NAME;
  }

  /**
   * @see org.kalypsodeegree_impl.extension.ITypeHandler#getTypeName()
   */
  public String getTypeName()
  {
    return TYPE_NAME;
  }

}
