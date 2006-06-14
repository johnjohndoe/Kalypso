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
import java.math.BigDecimal;
import java.net.URL;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.namespace.QName;

import org.apache.commons.io.IOUtils;
import org.kalypso.commons.factory.FactoryException;
import org.kalypso.contribs.java.net.IUrlResolver;
import org.kalypso.gmlschema.types.AbstractOldFormatMarshallingTypeHandlerAdapter;
import org.kalypso.gmlschema.types.TypeRegistryException;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.zml.Observation;
import org.kalypsodeegree.xml.XMLTools;
import org.w3c.dom.CDATASection;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.xml.sax.InputSource;

/**
 * @author kuepfer
 */
public class ZmlInlineTypeHandler extends AbstractOldFormatMarshallingTypeHandlerAdapter
{
  // public static final class CLASS_ZmlInlineIdealKcWtLaiType=new
  public static final String NAMESPACE = "inline.zml.kalypso.org";

  public final QName m_typeName;

  protected final String[] m_axisTypes;

  private final Class m_className;

  public ZmlInlineTypeHandler( final String name, final String[] axisTypes, final Class clazz )
  {
    m_axisTypes = axisTypes;
    m_typeName = new QName( NAMESPACE, name );
    m_className = clazz;
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#marshall(java.lang.Object, org.w3c.dom.Node,
   *      java.net.URL)
   */
  @Override
  public void marshall( final Object object, final Node node, final URL context ) throws TypeRegistryException
  {
    final StringWriter sw = new StringWriter();
    try
    {
      final Observation xml = ZmlFactory.createXML( (IObservation) object, null, null );
      final Marshaller marshaller = ZmlFactory.getMarshaller();
      marshaller.marshal( xml, sw );
      sw.close();
      final String toString = sw.toString();
      final Document ownerDocument = node.getOwnerDocument();
      // Text text = ownerDocument.createTextNode( toString );
      // node.appendChild( text );
      final CDATASection section = ownerDocument.createCDATASection( toString );
      node.appendChild( section );
      // XMLUtilities.setTextNode( node.getOwnerDocument(), node, toString );
    }
    catch( final Exception e )
    {
      throw new TypeRegistryException( e );
    }
    finally
    {
      IOUtils.closeQuietly( sw );
    }
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#unmarshall(org.w3c.dom.Node, java.net.URL,
   *      org.kalypso.contribs.java.net.IUrlResolver)
   */
  @Override
  public Object unmarshall( final Node node, final URL context, final IUrlResolver urlResolver ) throws TypeRegistryException
  {
    final String zmlStr = XMLTools.getStringValue( node );
    if( zmlStr == null || zmlStr.length() < 1 )
      return null;
    final StringReader reader = new StringReader( zmlStr.trim() );
    IObservation obs = null;
    try
    {
      obs = ZmlFactory.parseXML( new InputSource( reader ), "null-id", null );
      reader.close();
    }
    catch( final SensorException e )
    {
      throw new TypeRegistryException( e );
    }
    finally
    {
      IOUtils.closeQuietly( reader );
    }
    return obs;
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#getShortname()
   */
  public String getShortname( )
  {
    return "ZmlInline";
  }

  /**
   * @see org.kalypsodeegree_impl.extension.ITypeHandler#getClassName()
   */
  public Class getValueClass( )
  {
    return m_className;
  }

  /**
   * @see org.kalypsodeegree_impl.extension.ITypeHandler#getTypeName()
   */
  public QName getTypeName( )
  {
    return m_typeName;
  }

  /**
   * @return axistypes as String[]
   */
  public String[] getAxisTypes( )
  {
    return m_axisTypes;
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#cloneObject(java.lang.Object)
   */
  public Object cloneObject( Object objectToClone )
  {
    final StringWriter sw = new StringWriter();
    IObservation clone = null;
    try
    {
      final Observation xml = ZmlFactory.createXML( (IObservation) objectToClone, null, null );
      final Marshaller marshaller = ZmlFactory.getMarshaller();
      marshaller.marshal( xml, sw );
      final StringReader reader = new StringReader( sw.toString().trim() );
      clone = ZmlFactory.parseXML( new InputSource( reader ), null, null );
    }
    catch( FactoryException e )
    {
      e.printStackTrace();
    }
    catch( JAXBException e )
    {
      e.printStackTrace();
    }
    catch( SensorException e )
    {
      e.printStackTrace();
    }
    return clone;
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#parseType(java.lang.String)
   */
  public Object parseType( final String text )
  {
    throw new UnsupportedOperationException();
  }

  public interface TA extends IObservation
  {
    final public static String[] axis = new String[] { TimeserieConstants.TYPE_HOURS, TimeserieConstants.TYPE_NORM };
  }

  public interface WtKcLai extends IObservation
  {
    final public static String[] axis = new String[] { TimeserieConstants.TYPE_DATE, TimeserieConstants.TYPE_LAI, TimeserieConstants.TYPE_WT, TimeserieConstants.TYPE_KC };
  }

  public interface WVQ extends IObservation
  {
    final public static String[] axis = new String[] { TimeserieConstants.TYPE_NORMNULL, TimeserieConstants.TYPE_VOLUME, TimeserieConstants.TYPE_RUNOFF };
  }

  public interface TN extends IObservation
  {
    final public static String[] axis = new String[] { TimeserieConstants.TYPE_HOURS, TimeserieConstants.TYPE_RAINFALL };
  }

  /**
   * @see org.kalypso.gmlschema.types.ITypeHandler#isGeometry()
   */
  public boolean isGeometry( )
  {
    return false;
  }

}
