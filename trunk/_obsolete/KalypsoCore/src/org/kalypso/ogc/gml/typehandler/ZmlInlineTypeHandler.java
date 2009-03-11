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

import javax.xml.bind.Marshaller;
import javax.xml.namespace.QName;

import org.apache.commons.io.IOUtils;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.zml.Observation;
import org.kalypsodeegree.model.typeHandler.XsdBaseTypeHandler;
import org.xml.sax.InputSource;

/**
 * @author kuepfer
 */
public class ZmlInlineTypeHandler extends XsdBaseTypeHandler<IObservation>
{
  public static final String NAMESPACE = "inline.zml.kalypso.org"; //$NON-NLS-1$

  protected final String[] m_axisTypes;

  public ZmlInlineTypeHandler( final String name, final String[] axisTypes, final Class<IObservation> clazz )
  {
    super( new QName( NAMESPACE, name ), clazz );

    m_axisTypes = axisTypes;
  }

  /**
   * @see org.kalypsodeegree.model.typeHandler.XsdBaseTypeHandler#convertToXMLString(java.lang.Object)
   */
  @Override
  public String convertToXMLString( final IObservation value )
  {
    try
    {
      final StringWriter sw = new StringWriter();
      final Observation xml = ZmlFactory.createXML( value, null, null );
      final Marshaller marshaller = ZmlFactory.getMarshaller();
      marshaller.marshal( xml, sw );
      sw.close();
      return sw.toString();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return e.toString();
    }
  }

  /**
   * @see org.kalypsodeegree.model.typeHandler.XsdBaseTypeHandler#convertToJavaValue(java.lang.String)
   */
  @Override
  public IObservation convertToJavaValue( final String zmlStr )
  {
    if( zmlStr == null || zmlStr.isEmpty() )
      return null;

    final StringReader reader = new StringReader( zmlStr.trim() );
    try
    {
      final IObservation obs = ZmlFactory.parseXML( new InputSource( reader ), "null-id", null );
      reader.close();
      return obs;
    }
    catch( final SensorException e )
    {
      e.printStackTrace();
      return null;
    }
    finally
    {
      IOUtils.closeQuietly( reader );
    }
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#getShortname()
   */
  @Override
  public String getShortname( )
  {
    return "ZmlInline"; //$NON-NLS-1$
  }

  /**
   * @return axistypes as String[]
   */
  public String[] getAxisTypes( )
  {
    return m_axisTypes;
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#parseType(java.lang.String)
   */
  @Override
  public Object parseType( final String text )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
   */
  @Override
  public int compare( final IObservation o1, final IObservation o2 )
  {
    throw new UnsupportedOperationException();
  }

  // TODO: these do NOT belong here!
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
    final public static String[] axis = new String[] { TimeserieConstants.TYPE_MIN, TimeserieConstants.TYPE_RAINFALL };
  }

}
