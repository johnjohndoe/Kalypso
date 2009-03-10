/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.sensor.timeseries.wq.wechmann;

import java.io.StringWriter;
import java.text.SimpleDateFormat;
import java.util.Iterator;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import org.kalypso.commons.bind.JaxbUtilities;
import org.kalypso.ogc.sensor.timeseries.wq.WQException;
import org.kalypso.wechmann.Wechmann;
import org.kalypso.wechmann.XMLWechmannParams;
import org.kalypso.wechmann.XMLWechmannSet;
import org.kalypso.wechmann.XMLWechmannSet.Validity;
import org.xml.sax.InputSource;

/**
 * Parses and generates XML for the Wechmann parameters. Returns a simple XML-Representation of this object. The format
 * of the XML is as follows:
 * 
 * <pre>
 *           &lt;set&gt;
 *            &lt;validity&gt;15.10.2004 17:53:17&lt;/validity&gt;
 *            &lt;params&gt;
 *              &lt;w1&gt;-38,12000&lt;/w1&gt;
 *              &lt;lnk1&gt;-7,87274&lt;/lnk1&gt;
 *              &lt;k2&gt;2,25925&lt;/k2&gt;
 *              &lt;wgr&gt;170,00000&lt;/wgr&gt;
 *            &lt;/params&gt;
 *            &lt;params&gt;
 *              &lt;w1&gt;-43,32000&lt;/w1&gt;
 *              &lt;lnk1&gt;-7,24065&lt;/lnk1&gt;
 *              &lt;k2&gt;2,13100&lt;/k2&gt;
 *            &lt;/params&gt;
 *           &lt;/set&gt;
 *      		&lt;set&gt;
 *      			...
 *      		&lt;/set&gt;
 * </pre>
 * 
 * <p>
 * The attribute validity is optional, if no attribute is provided, it takes the minimum Date that is delivered by the
 * <code>DateUtilities.getMinimum()</code> method.
 * 
 * @author schlienger
 */
public class WechmannFactory
{
  private final static org.kalypso.wechmann.ObjectFactory OF_WECHMANN = new org.kalypso.wechmann.ObjectFactory();

  private final static JAXBContext JC_WECHMANN = JaxbUtilities.createQuiet( org.kalypso.wechmann.ObjectFactory.class );

  private WechmannFactory( )
  {
    // not to be instanciated
  }

  /**
   * Parses the xml and creates a WechmannSets object.
   * 
   * @param ins
   * @return newly created WechmannGroup object
   * @throws WQException
   */
  public static WechmannGroup parse( final InputSource ins ) throws WQException
  {
    try
    {
      final Unmarshaller unm = JC_WECHMANN.createUnmarshaller();
      final Wechmann wm = (Wechmann) unm.unmarshal( ins );
      final WechmannSet[] sets = new WechmannSet[wm.getSet().size()];
      int i = 0;
      for( Iterator<XMLWechmannSet> it = wm.getSet().iterator(); it.hasNext(); )
      {
        final XMLWechmannSet wset = it.next();
        final WechmannParams[] wparams = new WechmannParams[wset.getParams().size()];
        int j = 0;
        for( final XMLWechmannParams wp : wset.getParams() )
        {
          final double k2 = wp.getK2();
          final double lnk1 = wp.getLnk1();
          final double w1 = wp.getW1();
          final Double wgr = wp.getWgr(); // if not existing defaults to -1
          // (see schema)
          // wgr is optional
          if( wgr == null )
            wparams[j] = new WechmannParams( w1, lnk1, k2 );
          else
            wparams[j] = new WechmannParams( w1, lnk1, k2, wgr );
          j++;
        }
        // validity is optional
        if( wset.getValidity() == null )
          sets[i] = new WechmannSet( wparams );
        else
        {
          final SimpleDateFormat df = new SimpleDateFormat( wset.getValidity().getFormat() );
          sets[i] = new WechmannSet( df.parse( wset.getValidity().getValue() ), wparams );
        }
        i++;
      }
      return new WechmannGroup( sets );
    }
    catch( Exception e ) // generic exception caught for simplicity
    {
      throw new WQException( e );
    }
  }

  /**
   * Creates a XML-String from the given WechmannGroup object.
   * 
   * @param wg
   * @return xml String
   * @throws WQException
   */
  public static String createXMLString( final WechmannGroup wg ) throws WQException
  {
    try
    {
      final Wechmann wt = OF_WECHMANN.createWechmann();
      final List<XMLWechmannSet> sets = wt.getSet();
      final SimpleDateFormat df = new SimpleDateFormat( "yyyy-MM-dd'T'HH:mm:ss" ); //$NON-NLS-1$
      for( final Iterator<WechmannSet> it = wg.iterator(); it.hasNext(); )
      {
        final WechmannSet wset = it.next();
        final XMLWechmannSet wechmannSet = OF_WECHMANN.createXMLWechmannSet();
        final Validity validityType = OF_WECHMANN.createXMLWechmannSetValidity();
        validityType.setFormat( df.toPattern() );
        validityType.setValue( df.format( wset.getValidity() ) );
        wechmannSet.setValidity( validityType );
        for( final Iterator<WechmannParams> itp = wset.iterator(); itp.hasNext(); )
        {
          final WechmannParams wp = itp.next();
          final XMLWechmannParams wechmannParams = OF_WECHMANN.createXMLWechmannParams();
          wechmannParams.setK2( wp.getK2() );
          wechmannParams.setLnk1( wp.getLNK1() );
          wechmannParams.setW1( wp.getW1() );
          if( wp.hasWGR() )
            wechmannParams.setWgr( new Double( wp.getWGR() ) );
          else
            wechmannParams.setWgr( new Double( -1 ) );
          wechmannSet.getParams().add( wechmannParams );
        }
        sets.add( wechmannSet );
      }
      final Marshaller marshaller = JaxbUtilities.createMarshaller( JC_WECHMANN );
      marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );
      final StringWriter writer = new StringWriter();
      marshaller.marshal( wt, writer );
      return writer.toString();
    }
    catch( JAXBException e )
    {
      throw new WQException( e );
    }
  }
}