package org.kalypso.ogc.sensor.timeseries.wq;

import java.io.StringWriter;
import java.text.SimpleDateFormat;
import java.util.Iterator;
import java.util.List;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import org.kalypso.wechmann.Wechmann;
import org.kalypso.wechmann.WechmannType;
import org.kalypso.wechmann.XMLWechmannParams;
import org.kalypso.wechmann.XMLWechmannSet;
import org.kalypso.wechmann.XMLWechmannSet.ValidityType;
import org.xml.sax.InputSource;

/**
 * Parses and generates XML for the Wechmann parameters.
 * 
 * Returns a simple XML-Representation of this object. The format of the XML is
 * as follows:
 * 
 * <pre>
 *      &lt;set&gt;
 *       &lt;validity&gt;15.10.2004 17:53:17&lt;/validity&gt;
 *       &lt;params&gt;
 *         &lt;w1&gt;-38,12000&lt;/w1&gt;
 *         &lt;lnk1&gt;-7,87274&lt;/lnk1&gt;
 *         &lt;k2&gt;2,25925&lt;/k2&gt;
 *         &lt;wgr&gt;170,00000&lt;/wgr&gt;
 *       &lt;/params&gt;
 *       &lt;params&gt;
 *         &lt;w1&gt;-43,32000&lt;/w1&gt;
 *         &lt;lnk1&gt;-7,24065&lt;/lnk1&gt;
 *         &lt;k2&gt;2,13100&lt;/k2&gt;
 *       &lt;/params&gt;
 *      &lt;/set&gt;
 * 		&lt;set&gt;
 * 			...
 * 		&lt;/set&gt;
 * </pre>
 * 
 * <p>
 * The attribute validity is optional, if no attribute is provided, it takes the
 * minimum Date that is delivered by the <code>DateUtilities.getMinimum()</code>
 * method.
 * 
 * @author schlienger
 */
public class WechmannFactory
{
  private final static org.kalypso.wechmann.ObjectFactory m_objectFactory = new org.kalypso.wechmann.ObjectFactory();

  private WechmannFactory( )
  {
    // not to be instanciated
  }

  /**
   * Parses the xml and creates a WechmannSets object.
   * 
   * @param ins
   * @return newly created WechmannGroup object
   * @throws WechmannException
   */
  public static WechmannGroup parse(final InputSource ins)
      throws WechmannException
  {
    try
    {
      final Unmarshaller unm = m_objectFactory.createUnmarshaller();

      WechmannType wm = (WechmannType) unm.unmarshal( ins );

      final WechmannSet[] sets = new WechmannSet[wm.getSet().size()];
      int i = 0;

      for( Iterator it = wm.getSet().iterator(); it.hasNext(); )
      {
        final XMLWechmannSet wset = (XMLWechmannSet) it.next();

        final WechmannParams[] wparams = new WechmannParams[wset.getParams()
            .size()];
        int j = 0;

        for( Iterator itp = wset.getParams().iterator(); itp.hasNext(); )
        {
          XMLWechmannParams wp = (XMLWechmannParams) itp.next();

          double k2 = wp.getK2();
          double lnk1 = wp.getLnk1();
          double w1 = wp.getW1();
          double wgr = wp.getWgr(); // if not existing defaults to -1
          // (see schema)

          // wgr is optional
          if( wgr == -1 )
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
          final SimpleDateFormat df = new SimpleDateFormat( wset.getValidity()
              .getFormat() );

          sets[i] = new WechmannSet( df.parse( wset.getValidity().getValue() ),
              wparams );
        }
      }

      return new WechmannGroup( sets );
    }
    catch( Exception e ) // generic exception caught for simplicity
    {
      throw new WechmannException( e );
    }
  }

  /**
   * Creates a XML-String from the given WechmannGroup object.
   * 
   * @param wg
   * @return xml String
   * @throws JAXBException
   */
  public static String createXMLString(final WechmannGroup wg)
      throws JAXBException
  {
    final Wechmann wt = m_objectFactory.createWechmann();

    final List sets = wt.getSet();

    final SimpleDateFormat df = new SimpleDateFormat( "dd.MM.yyyy HH:mm:ss" );

    for( final Iterator it = wg.iterator(); it.hasNext(); )
    {
      final WechmannSet wset = (WechmannSet) it.next();

      final XMLWechmannSet wechmannSet = m_objectFactory.createXMLWechmannSet();
      final ValidityType validityType = m_objectFactory
          .createXMLWechmannSetValidityType();

      validityType.setFormat( df.toPattern() );
      validityType.setValue( df.format( wset.getValidity() ) );
      wechmannSet.setValidity( validityType );

      for( final Iterator itp = wset.iterator(); itp.hasNext(); )
      {
        final WechmannParams wp = (WechmannParams) itp.next();

        final XMLWechmannParams wechmannParams = m_objectFactory
            .createXMLWechmannParams();
        wechmannParams.setK2( wp.getK2() );
        wechmannParams.setLnk1( wp.getLNK1() );
        wechmannParams.setW1( wp.getW1() );

        if( wp.hasWGR() )
          wechmannParams.setWgr( wp.getWGR() );
        else
          wechmannParams.setWgr( -1 );

        wechmannSet.getParams().add( wechmannParams );
      }

      sets.add( wechmannSet );
    }

    final Marshaller marshaller = m_objectFactory.createMarshaller();

    final StringWriter writer = new StringWriter();
    marshaller.marshal( wt, writer );

    return writer.toString();
  }
}