package org.kalypso.ogc.sensor.timeseries.wq;

import java.text.SimpleDateFormat;
import java.util.Iterator;

import javax.xml.bind.Unmarshaller;

import org.xml.sax.InputSource;


/**
 * Parses and generates XML for the Wechmann parameters.
 * 
 * @author schlienger
 */
public class WechmannFactory
{
  private final static org.kalypso.wechmann.ObjectFactory m_objectFactory = new org.kalypso.wechmann.ObjectFactory();
  
  private WechmannFactory()
  {
    // not to be instanciated
  }
  
  /**
   * Parses the xml and creates a WechmannSets object.
   */
  public static WechmannSets parse( final InputSource ins ) throws WechmannException
  {
    try
    {
      final Unmarshaller unm = m_objectFactory.createUnmarshaller();
      
      org.kalypso.wechmann.WechmannType wm = (org.kalypso.wechmann.Wechmann)unm.unmarshal( ins );

      final WechmannSet[] sets = new WechmannSet[wm.getSet().size()];
      int i=0;
      
      for( Iterator it = wm.getSet().iterator(); it.hasNext(); )
      {
        org.kalypso.wechmann.WechmannSet wset = (org.kalypso.wechmann.WechmannSet)it.next();
        
        final WechmannParams[] wparams = new WechmannParams[wset.getParams().size()];
        int j=0;
        
        for( Iterator itp = wset.getParams().iterator(); itp.hasNext(); )
        {
          org.kalypso.wechmann.WechmannParams wp = (org.kalypso.wechmann.WechmannParams)itp.next();
          
          double k2 = wp.getK2();
          double lnk1 = wp.getLnk1();
          double w1 = wp.getW1();
          double wgr = wp.getWgr(); // if not existing defaults to -1 (see schema)
          
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
          final SimpleDateFormat df = new SimpleDateFormat( wset.getValidity().getFormat() );
        
          sets[i] = new WechmannSet( df.parse( wset.getValidity().getValue() ), wparams  );
        }
      }
      
      return new WechmannSets( sets );
    }
    catch( Exception e ) // generic exception caught for simplicity
    {
      throw new WechmannException( e );
    }
  }
}
