// $Header:
// /var/lib/cvs/backupdeegree/deegree/org/deegree_impl/services/gazetteer/GazetteerFactory.java,v
// 1.1.1.1 2004/05/11 16:43:27 doemming Exp $
/*----------------    FILE HEADER  ------------------------------------------

 This file is part of deegree.
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon Fitzke/Fretter/Poth GbR
 http://www.lat-lon.de

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

 Andreas Poth
 lat/lon Fitzke/Fretter/Poth GbR
 Meckenheimer Allee 176
 53115 Bonn
 Germany
 E-Mail: poth@lat-lon.de

 Jens Fitzke
 Department of Geography
 University of Bonn
 Meckenheimer Allee 166
 53115 Bonn
 Germany
 E-Mail: jens.fitzke@uni-bonn.de

 
 ---------------------------------------------------------------------------*/
package org.deegree_impl.services.gazetteer;

import java.util.Date;
import java.util.HashMap;

import org.deegree.model.geometry.GM_Object;
import org.deegree.model.geometry.GM_Point;
import org.deegree.services.gazetteer.GazetteerException;
import org.deegree.services.gazetteer.SI_LocationInstance;
import org.deegree.services.gazetteer.SI_LocationType;
import org.deegree.services.wcas.metadatadesc.CitedResponsibleParty;
import org.deegree.xml.ElementList;
import org.deegree.xml.XMLTools;
import org.deegree_impl.model.geometry.GMLAdapter;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.TimeTools;
import org.w3c.dom.Element;

/**
 * 
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @author last edited by: $Author$
 * 
 * @version 1.0. $Revision$, $Date$
 * 
 * @since 1.1
 */
public class GazetteerFactory
{

  private static String LINS = null;//"http://www.opengis.net/wfs-g";

  private static String WFSNS = "http://www.opengis.net/wfs";

  private static String GMLNS = "http://www.opengis.net/gml";

  /**
   * creates an array of <tt>SI_LocationInstance</tt> s from the passed
   * <tt>Document</tt>
   * 
   * @param root
   *          <tt>Element</tt> containing an XML representation of one ore
   *          more <tt>SI_LocationInstance</tt> s
   * @return @throws
   *         GazetteerException
   */
  public static SI_LocationInstance[] createLocationInstances( Element root )
      throws GazetteerException
  {
    Debug.debugMethodBegin();

    ElementList el = XMLTools.getChildElementsByName( "featureMember", GMLNS, root );

    SI_LocationInstance[] lis = new SI_LocationInstance[el.getLength()];
    for( int i = 0; i < lis.length; i++ )
    {
      lis[i] = createLocationInstance( el.item( i ) );
    }

    Debug.debugMethodEnd();
    return lis;
  }

  /**
   * creates a <tt>SI_LocationInstance</tt> from the passed <tt>Element</tt>
   * 
   * @param element
   *          containing an XML representation of a <tt>SI_LocationInstance</tt>
   * @return @throws
   *         GazetteerException
   */
  public static SI_LocationInstance createLocationInstance( Element element )
      throws GazetteerException
  {
    Debug.debugMethodBegin();

    SI_LocationInstance li = null;

    HashMap map = new HashMap();
    element = XMLTools.getFirstElement( element );
    ElementList el = XMLTools.getChildElements( element );
    // loop over all Attributes
    for( int i = 0; i < el.getLength(); i++ )
    {
      String name = XMLTools.toLocalName( el.item( i ).getNodeName() );
      Object value = XMLTools.getStringValue( el.item( i ) );
      if( name.equals( "position" ) || name.equals( "geographicExtent" ) )
      {
        Element elem = XMLTools.getChildElements( el.item( i ) ).item( 0 );
        if( elem.getNodeName().endsWith( "GM_Envelope" ) )
        {
          // TODO wrap GM_Envelope
        }
        else
        {
          try
          {
            value = GMLAdapter.wrap( XMLTools.getStringValue( elem ) );
            if( name.equals( "position" ) )
            {
              value = new GM_Point[]
              { (GM_Point)value };
            }
            else
            {
              value = new GM_Object[]
              { (GM_Object)value };
            }
          }
          catch( Exception e )
          {
            throw new GazetteerException( "", e );
          }
        }
      }
      else if( name.equals( "children" ) || name.equals( "parents" ) )
      {
        // first child is a feature collection
        Element elem = XMLTools.getChildElements( el.item( i ) ).item( 0 );
        value = createLocationInstances( elem );
      }
      else if( name.equals( "SI_LocationType" ) )
      {

      }
      else if( name.equals( "sourceFeature" ) )
      {

      }
      else if( name.equals( "administrator" ) )
      {

      }
      else if( name.equals( "begin" ) || name.equals( "end" ) )
      {
        value = TimeTools.createCalendar( (String)value ).getTime();
      }
      map.put( name, value );
    }

    li = createLocationInstance( map );

    Debug.debugMethodEnd();
    return li;
  }

  public static SI_LocationInstance createLocationInstance( HashMap map ) throws GazetteerException
  {
    Debug.debugMethodBegin();
    SI_LocationInstance li = new SI_LocationInstance_Impl(
        (String)map.get( "geographicIdentifier" ), (String)map
            .get( "alternativeGeographicIdentifier" ), (String)map.get( "identifier" ), (Date)map
            .get( "begin" ), (Date)map.get( "end" ), (GM_Point[])map.get( "position" ),
        (GM_Object[])map.get( "geographicExtent" ), (CitedResponsibleParty)map
            .get( "administrator" ), (SI_LocationInstance[])map.get( "parents" ),
        (SI_LocationInstance[])map.get( "children" ), (SI_LocationType)map.get( "locationType" ) );
    Debug.debugMethodEnd();
    return li;
  }

}
/*******************************************************************************
 * Changes to this class. What the people have been up to: $Log:
 * GazetteerFactory.java,v $ Revision 1.1.1.1 2004/05/11 16:43:27 doemming
 * backup of local modified deegree sources
 * 
 * Revision 1.6 2004/03/26 16:42:18 poth no message
 * 
 * Revision 1.5 2004/03/26 11:19:29 poth no message
 * 
 * Revision 1.4 2004/03/24 08:12:20 poth no message
 * 
 * Revision 1.3 2004/03/16 08:07:24 poth no message
 * 
 * Revision 1.2 2004/03/15 16:55:29 poth no message
 * 
 * Revision 1.1 2004/03/15 07:39:00 poth no message
 * 
 * 
 *  
 ******************************************************************************/