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
package org.deegree_impl.clients.wcasclient.control;

import org.deegree.services.wfs.filterencoding.Filter;
import org.deegree.services.wfs.filterencoding.Operation;
import org.deegree.services.wfs.protocol.WFSGetFeatureRequest;
import org.deegree.services.wfs.protocol.WFSQuery;
import org.deegree_impl.services.wfs.filterencoding.ComplexFilter;
import org.deegree_impl.services.wfs.filterencoding.Literal;
import org.deegree_impl.services.wfs.filterencoding.PropertyIsLikeOperation;
import org.deegree_impl.services.wfs.filterencoding.PropertyName;
import org.deegree_impl.services.wfs.protocol.WFSProtocolFactory;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.IDGenerator;

/**
 * 
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public class ThesaurusRequestFactory
{
  /**
   * creates a GetFeature request against a thesaurus. the passed term will be
   * compared with the TH_TERM field
   * 
   * @param pattern
   * 
   * @return
   */
  public static WFSGetFeatureRequest createRequest( String pattern )
  {
    Debug.debugMethodBegin();

    Operation op = new PropertyIsLikeOperation( new PropertyName( "TH_TERM" ), new Literal( "*"
        + pattern + "*" ), '*', '?', '/' );
    Filter filter = new ComplexFilter( op );

    WFSQuery query = WFSProtocolFactory.createQuery( new String[]
    { "TH_TERM", "TH_DESC_NO" }, null, "1.0.0", "Thesaurus", filter );
    IDGenerator idg = IDGenerator.getInstance();
    WFSGetFeatureRequest request = WFSProtocolFactory.createWFSGetFeatureRequest( "1.0.0", ""
        + idg.generateUniqueID(), null, null, "GML2", null, null, -1, 0, new WFSQuery[]
    { query } );

    Debug.debugMethodEnd();
    return request;
  }

  /**
   * 
   * 
   * @param pattern
   * 
   * @return
   */
  public WFSGetFeatureRequest createBroderTermRequest( String[] pattern )
  {
    Debug.debugMethodBegin();

    Debug.debugMethodEnd();
    return null;
  }

  /**
   * 
   * 
   * @param pattern
   * 
   * @return
   */
  public WFSGetFeatureRequest createNarrowerTermRequest( String[] pattern )
  {
    Debug.debugMethodBegin();

    Debug.debugMethodEnd();
    return null;
  }
}