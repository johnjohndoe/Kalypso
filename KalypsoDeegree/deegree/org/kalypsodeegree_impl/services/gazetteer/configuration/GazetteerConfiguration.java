// $Header:
// /cvsroot/deegree/deegree/org/deegree_impl/services/gazetteer/configuration/GazetteerConfiguration.java,v
// 1.2 2004/03/26 11:19:31 poth Exp $
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
package org.deegree_impl.services.gazetteer.configuration;

import java.util.HashMap;

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
public class GazetteerConfiguration
{
  private HashMap ltyp = new HashMap();

  private Relation[] relations = null;

  /** Creates a new instance of Configuration */
  public GazetteerConfiguration( String name, Relation[] relations )
  {
    this.relations = relations;

    for( int i = 0; i < relations.length; i++ )
    {
      String s = relations[i].getSourceLocationType() + ":" + relations[i].getTargetLocationType();
      ltyp.put( s, relations[i] );
    }
  }

  /**
   * returns a list of all relations defined for a gazetteer
   * 
   * @return
   */
  public Relation[] getRelations()
  {
    return relations;
  }

  /**
   * returns the relation between the two passed location types. If no relation
   * is defined <tt>null</tt> will be returned
   * 
   * @param sourceLocationType
   * @param targetLocationType
   * 
   * @return
   */
  public Relation getRelation( String sourceLocationType, String targetLocationType )
  {
    return (Relation)ltyp.get( sourceLocationType + ":" + targetLocationType );
  }

}
/*******************************************************************************
 * Changes to this class. What the people have been up to: $Log:
 * GazetteerConfiguration.java,v $ Revision 1.2 2004/03/26 11:19:31 poth no
 * message
 * 
 * Revision 1.1 2004/03/24 12:36:22 poth no message
 * 
 * 
 *  
 ******************************************************************************/