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
package org.deegree.services.gazetteer;

import org.deegree.model.geometry.GM_Object;
import org.deegree.services.wcas.metadatadesc.CitedResponsibleParty;

/**
 * This allows for either an inline description or a reference to such a
 * description. The reference is an xlink allowing both human readable labels
 * and definitive identifiers. A gazetteer response may optionally include a
 * block for such definitions
 * 
 * @version $Revision$
 * @author $author$
 */
public interface SI_LocationType
{

  /**
   * ISO DIS 19112 references RS_Identifier (but not from ISO DIS 19115). The
   * allowable values are free text.
   * 
   * @return
   */
  String getName();

  /**
   * property used as the defining characteristic of the location type
   * 
   * @return
   */
  String getTheme();

  /**
   * method of uniquely identifying location instances
   * 
   * @return
   */
  String getIdentifier();

  /**
   * the way in which location instances are defined
   * 
   * @return
   */
  String getDefinition();

  /**
   * name of organization or class of organization able to create and destroy
   * location instances
   * 
   * @return
   */
  CitedResponsibleParty getOwner();

  /**
   * name of parent location type (a location type of which this location type
   * is a subdivision)
   */
  SI_LocationType[] getParent();

  /**
   * name of child location type (a location type which sub-divides this
   * location type)
   */
  SI_LocationType[] getChild();

  /**
   * geographic area within which the location type occurs
   * 
   * @return
   */
  GM_Object getTerritoryOfUse();

}