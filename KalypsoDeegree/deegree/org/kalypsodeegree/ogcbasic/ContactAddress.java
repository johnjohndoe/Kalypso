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

package org.deegree.ogcbasic;

/**
 * specifies the data structur of a address and the access to its components
 * based on ISO 19115
 * <p>----------------------------------------------------------------------</p>
* @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version 2002-03-01
 */
public interface ContactAddress {

   /**
    * returns the address type. e.g. 'postal'
    */
    String getAddressType();

   /**
    * returns the address. usally this is the street and number of a building. It
    * also can be a p.o. box
    */
    String getAddress();

   /**
    * returns the name of the city
    */
    String getCity();

   /**
    * returns the name of the state or province of the address. If no state or
    * province is known or nessecary <tt>null</tt> will be returned.
    */
    String getStateOrProvince();

   /**
    * returns the post code. This doesn't contain an abbreviation for the country
    */
    String getPostCode();

   /**
    * returns the name of the country. this should be the complete name and not
    * an abbreviation.
    */
    String getCountry();

}
