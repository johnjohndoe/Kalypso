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

package org.deegree.services.wcas.metadatadesc;

/**
 * CitationType.java
 * 
 * Created on 11. September 2002, 15:55
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:schaefer@lat-lon.de">Axel Schaefer </a>
 * @version $Revision$ $Date$
 *  
 */
public interface Citation
{

  /**
   * @return
   */
  String getTitle();

  /**
   * minOccurs="0" maxOccurs="unbounded"
   * 
   * @return
   */
  String[] getAlternateTitle();

  /**
   * maxOccurs="unbounded"
   * 
   * @return
   */
  Date[] getDate();

  /**
   * minOccurs="0"
   * 
   * @return String
   */
  String getEdition();

  /**
   * minOccurs="0"
   * 
   * @return
   */
  String getEditionDate();

  /**
   * minOccurs="0" maxOccurs="unbounded"
   * 
   * @return
   */
  String[] getIdentifier();

  /**
   * minOccurs="0" maxOccurs="unbounded"
   * 
   * @return
   */
  String[] getIdentifierType();

  /**
   * minOccurs="0" maxOccurs="unbounded"
   * 
   * @return
   */
  CitedResponsibleParty[] getCitedResponsibleParty();

  /**
   * minOccurs="0" maxOccurs="unbounded"
   * 
   * @return
   */
  PresentationFormCode[] getPresentationFormCode();

  /**
   * minOccurs="0"
   * 
   * @return
   */
  String getSeriesName();

  /**
   * minOccurs="0"
   * 
   * @return
   */
  String getIssueIdentification();

  /**
   * minOccurs="0"
   * 
   * @return
   */
  String getOtherCitationDetails();

  /**
   * minOccurs="0"
   * 
   * @return
   */
  String getCollectionTitle();

  /**
   * minOccurs="0"
   * 
   * @return
   */
  String getPage();

  /**
   * minOccurs="0"
   * 
   * @return
   */
  String getIsbn();

  /**
   * minOccurs="0"
   * 
   * @return
   */
  String getIssn();

}