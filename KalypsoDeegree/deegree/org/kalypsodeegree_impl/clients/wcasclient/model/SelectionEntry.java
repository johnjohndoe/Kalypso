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
package org.deegree_impl.clients.wcasclient.model;

import org.deegree.model.geometry.GM_Envelope;

/**
 * represents an entry of a users selectin of catalog entries. An entry is
 * described by its ID, title, the catalog it is completly described at and the
 * bounding box that bases the resaerch where the entry was found.
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
public class SelectionEntry
{

  public static int UNKNOWN = -1;

  public static int VIEW = 0;

  public static int DOWNLOAD = 1;

  private GM_Envelope bbox = null;

  private String catalog = null;

  private String id = null;

  private String title = null;

  private boolean selected = false;

  private int[] availability = null;

  /**
   * Creates a new SelectionEntry object. For default the entry is set to 'not
   * selected'
   * 
   * @param id
   *          unique identifier of a catalog entry
   * @param title
   *          title of the catalog entry
   * @param catalog
   *          name of the catalog the entry belongs to
   * @param bbox
   *          bbox that bases the request the entry was find at
   */
  public SelectionEntry( String id, String title, String catalog, GM_Envelope bbox,
      int[] availability )
  {
    this.id = id;
    this.title = title;
    this.catalog = catalog;
    this.bbox = bbox;
    this.availability = availability;
  }

  /**
   * returns the id of the catalog/selection entry
   * 
   * @return id of the catalog/selection entry
   */
  public String getId()
  {
    return id;
  }

  /**
   * returns the title of the catalog/selection entry
   * 
   * @return title of the catalog/selection entry
   */
  public String getTitle()
  {
    return title;
  }

  /**
   * returns the name of the catalog the catalog/selection entry belongs to
   * 
   * @return name of the catalog the catalog/selection entry belongs to
   */
  public String getCatalog()
  {
    return catalog;
  }

  /**
   * returns the bounding box that bases the request the entry was find at
   * 
   * @return bounding box that bases the request the entry was find at
   */
  public GM_Envelope getBoundingBox()
  {
    return bbox;
  }

  /**
   * returns true if the entry is marked as selected
   */
  public boolean isSelected()
  {
    return selected;
  }

  /**
   * sets the entry to 'selected' or not.
   */
  public void setSelected( boolean selected )
  {
    this.selected = selected;
  }

  /**
   * returns a list of service types that offer the dataset assigned to this
   * entry
   */
  public int[] getAvailability()
  {
    return availability;
  }

  /**
   * set a list of service types that offer the dataset assigned to this entry
   */
  public void setAvailability( int[] availability )
  {
    this.availability = availability;
  }

}