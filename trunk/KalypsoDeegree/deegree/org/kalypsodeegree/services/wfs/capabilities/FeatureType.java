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
package org.deegree.services.wfs.capabilities;

import java.net.URL;

import org.deegree.model.geometry.GM_Envelope;
import org.deegree.services.capabilities.MetadataURL;

/**
 * The main purpose of the &lt;FeatureTypeList&gt; section is to define the list
 * of feature types that a WFS can service and define the operations that are
 * supported on each feature type. For possible operations see the Operations
 * interface.
 * 
 * <p>
 * -----------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:uzs6tr@uni-bonn.de">Axel Schaefer </a>
 * @version $Revision$ $Date$
 */
public interface FeatureType
{
  /**
   * The name of the feature type. This element is mandatory.
   */
  public String getName();

  /**
   * The &lt;Title&gt; is a human-readable title to briefly identify this
   * feature type in menus.
   */
  public String getTitle();

  /**
   * The &lt;Abstract&gt; is a descriptive narrative for more information about
   * the feature type.
   */
  public String getAbstract();

  /**
   * The &lt;Keyword&gt; element delimits short words to aid catalog searching.
   */
  public String[] getKeywords();

  /**
   * The &lt;SRS&gt; element is used to indicate which spatial reference system
   * should be used to express the state of a feature.
   */
  public String getSrs();

  /**
   * The &lt;Operations&gt; element defines which are operations are supported
   * on a feature type. Any locally defined operations take precedence over any
   * globally defined operations.
   */
  public Operation[] getOperations();

  /**
   * The LatLonBoundingBox attributes indicate the edges of the enclosing
   * rectangle in latitude/longitude decimal degrees (as in SRS EPSG:4326
   * [WGS1984 lat/lon]). Its purpose is to facilitate geographic searches
   * without requiring coordinate transformations by the search engine.
   */
  public GM_Envelope getLatLonBoundingBox();

  /**
   * A WFS may use zero or more &lt;MetadataURL&gt; elements to offer detailed,
   * standardized metadata about the data inaparticularfeature type.The type
   * attribute indicates the standard to which the metadata complies; the format
   * attribute indicates how the metadata is structured. Two types are defined
   * at present: ’TC211’ = ISO TC211 19115; FGDC = FGDC CSDGM.
   */
  public MetadataURL[] getMetadataURL();

  /**
   * returns an instance of the feature type list the feature belongs to.
   */
  public FeatureTypeList getParentList();

  /**
   * returns the name of the class that's responsible for performing the
   * operation.
   */
  String getResponsibleClassName();

  /**
   * returns the name of the configuration file of the responsible class
   */
  public URL getConfigURL();

  /* #FeatureTypeList lnkWFS_FeatureTypeList_Cap; */
}

/*
 * Changes to this class. What the people haven been up to:
 * 
 * $Log$
 * Revision 1.3  2004/10/07 14:09:07  doemming
 * *** empty log message ***
 *
 * Revision 1.1  2004/09/02 23:57:12  doemming
 * *** empty log message ***
 * Revision 1.3 2004/08/31 12:45:01 doemming ***
 * empty log message *** Revision 1.2 2003/06/10 07:52:03 poth no message
 * 
 * Revision 1.1.1.1 2002/09/25 16:01:52 poth no message
 * 
 * Revision 1.9 2002/08/15 10:02:41 ap no message
 * 
 * Revision 1.8 2002/07/04 14:52:18 ap no message
 * 
 * Revision 1.7 2002/05/24 07:02:42 ap no message
 * 
 * Revision 1.6 2002/05/06 16:01:41 ap no message
 * 
 * Revision 1.5 2002/04/26 09:02:34 ap no message
 * 
 * Revision 1.3 2002/04/25 16:16:36 ap no message
 * 
 * Revision 1.2 2002/04/16 16:15:57 ap no message
 * 
 * Revision 1.1 2002/04/04 16:17:15 ap no message
 *  
 */
