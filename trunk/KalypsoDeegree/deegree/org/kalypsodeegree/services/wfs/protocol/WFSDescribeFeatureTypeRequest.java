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
package org.deegree.services.wfs.protocol;

/**
 * The function of the DescribeFeatureType interface is to provide a client the
 * means to request a schema definition of any feature type that a particular
 * WFS can service. The description that is generated will define how a WFS
 * expects a client application to express the state of a feature to be created
 * or the new state of a feature to be updated. The result of a
 * DescribeFeatureType request is an XML document, describing one or more
 * feature types serviced by the WFS.
 * 
 * <p>
 * --------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public interface WFSDescribeFeatureTypeRequest extends WFSBasicRequest
{
  /**
   * The outputFormat attribute, is used to indicate the schema description
   * language that should be used to describe a feature schema. The only
   * mandated format is XML-Schema denoted by the XMLSCHEMA element; other
   * vendor specific formats specified in the capabilities document are also
   * possible.
   */
  public String getOutputFormat();

  /**
   * If a filter is not specified, then the optional typeName attribute can be
   * used to specify that all feature instances of a particular feature type
   * should be locked.
   */
  public String[] getTypeNames();

  /**
   * returns the field names of each feature that shall be described. if all
   * fields shall be described (default) <tt>null</tt> will be returned.
   * <p>
   * </p>
   * notice: the OGC WFS 1.0.0 isn't clear at this point. at chapter 8 that
   * describes the DescribeFeatureType operation it isn't specified to define
   * properties names within the request. At chapter 9 that describes the
   * GetFeature operation it is possible to select a subset of properties of a
   * feature type, that may not be vaild against the schema that describes the
   * full feature type.
   */
  public String[] getProperties( String featureType );
}

/*
 * Changes to this class. What the people haven been up to:
 * 
 * $Log$
 * Revision 1.3  2004/10/07 14:09:06  doemming
 * *** empty log message ***
 *
 * Revision 1.1  2004/09/02 23:56:55  doemming
 * *** empty log message ***
 * Revision 1.3 2004/08/31 12:45:01
 * doemming *** empty log message *** Revision 1.3 2004/02/09 07:57:02 poth no
 * message
 * 
 * Revision 1.2 2003/04/23 07:23:13 poth no message
 * 
 * Revision 1.1.1.1 2002/09/25 16:01:54 poth no message
 * 
 * Revision 1.5 2002/08/15 10:02:41 ap no message
 * 
 * Revision 1.4 2002/08/05 16:13:11 ap no message
 * 
 * Revision 1.3 2002/04/26 09:02:51 ap no message
 * 
 * Revision 1.1 2002/04/04 16:17:15 ap no message
 *  
 */
