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

/**
 * 
 * 
 * <p>
 * -----------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:uzs6tr@uni-bonn.de">Axel Schaefer </a>
 * @version $Revision$ $Date$
 */
public interface Request
{
  /**
   * The &lt;GetCapabilities&gt; element is included to define the available
   * distributed computing platforms for this interface.
   */
  public GetCapabilities getGetCapabilities();

  /**
   * The &lt;DescribeFeatureType&gt; tag isused toindicate what schema
   * description languages can be used to describe the schema of a feature type
   * when a client requests such a description. XMLSCHEMA is the only mandatory
   * language that must be available. The.SCHEMALANGUAGES entity can be
   * redefined to include vendor specific languages.
   */
  public DescribeFeatureType getDescribeFeatureType();

  /**
   * The &lt;Transaction&gt; element is included to define the available
   * distributed computing platforms for this interface.
   */
  public Transaction getTransaction();

  /**
   * The &lt;GetFeature&gt; tag isused todefine the formats available for
   * expressing the results of a query. The RESULTFORMATS entity defines the
   * mandatory output format of GML but can be redefined to include additional
   * vendor specific formats.
   */
  public GetFeature getGetFeature();

  /**
   * Get The Feature with Lock. It is the same as GetFeature(), with a Lock!
   */
  public GetFeatureWithLock getGetFeatureWithLock();

  /**
   * The &lt;LockFeature&gt; element is included to define the available
   * distributed computing platforms.
   */
  public LockFeature getLockFeature();
}

/*
 * Changes to this class. What the people haven been up to:
 * 
 * $Log$
 * Revision 1.2  2004/08/30 00:36:52  doemming
 * *** empty log message ***
 * Revision 1.1.1.1 2004/05/11 16:43:22 doemming backup
 * of local modified deegree sources
 * 
 * Revision 1.2 2003/06/10 07:52:04 poth no message
 * 
 * Revision 1.1.1.1 2002/09/25 16:01:52 poth no message
 * 
 * Revision 1.5 2002/08/15 10:02:41 ap no message
 * 
 * Revision 1.4 2002/04/26 09:02:34 ap no message
 * 
 * Revision 1.2 2002/04/25 16:16:36 ap no message
 * 
 * Revision 1.1 2002/04/04 16:17:15 ap no message
 *  
 */
