// $Header:
// /cvsroot/deegree/deegree/org/deegree_impl/services/gazetteer/configuration/Relation.java,v
// 1.1 2004/03/24 12:36:22 poth Exp $
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
/*
 * Relation.java
 * 
 * Created on 24. März 2004, 10:45
 */
package org.deegree_impl.services.gazetteer.configuration;

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
public class Relation
{
  private String operation = null;

  private String sourceLocationType = null;

  private String sourceProperty = null;

  private String targetLocationType = null;

  private String targetProperty = null;

  /** Creates a new instance of Relation */
  public Relation( String sourceLocationType, String sourceProperty, String targetLocationType,
      String targetProperty, String operation )
  {
    this.sourceLocationType = sourceLocationType;
    this.sourceProperty = sourceProperty;
    this.targetLocationType = targetLocationType;
    this.targetProperty = targetProperty;
    this.operation = operation;
  }

  /**
   * returns the name of the source location type of the relation
   * 
   * @return
   */
  public String getSourceLocationType()
  {
    return sourceLocationType;
  }

  /**
   * returns the name of the property of source location type that establishes
   * the relation
   * 
   * @return
   */
  public String getSourceProperty()
  {
    return sourceProperty;
  }

  /**
   * returns the name of the target location type of the relation
   * 
   * @return
   */
  public String getTargetLocationType()
  {
    return targetLocationType;
  }

  /**
   * returns the name of the property of target location type that establishes
   * the relation
   * 
   * @return
   */
  public String getTargetProperty()
  {
    return targetProperty;
  }

  /**
   * return the name of the operation that shall be performed between the source
   * and target property.
   * 
   * @return
   */
  public String getOperation()
  {
    return operation;
  }
}
/*******************************************************************************
 * Changes to this class. What the people have been up to: $Log$
 * Changes to this class. What the people have been up to: Revision 1.3  2004/10/07 14:09:23  doemming
 * Changes to this class. What the people have been up to: *** empty log message ***
 * Changes to this class. What the people have been up to:
 * Changes to this class. What the people have been up to: Revision 1.1  2004/09/02 23:57:14  doemming
 * Changes to this class. What the people have been up to: *** empty log message ***
 * Changes to this class. What the people have been up to:
 * Changes to this class. What the people have been up to: Revision 1.2
 * 2004/08/31 12:53:32 doemming Changes to this class. What the people have been
 * up to: *** empty log message *** Changes to this class. What the people have
 * been up to: Revision 1.1 2004/03/24 12:36:22 poth no message
 * 
 * 
 *  
 ******************************************************************************/