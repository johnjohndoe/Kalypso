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

package org.deegree_impl.services.wcas.capabilities;

import java.util.ArrayList;

import org.deegree.services.wcas.capabilities.Operation;
import org.deegree.services.wcas.capabilities.RecordType;
import org.deegree.services.wcas.capabilities.RecordTypeList;

/**
 * This section defines the list of record types (and operations on each record
 * type) that are available from a web record server. Additional information,
 * such as SRS, about each record type is also provided.
 * <p>
 * The main purpose of the <RecordTypeList>section is to define the list of
 * record types that a WFS can service and define the operations that are
 * supported on each record type.
 * 
 * <p>
 * -----------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:uzs6tr@uni-bonn.de">Axel Schaefer </a>
 * @version $Revision$ $Date$
 */

public class RecordTypeList_Impl implements RecordTypeList
{

  private ArrayList operation = null;

  private ArrayList recordType = null;

  /**
   * constructor initializing the class with the <RecordTypeList>
   */
  RecordTypeList_Impl( Operation[] operation, RecordType[] recordType )
  {
    this.operation = new ArrayList();
    this.recordType = new ArrayList();
    setOperation( operation );
    setRecordType( recordType );
  }

  /**
   * <ul>
   * <li>INSERT: The <InsertRecord>element is used to indicate that the WFS is
   * capable of creating new instances of a record type.
   * <li>UPDATE: The <UpdateRecord>element indicates that the WFS can change
   * the existing state of a record. <li DELETE: The <DeleteRecord>element
   * indicates that the WFS can delete or remove instances of a record type from
   * the datastore.
   * <li>QUERY: The <QueryRecord>element indicates that the WFS is capable of
   * executing a query on a record type.
   * <li>LOCK: The <LockRecord>element indicates that the WFS is capable of
   * locking instances of a record type.
   * </ul>
   * <p>
   * Operations can be defined globally for all record types or locally for each
   * specific record type. Local <Operations>specifications take precedence over
   * global <Operations>specifications. If no operations are defined anywhere,
   * then the default operation list will include <Query>only.
   */
  public Operation[] getOperations()
  {
    return (Operation[])operation.toArray( new Operation[operation.size()] );
  }

  /**
   * adds the operation
   */
  public void addOperations( String operation )
  {
    this.operation.add( operation );
  }

  /**
   * sets the operation
   */
  public void setOperation( Operation[] operation )
  {
    this.operation.clear();
    for( int i = 0; i < operation.length; i++ )
    {
      this.operation.add( operation[i] );
    }
  }

  /**
   * returns the list of the RecordType.
   */
  public RecordType[] getRecordTypes()
  {
    return (RecordType[])recordType.toArray( new RecordType[recordType.size()] );
  }

  /**
   * adds the recordType
   */
  public void addRecordType( RecordType recordType )
  {
    this.recordType.add( recordType );
  }

  /**
   * sets the recordType
   */
  public void setRecordType( RecordType[] recordType )
  {
    this.recordType.clear();
    for( int i = 0; i < recordType.length; i++ )
    {
      this.recordType.add( recordType[i] );
    }
  }

}
/*
 * Changes to this class. What the people haven been up to:
 * 
 * $Log$
 * Revision 1.3  2004/10/07 14:09:04  doemming
 * *** empty log message ***
 *
 * Revision 1.1  2004/09/02 23:57:01  doemming
 * *** empty log message ***
 * Revision 1.3 2004/08/31 12:53:32 doemming
 * *** empty log message *** Revision 1.1.1.1 2002/09/25 16:01:29 poth no
 * message
 * 
 * Revision 1.1 2002/08/19 15:58:38 ap no message
 *  
 */
