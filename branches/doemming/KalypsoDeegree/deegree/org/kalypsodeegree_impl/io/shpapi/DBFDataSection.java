
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

package org.deegree_impl.io.shpapi;


import java.util.ArrayList;



/**
 * Class representing a record of the data section of a dBase III/IV file<BR>
 * at the moment only the daata types character ("C") and numeric ("N")
 * are supported
 * 
 * <P>
 * <B>Last changes<B>:<BR>
 * 28.04.00 ap: constructor declared and implemented<BR>
 * 28.04.00 ap: method setRecord(ArrayList recData) declared and implemented<BR>
 * 28.04.00 ap: method getDataSection() declared and implemented<BR>
 * 03.05.00 ap: method setRecord(ArrayList recData) modified<BR>
 * 03.05.00 ap: method setRecord(int index, ArrayList recData) declared and implemented<BR>
 * 03.05.00 ap: method getDataSection() modified<BR>
 *
 *
 * <!---------------------------------------------------------------------------->
 * @version 03.05.2000
 * @author Andreas Poth
 */

public class DBFDataSection {    

    // length of one record in bytes
    private int recordlength = 0;

    private FieldDescriptor[] fieldDesc = null;

    private ArrayList data = new ArrayList();

   /**
    * constructor
    */
    public DBFDataSection(FieldDescriptor[] fieldDesc) {

        this.fieldDesc = fieldDesc;
        
        // calculate length of the data section
        recordlength = 0;
        for (int i = 0; i < this.fieldDesc.length; i++) {            

            byte[] fddata = this.fieldDesc[i].getFieldDescriptor();

            recordlength += fddata[16];

            fddata = null;

        }

        recordlength++;

    }

   /**
    * method: public setRecord(ArrayList recData)
    * writes a data record to byte array representing the data
    * section of the dBase file. The method gets the data type
    * of each field in recData from fieldDesc wich has been
    * set at the constructor. 
    */
    public void setRecord(ArrayList recData) throws DBaseException {

        setRecord(data.size(),recData);

    }

   /**
    * method: public setRecord(int index, ArrayList recData)
    * writes a data record to byte array representing the data
    * section of the dBase file. The method gets the data type
    * of each field in recData from fieldDesc wich has been
    * set at the constructor. index specifies the location
    * of the retrieved record in the datasection. if an invalid
    * index is used an exception will be thrown
    */
    public void setRecord(int index, ArrayList recData) throws DBaseException {

        ByteContainer datasec = new ByteContainer(recordlength);

        if ( (index < 0) || (index > data.size()) )
            throw new DBaseException("invalid index: "+index);        

        if (recData.size() != this.fieldDesc.length) 
            throw new DBaseException("invalid size of recData");        

        int offset = 0;

        datasec.data[offset] = 0x20;

        offset++;

        byte[] b = null;

        // write every field on the ArrayList to the data byte array
        for (int i = 0; i < recData.size(); i++) {

            byte[] fddata = this.fieldDesc[i].getFieldDescriptor();

            switch (fddata[11]) {

                // if data type is character
                case (byte)'C': if (!(recData.get(i) instanceof String))
                                    throw new DBaseException("invalid data type at field: "+i);
                                b = ((String)recData.get(i)).getBytes();
                                if (b.length > fddata[16]) 
                                    throw new DBaseException("string contains too many characters "+
                                                        (String)recData.get(i));
                                for (int j = 0; j < b.length; j++) datasec.data[offset+j] = b[j];
                                for (int j = b.length; j < fddata[16]; j++) datasec.data[offset+j] = 0x20;
                                break;
                case (byte)'N': if (!(recData.get(i) instanceof Double))
                                    throw new DBaseException("invalid data type at field: "+i);
                                b = ((Double)recData.get(i)).toString().getBytes();
                                if (b.length > fddata[16]) 
                                    throw new DBaseException("string contains too many characters "+
                                                        (String)recData.get(i));
                                for (int j = 0; j < b.length; j++) datasec.data[offset+j] = b[j];        
                                for (int j = b.length; j < fddata[16]; j++) datasec.data[offset+j] = 0x0;
                                break;
                default: throw new DBaseException("data type not supported");

            }

            offset += fddata[16];

        }

        // puts the record to the ArrayList (container)
        data.add( index, datasec );

    }

   /**
    * method: public byte[] getDataSection()
    * returns the data section as a byte array. 
    */
    public byte[] getDataSection() {

        // allocate memory for all datarecords on one array + 1 byte
        byte[] outdata = new byte[data.size()*recordlength+1];

        // set the file terminating byte
        outdata[outdata.length-1] = 0x1A;

        // get all records from the ArrayList and put it 
        // on a single array
        int j = 0;
        for (int i = 0; i < data.size(); i++) {
 
            ByteContainer bc = (ByteContainer) data.get(i);

            for (int k = 0; k < recordlength; k++) {
                outdata[j++] = bc.data[k];
            }

        }

        return outdata;

    }

   /**
    * method: public int getNoOfRecords()
    * returns the number of records within the container
    */
    public int getNoOfRecords() {

        return data.size();

    }


}


class  ByteContainer {

    public byte[] data = null;

    public ByteContainer(int size) {

        data = new byte[size];

    }

}
