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
package org.deegree_impl.services.wcas.protocol;

import java.util.ArrayList;
import java.util.HashMap;

import org.deegree.services.wcas.protocol.CASDescribeRecordTypeRequest;
import org.deegree_impl.services.OGCWebServiceRequest_Impl;


/**
 * The describe record type operation describes the architecture of a
 * possible result to a getRecord request as a XML schema. At the moment two
 * basic schemas are known: ISO19115 (by NIMA and GDI NRW) and ISO19119
 * (by OWS1.2 and GDI NRW). The basic schemas are splitted into three
 * subschemas (Full, Summary and Brief) called setNames.
 * <p>--------------------------------------------------------------------</p>
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version 2002-04-16
 */
final public class CASDescribeRecordTypeRequest_Impl extends OGCWebServiceRequest_Impl
    implements CASDescribeRecordTypeRequest {
    private ArrayList setNames = null;
    private ArrayList typeNames = null;
    private String outputFormat = null;

    /**
     * Creates a new CASDescribeRecordTypeRequest_Impl object.
     *
     * @param version 
     * @param id 
     * @param vendorSpecificParameter 
     * @param typeNames 
     * @param setNames 
     * @param outputFormat 
     */
    CASDescribeRecordTypeRequest_Impl(String version, String id, HashMap vendorSpecificParameter, 
                                      String[] typeNames, String[] setNames, String outputFormat) {
        super("DescribeRecordType", "WCAS", version, id, vendorSpecificParameter);
        this.typeNames = new ArrayList();
        this.setNames = new ArrayList();
        setTypeNames(typeNames);
        setSetNames(setNames);
        setOutputFormat(outputFormat);
    }

    /**
     * returns the (catalog) types that shall be described. three catalog
     * types are known: Service, Product and Collection
     */
    public String[] getTypeNames() {
        return (String[]) typeNames.toArray(new String[typeNames.size()]);
    }

    /**
     * @see CASDescribeRecordTypeRequest_Impl#getTypeNames()
     */
    public void setTypeNames(String[] typeNames) {
        this.typeNames.clear();

        if (typeNames != null) {
            for (int i = 0; i < typeNames.length; i++) {
                addTypeName(typeNames[i]);
            }
        }
    }

    /**
     * @see CASDescribeRecordTypeRequest_Impl#getTypeNames()
     */
    public void addTypeName(String typeName) {
        typeNames.add(typeName);
    }

    /**
     * returns the set names of the record types that shall be described.
     * each set name is associated with the type name returned by
     * <tt>getTypeNames</tt> at same index position.
     */
    public String[] getSetNames() {
        return (String[]) setNames.toArray(new String[setNames.size()]);
    }

    /**
     * @see CASDescribeRecordTypeRequest_Impl#getSetNames()
     */
    public void setSetNames(String[] setNames) {
        this.setNames.clear();

        if (setNames != null) {
            for (int i = 0; i < setNames.length; i++) {
                addSetName(setNames[i]);
            }
        }
    }

    /**
     * @see CASDescribeRecordTypeRequest_Impl#getSetNames()
     */
    public void addSetName(String setName) {
        setNames.add(setName);
    }

    /**
     * returns the format the result to the request will e returned. At the
     * moment only XML-schema is defined as valid output format.
     */
    public String getOutputFormat() {
        return outputFormat;
    }

    /**
     * @see CASDescribeRecordTypeRequest_Impl#getOutputFormat()
     */
    public void setOutputFormat(String outputFormat) {
        this.outputFormat = outputFormat;
    }

    /**
     *
     *
     * @return 
     */
    public String toString() {
        String ret = null;
        ret = "typeNames = " + typeNames + "\n";
        ret += ("setNames = " + setNames + "\n");
        ret += ("outputFormat = " + outputFormat + "\n");
        return ret;
    }
}