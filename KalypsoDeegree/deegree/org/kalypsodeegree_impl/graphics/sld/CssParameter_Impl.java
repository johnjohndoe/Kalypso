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
package org.deegree_impl.graphics.sld;

import org.deegree.graphics.sld.CssParameter;
import org.deegree.graphics.sld.ParameterValueType;
import org.deegree.model.feature.Feature;
import org.deegree.services.wfs.filterencoding.FilterEvaluationException;
import org.deegree.xml.Marshallable;
import org.deegree_impl.tools.Debug;


/**
 * The simple SVG/CSS2 styling parameters are given with the CssParameter
 * element, which is defined as follows:
 * <pre> 
 *  <xs:element name="CssParameter" type="sld:ParameterValueType"/>
 *     <xs:complexType name="ParameterValueType" mixed="true">
 *        <xs:choice minOccurs="0" maxOccurs="unbounded">
 *             <xs:element ref="wfs:expression"/>
 *         </xs:choice>
 *  </xs:complexType>
 * </pre>
 * The parameter values are allowed to be complex expressions for maximum
 * flexibility. The mixed="true" definition means that regular text may be mixed
 * in with various sub-expressions, implying a text-substitution model for
 * parameter values. Numeric and character-string data types are not
 * distinguished, which may cause some complications.<p></p>
 * Here are some usage examples:
 * <pre>
 * 1. <CssParameter name="stroke-width">3</CssParameter>
 * 2. <CssParameter name="stroke-width">
 *         <wfs:Literal>3</wfs:Literal>
 *    </CssParameter>
 * 3. <CssParameter name="stroke-width">
 *         <wfs:Add>
 *             <wfs:PropertyName>/A</wfs:PropertyName>
 *             <wfs:Literal>2</wfs:Literal>
 *         </wfs:Add>
 *    </CssParameter>
 * 4. <Label>This is city "<wfs:PropertyName>/NAME</wfs:PropertyName>"
 * of state <wfs:PropertyName>/STATE</wfs:PropertyName></Label>
 * </pre>
 * The allowed SVG/CSS styling parameters for a stroke are: stroke (color),
 * stroke-opacity, stroke-width, stroke-linejoin, stroke-linecap,
 * stroke-dasharray, and stroke-dashoffset. The chosen parameter is given by the
 * name attribute of the CssParameter element.
 * <p>
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider</a>
 * @version $Revision$ $Date$
 */
class CssParameter_Impl implements CssParameter, Marshallable {
    private ParameterValueType pvt = null;
    private String name = null;

    /**
     * constructor initializing the class with the <CssParameter>
     */
    CssParameter_Impl( String name, ParameterValueType pvt ) {
        this.name = name;
        this.pvt = pvt;
    }

    /**
     * Returns the name attribute's value of the CssParameter.
     * <p>
     * @return the value of the name attribute of the CssParameter
     */
    public String getName() {
        return name;
    }

   /**
    * Sets the name attribute's value of the CssParameter.
    * <p>
    * @param name the value of the name attribute of the CssParameter
    */
    public void setName (String name) {
        this.name = name;
    }
    
    /**
     * Returns the value of the CssParameter as an <tt>ParameterValueType</tt>.
     * <p>
     * @return the mixed content of the element
     */
    public ParameterValueType getValue( ) {
        return pvt;
    }
    
    /**
     * Sets the value of the CssParameter as an <tt>ParameterValueType</tt>.
     * <p>
     * @param value the mixed content of the element
     */
    public void setValue (ParameterValueType value) {
        this.pvt = value;
    }   

    /**
     * Returns the (evaluated) value of the CssParameter as a simple
     * <tt>String</tt>.
     * <p>
     * @param feature specifies the <tt>Feature</tt> to be used for evaluation
     *        of the underlying 'sld:ParameterValueType'
     * @return the (evaluated) <tt>String</tt> value of the parameter
     * @throws FilterEvaluationException if the evaluations fails
     */
    public String getValue( Feature feature ) throws FilterEvaluationException {
        return pvt.evaluate( feature );
    }
    
    /**
     * Sets the value of the CssParameter as a simple
     * <tt>String</tt>.
     * <p>
     * @param value CssParameter-Value to be set 
     */
    public void setValue (String value) {
        ParameterValueType pvt = null;
        pvt = StyleFactory.createParameterValueType( "" + value );
        this.pvt = pvt;
    }
    
    /**
     * exports the content of the CssParameter as XML formated String
     *
     * @return xml representation of the CssParameter
     */
    public String exportAsXML() {
        Debug.debugMethodBegin();
        
        StringBuffer sb = new StringBuffer("<CssParameter name=");
        sb.append( "'" + name + "'>" );
        sb.append( ((Marshallable)pvt).exportAsXML() );
        sb.append( "</CssParameter>" );
        
        Debug.debugMethodEnd();
        return sb.toString();
    }    
}