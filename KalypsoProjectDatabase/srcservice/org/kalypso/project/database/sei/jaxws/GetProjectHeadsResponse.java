
package org.kalypso.project.database.sei.jaxws;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

@XmlRootElement(name = "getProjectHeadsResponse", namespace = "http://sei.database.project.kalypso.org/")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "getProjectHeadsResponse", namespace = "http://sei.database.project.kalypso.org/")
public class GetProjectHeadsResponse {

    @XmlElement(name = "return", namespace = "", nillable = true)
    private org.kalypso.project.database.sei.beans.KalypsoProjectBean[] _return;

    /**
     * 
     * @return
     *     returns KalypsoProjectBean[]
     */
    public org.kalypso.project.database.sei.beans.KalypsoProjectBean[] getReturn() {
        return this._return;
    }

    /**
     * 
     * @param _return
     *     the value for the _return property
     */
    public void setReturn(org.kalypso.project.database.sei.beans.KalypsoProjectBean[] _return) {
        this._return = _return;
    }

}
