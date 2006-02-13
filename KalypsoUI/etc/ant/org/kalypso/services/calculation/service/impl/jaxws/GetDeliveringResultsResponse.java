
package org.kalypso.services.calculation.service.impl.jaxws;

import javax.xml.bind.annotation.AccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import org.kalypso.services.calculation.service.impl.jaxws.GetDeliveringResultsResponse;

@XmlRootElement(name = "getDeliveringResultsResponse", namespace = "http://impl.service.calculation.services.kalypso.org/")
@XmlAccessorType(AccessType.FIELD)
@XmlType(name = "getDeliveringResultsResponse", namespace = "http://impl.service.calculation.services.kalypso.org/")
public class GetDeliveringResultsResponse {

    @XmlElement(name = "return", namespace = "")
    private org.kalypso.services.calculation.service.CalcJobServerBean[] _return;

    /**
     * 
     * @return
     *     returns CalcJobServerBean[]
     */
    public org.kalypso.services.calculation.service.CalcJobServerBean[] get_return() {
        return this._return;
    }

    /**
     * 
     * @param _return
     *     the value for the _return property
     */
    public void set_return(org.kalypso.services.calculation.service.CalcJobServerBean[] _return) {
        this._return = _return;
    }

}
