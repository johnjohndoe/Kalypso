
package org.kalypso.services.calculation.service.impl.jaxws;

import javax.xml.bind.annotation.AccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import org.kalypso.services.calculation.service.impl.jaxws.GetJobsResponse;

@XmlRootElement(name = "getJobsResponse", namespace = "http://impl.service.calculation.services.kalypso.org/")
@XmlAccessorType(AccessType.FIELD)
@XmlType(name = "getJobsResponse", namespace = "http://impl.service.calculation.services.kalypso.org/")
public class GetJobsResponse {

    @XmlElement(name = "return", namespace = "")
    private org.kalypso.services.calculation.service.CalcJobInfoBean[] _return;

    /**
     * 
     * @return
     *     returns CalcJobInfoBean[]
     */
    public org.kalypso.services.calculation.service.CalcJobInfoBean[] get_return() {
        return this._return;
    }

    /**
     * 
     * @param _return
     *     the value for the _return property
     */
    public void set_return(org.kalypso.services.calculation.service.CalcJobInfoBean[] _return) {
        this._return = _return;
    }

}
