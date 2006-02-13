
package org.kalypso.services.calculation.service.impl.jaxws;

import javax.xml.bind.annotation.AccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import org.kalypso.services.calculation.service.impl.jaxws.GetSchemaValidityResponse;

@XmlRootElement(name = "getSchemaValidityResponse", namespace = "http://impl.service.calculation.services.kalypso.org/")
@XmlAccessorType(AccessType.FIELD)
@XmlType(name = "getSchemaValidityResponse", namespace = "http://impl.service.calculation.services.kalypso.org/")
public class GetSchemaValidityResponse {

    @XmlElement(name = "return", namespace = "")
    private long _return;

    /**
     * 
     * @return
     *     returns long
     */
    public long get_return() {
        return this._return;
    }

    /**
     * 
     * @param _return
     *     the value for the _return property
     */
    public void set_return(long _return) {
        this._return = _return;
    }

}
